(in-package :event)
;;;; Event facility
;;;; While entities have primitive mutators and action-performers, this facility arranges them in time.
;;;; TODO: Separate it from AI facility

;;;Main

(defclass event()
  ((energy :initform 0 :type fixnum :accessor event-energy :initarg :energy)))

(defvar *next-list* '())
(defvar *zero-energy* 0)

(defmacro in-event-context(form)
  (alexandria:with-gensyms (blck retry)
    `(block ,blck
       (tagbody ,retry
	  (restart-case
	      (return-from ,blck ,form)
	    (retry-event ()
	      :report "Retry last event"
	      (go ,retry))
	    (abandon ()
	      :report "Forget last event"
	      nil))))))

(defun process-events(&aux (event-list *next-list*))
  (psetf *next-list* nil)
  (iterate (while event-list) 
	   (let [chosen-event (pop (if (and *next-list* (< (event-energy (car *next-list*)) (event-energy (car event-list))))
				       *next-list*
				       event-list))
		 *zero-energy* (event-energy chosen-event)
		 new-list (in-event-context (aif (exec chosen-event)
						 (add-before (let ((e (event-energy it)))
							       (amutf (event-energy it) (+ e *zero-energy*)))
							     #'< *next-list* :key #'event-energy :before-last t)
						 *next-list*))]
	     (setf *next-list* new-list)))
  (setf *next-list* (when *next-list* (mapcar (let [floor (event-energy (car *next-list*))]
						(lambda (event) (amutf (-> event energy) (- it floor))))
					      *next-list*))))

(defun add-event(ev)
  (psetf (event-energy ev) (+ (event-energy ev) *zero-energy*)
	 *next-list* (add-before ev #'< *next-list* :key #'event-energy :before-last t)))

;;;Classes

(defclass turn(event)
  ((actor :type enitity :reader entity :initarg :entity)))

(defclass map-entity-turn(turn)
  ((pos :type pos :reader get-pos :initarg :pos)))

(defclass thunk-event(event)
  ((thunk :type (function () t) :initarg :thunk)))

(defclass ai-update(turn)
  ())

(defmethod make-update((entity entity) pos eu)
  (make-instance 'map-entity-turn :entity entity :pos pos :energy (/ eu (get-speed entity))))

(defmethod make-turn((entity creature) eu)
  (assert entity)
  (make-instance 'turn :entity entity :energy (/ eu (get-speed entity))))

(defun make-thunk-event(thunk eu)
  (make-instance 'thunk-event :thunk thunk :energy eu))

(defmethod exec((turn turn))
  (take-turn (entity turn)))

(defmethod exec((turn map-entity-turn))
  (update-entity (entity turn) (get-pos turn)))

(defmethod exec((event thunk-event))
  (funcall (-> event thunk)))

;;;Generics

(defgeneric try-to-move!(entity direction)) ;WARNING! It is subpredicate. Invalid call can freeze creature!
(defgeneric try-to-close!(entity direction))
(defgeneric melee-attack!(entity direction))
(defgeneric interact-with-cell!(entity direction))
(defgeneric reload-weapon!(entity))
(defgeneric perform-shot!(entity cell &optional fire-mode))
;;;Start

(defmethod take-turn :around ((creature creature))
  (when (alivep creature)
    (call-next-method)))

(defmethod update-entity((entity trap) pos)
  (progn
    (setf (activep entity) t)
    (update-trap entity pos)
    (if (activep entity)
	nil
	(make-update entity pos 100))))

(defun update-trap(trap pos)
  (aif (and (activep trap) (level:get-entities pos 'creature))
       (trap-activate trap pos)
       nil))

;;;Interaction

(defun choose-items(item-list)
  (alet ((chosen nil)
	 (item-list item-list))
    (if (null item-list)
	chosen
	(aif (panels:item-choose item-list)
	     (self (cons it chosen)
		   (remove it item-list :count 1))
	     chosen))))

(defmethod pickup-items!((creature actor))
  (aif (level:get-entities (get-pos creature) 'item-stack)
       (aif (choose-items it)
	    (progn (dolist (stack it)
		     (add-stack (get-inventory creature) stack)
		     (level:remove-entity (get-pos creature) stack))
		   (awith (mapcar #'get-name it)
		     (simple-message creature
				     "I pick up ~a~{, ~a~}." (car it) (cdr it)))
		   (make-turn creature (* 100 (length it)))))
       (progn
	 (simple-note creature "Here is nothing to pick up.")
	 nil)))

(defmethod drop-items!((creature actor) stack)
  (awith (get-inventory creature)
	 (when (find stack (get-backpack it) :test #'eq)
	   (remove-stack it stack)
	   (level:add-entity (get-pos creature) stack)
	   (simple-message creature "I drop ~a. " (get-name stack))
	   (event:make-turn creature 100))))

(defmethod use-slot!(creature slot)
  (progn
    (when (if (slot-stack slot)
	      (put-off-slot creature (slot-category slot))
	      (slot-equip creature (slot-category slot)))
      (make-turn creature 100))))

(defmethod switch-weapon!((creature actor))
  (swap-slots (get-gear creature))
  (simple-message creature "I switch to ~a. " (get-name (get-weapon creature)))
  (make-turn creature (/ 100 (get-speed (get-weapon creature)))))

(defmethod close-near!((creature actor) fov-info &aux (buf (get-message-buffer creature)))
  (simple-note creature "Choose a direction. ")
  (panels:change-layer (panels:game-panel creature (panels:close-controller creature)
					  fov-info (ui:buffer-source buf))))

(defmethod try-to-close!((creature actor) delta &aux (pos (add delta (get-pos creature))))
  (aif (map:search-terrain pos 'map:door)
       (if (map:openp it)
	   (if (level:get-entities pos)
	       (simple-note creature "The door is blocked.")
	       (progn
		 (simple-message creature "I close a door. ")
		 (map:close-door it)
		 (make-turn creature 100)))
	   (simple-note creature "The door is already closed. "))
       (simple-note creature "There is nothing to close. ")))

(defun attack-terrain!(creature terrain-pos)
  (map:attack-cell terrain-pos (damage-count (get-damage creature))))

(defmethod interact-with-cell!((creature actor) dir &aux (cell-pos (add (get-pos creature) dir)) (door (map:search-terrain cell-pos 'map:door)))
  (acond ((remove creature (level:get-entities cell-pos 'creature) :count 1)
	  (melee-attack! creature (first it)))
	 ((and door (not (map:openp door)))
	  (progn (map:open-door door)
		 (simple-message creature "I open the door. ")
		 (make-turn creature 100)))
	 (t (try-to-move! creature dir))))

(defmethod interact-with-cell!(creature dir)
  (or (try-to-move! creature dir) (make-turn creature 100)))

(defun bash-door!(creature dir &aux (pos (add dir (get-pos creature))) (door (map:search-terrain pos 'map:door)))
  (when (and door (not (map:openp door)))
    (simulate-message (get-pos creature) (msg (formatted "~a bashes door." (get-name creature))))
    (attack-terrain! creature pos)
    (report pos
	    (msg "The door is shaken violently.")
	    (snd "I hear a door hit." 8))
    (when (map:destroyedp door)
      (report pos
	      (msg "The door crushes down!")
	      (snd "I hear a door crush." 12)))
    (make-turn creature 100)))

(defmethod interact-with-cell!(creature dir)
  (aif (try-to-move! creature dir)
       it
       (bash-door! creature dir)))

;;;Attack

(defgeneric report-attack(attacker attackee dodgedp))

(defmethod report-attack(attacker attackee dodgedp)
  (simulate-message (get-pos attackee)
		    (msg (apply #'formatted (if dodgedp
						(list "~a misses ~a." (word-capitalize (get-name attacker)) (get-concrete-name attackee))
						(list "~a hits ~a."  (word-capitalize (get-name attacker)) (get-concrete-name attackee)))))))

(defmethod report-attack((attacker trap) (attackee actor) dodgedp)
  (if dodgedp
      (simple-message attackee "I avoid ~a." (get-concrete-name attacker))
      (call-next-method)))

(defmethod report-attack((attacker zombie) (attackee actor) dodgedp)
  (if dodgedp
      (call-next-method)
      (simple-message attackee "~a claws ~a!" (word-capitalize (get-name attacker)) (get-concrete-name attackee))))

(defmethod report-attack((attacker actor) attackee dodgedp)
  (apply #'simple-message attacker (if dodgedp
				       (list "I miss ~a." (get-concrete-name attackee) (get-damage attacker))
				       (list "I attack ~a with my ~a!" (get-name attackee) (get-name (get-weapon attacker))))))


(defmethod hit(source victim)
  "Melee hit"
  (awith (dodges victim (get-damage source))
    (report-attack source victim it)
    (unless it
      (take-damage victim (get-damage source)))))

(defmethod melee-attack!((attacker creature) attackee)
  (progn
    (hit attacker attackee)
    (event:make-turn attacker 100)))

(defmethod melee-attack!((attacker actor) attackee)
  (hit attacker attackee)
  (event:make-turn attacker (/ 100 (get-speed (get-weapon attacker)))))

;;;Movement

(defmethod try-to-move!(creature (dir pos))
  (if (and (zerop (x dir)) (zerop (y dir)))
      (make-turn creature 100)
      (when (can-move-p creature dir)
	(perform-movement creature dir)
	(make-turn creature (if (or (zerop (x dir)) (zerop (y dir))) 100 141)))))

(defmethod try-to-move!((act actor) (dir pos))
  (or (call-next-method) (simple-message act "The way is blocked.")))

(defun move-random!(creature &optional (tries 8))
  (or (try-to-move! creature (make-pos (1- (random 3)) (1- (random 3))))
      (if (zerop tries) (make-turn creature 100) (move-random! creature (1- tries)))))

(defun check-traps(pos)
  (dolist (trap (level:get-entities pos 'entity:trap))
    (provoke-trap! trap pos)))

(defmethod perform-movement :after ((creature creature) delta)
  (declare (ignore delta))
  (check-traps (get-pos creature)))


;;;Item interaction

(defgeneric item-actions(stack actor))

(defmethod item-actions((stack item-stack) actor)
  (list #'drop-items!))

;;;Traps

(defmethod trap-activate((entity anomaly) pos)
  (dolist (creature (level:get-entities pos 'creature) (not (setf (activep entity) nil)))
    (hit entity creature)))

(defmethod trap-activate :before((entity trap) pos)
  (report pos
	  (msg (formatted "~a activates." (word-capitalize (get-name entity))))
	  (snd "I hear an anomaly activation." 8)))


(defun provoke-trap!(trap pos)
  (when (activep trap)
    (update-trap trap pos)
    (add-event (make-update trap pos 100))))

;;;Ranged combat

(defmethod hit-report((creature actor))
  (simple-message creature "I am hit!"))

(defmethod hit-report(creature)
  (simulate-message (get-pos creature) (msg (formatted "~a is hit." (word-capitalize (get-name creature))))))

(defun trace-shot(line damage)
  (doray (cell line)
    (acond
      ((level:get-entities cell 'creature)
       (unless (dodges (first it) damage)
	 (hit-report (first it))
	 (take-damage (car it) damage)
	 (return))))))

(defmethod perform-shot!((creature actor) cell &optional (fire-mode 1))
  (when (can-shoot-p creature)
    (awith (get-weapon creature)
      (if (= fire-mode 1)
	  (simple-message creature "I fire ~a." (get-name it))
	  (simple-message creature "I fire a burst of ~a with my ~a." fire-mode (get-name it)))
      (dotimes (i fire-mode)
	(trace-shot (cast-ray (lambda (p) (map:obstaclep p)) (cell-line (get-pos creature) cell)) (take-shot it)))
      (make-turn creature (/ (* 100 fire-mode) (get-speed it))))))

(defmethod reload-weapon!((creature actor))
  (aif (and (typep (get-weapon creature) 'entity:firearm) (perform-reload creature))
       (progn (simple-message creature "I reload my ~a (+~a)." (get-name (get-weapon creature)) it)
	      (make-turn creature (get-reload-time (get-weapon creature))))))
