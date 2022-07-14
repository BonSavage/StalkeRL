;;;;Memory of creatures

(in-package :memory)

(defstruct (movement-memory (:conc-name movement-))
  (destination nil :type (or entity:creature coordinates:pos null))
  (update-energy 0 :type (integer 0)))

;;Actor memory

(defstruct (actor-memory (:conc-name act-memory-))
  (fov (perception:make-fov) :type perception:vision)
  (seen-creatures nil :type (list movement-memory))
  (heared-sounds nil :type (list (cons pos string)))
  (known-traps nil :type (list entity:trap)))

(defun update-actor-memory(actor delta)
  (let-be [memory (get-memory actor)
	   seen (act-memory-seen-creatures memory)]
    (psetf
     (act-memory-seen-creatures memory)
     (iter
       (for movmem in seen)
       (if (>= (decf (movement-update-energy movmem) delta) 0)
	   (collect movmem))))))
	  

(defun trap-knownp(trap memory)
  (find trap (act-memory-known-traps memory) :test #'eq))

(defmethod scout-creature(actor creature)
  (let-be [memory (get-memory actor)]
    (progn
      (entity:add-shock actor (entity:get-shock-value creature))
      (aif (find creature (act-memory-seen-creatures memory) :key #'movement-destination)
	   (progn
	     (psetf (movement-update-energy it) 400)
	     creature)
	   (movement-destination
	    (car (push (make-movement-memory :destination creature :update-energy 400) (act-memory-seen-creatures memory))))))))

(defun scout-trap(actor trap trap-pos)
  (awith (act-memory-known-traps (get-memory actor))
    (if (and (not (find trap it :test #'eq))
	     (< (distance (get-pos actor) trap-pos) 4)
	     (rnd:bernoulli (1+ (* 2 (distance (get-pos actor) trap-pos)))))
	(progn
	  (simple-message actor "I spot ~a." (get-name trap))
	  (push trap
		(act-memory-known-traps (get-memory actor)))
	  trap)
	(find trap it :test #'eq))))

(defun memory:get-fov((actor actor))
  (act-memory-fov (get-memory actor)))

(defun add-sound(memory message pos)
  (push (cons pos message) (-> memory heared-sounds)))

(defmethod update-fov(actor)
  (let-be [memory (get-memory actor)
	   pos (get-pos actor)
	   fov (act-memory-fov memory)]
	  (fov-shadowcast fov pos)
	  (make-instance 'perception:fov-info
		 :fov fov
		 :center pos
		 :marks (memory-marks memory)
		 :entity-positions
		 (collect-entities actor memory))))

(defmethod update-fov :after(actor)
  (psetf (act-memory-heared-sounds (get-memory actor)) nil))


(defun memory-creatures-marks(memory)
  (filtering-map (lambda (move-mem &aux (creature (movement-destination move-mem)))
		   (unless (vision-visiblep (act-memory-fov memory)
					    (get-pos creature))
		     (cons (get-pos creature)
			   (make-mark
			    (ui:static-gramma #\! (ui:cell-color :default))
			    (formatted "There must be ~a." (get-name creature))))))
		 (act-memory-seen-creatures memory)))

(defun memory-sounds-marks(memory)
  (mapcar (lambda (ps) (cons (car ps)
			     (make-mark (ui:static-gramma #\? (ui:cell-color :sound-mark))
					(cdr ps))))
	  (act-memory-heared-sounds memory)))

(defun memory-marks(memory)
  (append (memory-creatures-marks memory)
	  (memory-sounds-marks memory)))

(defun collect-entities(actor memory)
  (let-be [visible-entities (filtering-map (lambda (p) (when (vision-visiblep (act-memory-fov memory) (car p))
							 (cons (car p) (remove-if
									(lambda (ent) (and (typep ent 'trap) (not (trap-knownp ent memory))))
									(cdr p)))))
					   (level:pos-entities-alist))]
    (iter
      (for (pos . entities) in visible-entities)
      (dolist (cr (remove-if (of-type '(not creature)) entities))
	(scout-creature actor cr))
      (finally (return visible-entities)))))

;;Movement-memory

(defun report-enemy-position(memory enemy)
  (psetf (movement-destination memory) enemy
	 (movement-update-energy memory) 1200))

(defun update-memory(creature delta &aux (memory (get-memory creature)))
  (if (and level:*actor* (seesp creature level:*actor*))
	(psetf (movement-update-energy memory) 1200 (movement-destination memory) level:*actor*)
	(when (<= (decf (movement-update-energy memory) delta) 0)
	  (psetf (movement-destination memory) (if (typep (movement-destination memory) 'entity:creature)
						   (entity:get-pos (movement-destination memory))
						   (movement-destination memory))))))

(defun position-known-p(creature target)
  (or (seesp creature target)
      (eq (movement-destination (get-memory creature)) target)))
