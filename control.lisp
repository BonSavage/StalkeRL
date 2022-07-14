;;;;AI & player control facility
;;; Arranges primitive actions to compose complex AI

(in-package :creature-control)

;;States of creatures
(defgeneric state-execute(state turn))
(defgeneric init-state(state creature))

(defclass standard-state() ())

(defclass dead(standard-state) ())

(defclass time-state()
  ((duration :initarg :duration :reader state-duration)))

(defclass paralyzed(time-state)
  ((prev-state :initarg :previous-state :reader previous-state)))

(defclass zombie-lye(standard-state)
  ((corpse :initarg :corpse :reader fake-corpse)))

(defclass fake-dead(time-state zombie-lye)
  ((restore :initarg :restore :initform nil :reader restoringp)))

(defun make-state(type &rest other-args)
  (apply #'make-instance type other-args))

(defmethod init-state(state creature)
  nil)

(defmethod init-state((state zombie-lye) (creature zombie))
  (level:remove-entity (get-pos creature) creature)) ;TODO: Initialize "corpse" slot here?

(defmethod state-execute((state standard-state) turn)
  (take-turn (entity turn) turn))

(defmethod state-execute((state dead) turn)
  (let-be [creature (entity turn)]
	  (if (typep creature 'entity:actor)
	      (panels:game-panel creature (panels:dummy-controller) (memory:update-fov creature) (ui:buffer-source (get-message-buffer creature)))
	      nil)))

(defun event-wait(event duration)
  (make-thunk-event (delay (exec event)) duration))

(defmethod state-execute((state paralyzed) turn)
  (set-state (entity turn) (previous-state state))
  (event-wait turn (state-duration state)))

(defmethod state-execute((state fake-dead) turn)
  (make-thunk-event (delay (fake-death! (fake-corpse state)
					:restored (restoringp state)))
		    (state-duration state)))

(defmethod state-execute((state zombie-lye) turn)
  (if (and (seesp creature level:*actor*)
	   (< (distance (get-pos creature) (get-pos level:*actor*)) 3))
      (fake-death! (state-corpse (get-state creature)) :restored nil)
      (make-turn (entity turn) 100)))

(defmethod state-end((state time-state))
  (make-state 'standard-state))

(defmethod state-end((state paralyzed))
  (previous-state state))

;;Turns

(defmethod take-turn((creature actor) turn)
  (let-be [fov-info (memory:update-fov creature)
	   event (panels:game-panel creature
				    (panels:actor-controller creature fov-info)
				    fov-info
				    (ui:buffer-source (get-message-buffer creature)))]
    (progn
      (memory:update-actor-memory creature (event-energy event))
      event)))

(defun stuck-up(zombie corpse &key (restored t))
  (level:remove-entity (get-pos zombie) corpse)
  (simulate-message (get-pos zombie) (msg "The lying zombie suddenly starts to move!"))
  (when restored
    (psetf (get-hp zombie) (/ (get-max-hp zombie) 2)
	   (get-max-hp zombie) (get-hp zombie)))
  (level:add-entity (get-pos zombie) zombie))

(defun fake-death!(corpse &key (restored t) &aux (zombie (corpse-owner corpse)))
  (stuck-up zombie corpse :restored restored)
  (set-state zombie (make-state 'standard-state))
  (event:make-turn zombie 100))

(defmethod take-turn(creature turn)
  (awith (action-execute turn creature)
	 (assert it)
	 (memory:update-memory creature (event-energy it))
	 it))

;;AI

(defclass ai-state(action)
  ((next-action :initform nil :initarg :next :type ai-state :reader action-next)))

(defun go-next(ai-state)
  (action-execute
   (aif (action-next ai-state)
	it
	(make-turn (entity ai-state) 0))
   (entity ai-state)))

(defun continuate(state turn &rest other-args)
  (apply #'make-instance (type-of state)
	 :entity (entity turn)
	 :energy (event-energy turn)
	 other-args))

(defun jump(state-type &rest args)
  (take-turn (getf args :entity) (apply #'make-instance state-type :energy 0 args)))

(defclass wander(ai-state) ())

(defclass move-to-point(ai-state)
  ((way :initarg :destination :type (vector pos))))

(defclass follow(ai-state)
  ((who :initarg :destination :type creature)))

(defclass move-dir(ai-state)
  ((dir :initarg :direction :type pos)))

(defclass fight(ai-state) ())

(defclass act-rest(ai-state) ())


;;

(defmethod action-execute((action event:turn) creature)
  (jump 'wander :entity creature))

(defmethod action-execute :before((action fight) (creature zombied))
  (when (rnd:bernoulli 8)
    (report (get-pos creature)
	    (msg "Zombied mumbles \"Join us\".")
	    (snd "I hear a groathsome mumbling \"Join us\"." 6))))

(defmethod action-execute((action fight) creature)
  (awith(memory:movement-destination (get-memory creature))
    (if (typep it 'entity:creature)
	(continuate action (attack! creature it))
	(go-next action))))

(defmethod action-execute :before((action wander) (creature zombie))
  (when (rnd:bernoulli 4)
    (report (get-pos creature)
	    (msg "Zombie whispers.")
	    (snd "I hear a loathsome whisper." 5))))

(defmethod action-execute((action wander) creature)
  (awith (memory:movement-destination (get-memory creature))
    (if (typep it 'entity:creature)
	(jump 'fight :entity creature)
	(continuate action (move-random! creature)))))

(defmethod action-execute((action act-rest) (creature zombie))
  (set-state creature (make-state 'zombie-rest :corpse (spawn-corpse creature))))
;;Zombie

(defun zombie-fall(creature corpse &key (duration 800) restore)
  (entity:set-state creature (make-state 'fake-dead
					 :duration duration
					 :corpse corpse
					 :restore restore)))

;;Movement

(defun move-accurate!(creature dir)
  "Smart move"
  (awith(or
	 (interact-with-cell! creature dir)
	 (interact-with-cell! creature (find-best (lambda (p1 p2) (if (and (can-move-p creature p1)
									   (or (< (distance dir p1) (distance dir p2))
									       (and (= (distance dir p1) (distance dir p2)) (rnd:bernoulli))))
								      p1
								      p2))
						  (neighbours-delta))))
    (assert it)
    it))

;;Combat

(defun attack!(attacker attackee)
  (awith (step-to attacker attackee)
	 (if (> (distance (get-pos attacker) (get-pos attackee)) 1)
	     (move-accurate! attacker it)
	     (melee-attack! attacker attackee))))

(defgeneric choose-target(creature))

(defun zombie-idle(zombie)
  (when (rnd:bernoulli 4)
    (report (get-pos zombie)
	    (msg "Zombie whispers.")
	    (snd "I hear a loathsome whisper." 5)))
  (move-random! zombie))
