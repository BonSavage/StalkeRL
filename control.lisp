;;;;AI & player control facility
;;; Arranges primitive actions to compose complex AI

(in-package :creature-control)

(defstruct (movement-memory (:conc-name movement-))
  (destination nil :type (or entity:creature coordinates:pos null))
  (update-time 0 :type (integer 0)))

;;Turns

(defmethod take-turn :around ((creature actor))
  (if (not (alivep creature))
      (progn
	(panels:game-panel creature (panels:dummy-controller) (update-fov creature) (ui:buffer-source (get-message-buffer creature)))
	nil)
      (call-next-method)))

(defmethod take-turn((creature actor))
  (let-be [fov-info (update-fov creature)]
    (panels:game-panel creature (panels:actor-controller creature fov-info) fov-info (ui:buffer-source (get-message-buffer creature)))))

(defun stuck-up(zombie corpse)
  (level:remove-entity (get-pos zombie) corpse)
  (setf (get-hp zombie) (setf (get-max-hp zombie) (/ (get-max-hp zombie) 2)))
  (simulate-message (get-pos zombie) (msg "The zombie suddenly stands up!"))
  (level:add-entity (get-pos zombie) zombie))

(defun fake-death!(zombie)
  (aif (and (> (get-max-hp zombie) 3) (find-if (lambda (corpse) (eq (entity:corpse-owner corpse) zombie)) (level:get-entities (get-pos zombie) 'entity:corpse)))
       (progn
	 (stuck-up zombie it)
	 (event:make-turn zombie 100))))

(defmethod take-turn :around((creature zombie))
  (if (not (alivep creature))
      (event:make-thunk-event (delay (fake-death! creature)) 1000)
      (call-next-method)))

(defmethod take-turn((creature zombie))
  (awith (if (and level:*actor* (position-known-p creature level:*actor*))
	     (attack! creature level:*actor*)
	     (zombie-idle creature))
    (assert it)
    (update-movement creature (get-memory creature))
    it))

(defmethod take-turn((creature giant))
  (awith (if (and level:*actor* (position-known-p creature level:*actor*))
	     (attack! creature level:*actor*)
	     (move-random! creature))
	 (assert it)
	 (update-movement creature (get-memory creature))
	 it))

;;Movement

(defun report-enemy-position(memory enemy)
  (psetf (movement-destination memory) enemy
	 (movement-update-time memory) 16))

(defun update-movement(creature memory)
    (if (and level:*actor* (seesp creature level:*actor*))
	(psetf (movement-update-time memory) 16 (movement-destination memory) level:*actor*)
	(when (= (decf (movement-update-time memory)) 0)
	  (psetf (movement-destination memory) (if (typep (movement-destination memory) 'entity:creature)
						   (entity:get-pos (movement-destination memory))
						   (movement-destination memory))))))

(defun position-known-p(creature target)
  (or (seesp creature target)
      (eq (movement-destination (get-memory creature)) target)))

(defun move-accurate!(creature dir)
  "Smart move"
  (or
   (interact-with-cell! creature dir)
   (dolist (alternative (remove-if (lambda (p) (> (distance dir p) 1)) (neighbours-delta)) (make-turn creature 100)) ;Returns turn
     (if (can-move-p creature alternative)
	 (return (interact-with-cell! creature alternative))))))

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
