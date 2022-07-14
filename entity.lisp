(in-package :entity)

(defgeneric get-gramma(entity))

;;;Creatures


;;Macros

(defun build-class-property(form)
  (destructuring-bind (name initform &rest other) form
    `(,name :initform ,initform :allocation :class ,@other)))

(defun build-instance-property(form)
  (destructuring-bind (name initform &rest other) form
    `(,name :initform ,initform ,@other)))

(defmacro simple-defclass(name supers (&body instance-properties)
					 (&body class-properties))
  `(defclass ,name ,supers
     (,@(mapcar #'build-class-property class-properties)
      ,@(mapcar #'build-instance-property instance-properties))))

;;Interface

;;Predicates
(defgeneric can-move-p(creature dir))
(defgeneric seesp(creature what))
(defgeneric can-shoot-p(creature))
;;Selectors
(defgeneric get-inventory(creature))
(defgeneric get-melee-damage(creature))
(defgeneric get-weapon(humanoid))
(defgeneric get-hp(creature))
(defgeneric get-max-hp(creature))
(defgeneric get-speed(creature))
(defgeneric get-dodge-coeff(creature))
(defgeneric get-memory(creature))
(defgeneric get-protection(creature))
;;Primitive mutators (used by event commands)
(defgeneric perform-movement(creature delta))
(defgeneric die(creature)) 
(defgeneric take-damage(creature damage-info)) ;TODO: Must be generic for any entity
(defgeneric decrease-health(creature count))
(defgeneric hit(source victim))
;;Event commands
(defgeneric melee-attack!(attacker attackee))
(defgeneric interact-with-cell!(creature dir))
(defgeneric pickup-items!(creature))
(defgeneric perform-reload!(creature))
(defgeneric drop-items!(creature stack))


;;Classes

;;Damage

(deftype damage-type()
  '(member 'thermal 'firearm 'rip 'radiation 'chemical 'electricity 'other 'mechanic))
(defstruct protection
  (thermal 0 :type fixnum)
  (chemical 0 :type fixnum)
  (firearm 0 :type fixnum)
  (mechanic 0 :type fixnum)
  (radiation 0 :type fixnum)
  (electricity 0 :type fixnum))

(defstruct (damage-info (:constructor dmg (damage hit-check &optional (type 'mechanic))))
  (damage (rnd:dices 0 0) :type rnd:dices)
  (hit-check (rnd:dices 1 6) :type rnd:dices)
  (type 'mechanic :type symbol))

(defun damage-count(info)
  (rnd:throw-dices (-> info damage)))

;;

(defmacro define-creature(name supers &key slots class-properties) ;TODO: Must use define-creature-stats instead
  `(progn
     (defclass ,name ,(or supers '(creature))
       ,(append slots
	 (mapcar #'build-class-property class-properties)))
     (export ',(add-prefix name "make-"))
     (export ',name)
     (defun ,(add-prefix name "make-") (x y &rest key-pairs)
       (let* ((pos (make-pos x y))
	      (ins (apply #'make-instance ',name (list* :pos pos key-pairs))))
	 (level:add-entity pos ins)
	 ins))))



(defun equipment-slots(gear)
  (list
   (slot 'prepared  (slot-value gear 'prepared))
   (slot 'used (slot-value gear 'used))))

(defstruct (equipment-slot (:constructor slot (category item)))
  (category nil :type symbol)
  (item nil :type (or item null)))

(defun stack-slot(stack)
  (make-instance 'backpack-slot :stack stack))

(defmethod get-name((slot equipment-slot))
  (get-name (-> slot stack)))

(defmethod slot-stack((slot equipment-slot))
  (aif (-> slot item)
       (item-stack (-> slot item))
       nil))

(defmethod get-gramma((slot equipment-slot))
  (get-gramma (-> slot item)))

(defmethod get-description((slot equipment-slot))
  (get-description (-> slot item)))

(defmethod slot-category((slot equipment-slot))
  (-> slot category))


;;And generic class

(defclass creature(entity)
  ((pos :type pos :accessor get-pos :initarg :pos)
   (hp :accessor get-hp :initarg :hp)
   (max-hp :accessor get-max-hp :initarg max-hp)
   (speed :accessor get-speed :initarg :speed)
   (dodge-coeff :accessor get-dodge-coeff :initarg :dodge-coeff)
   (protection :reader get-protection
	       :initform (make-protection) :allocation :class)
   (hear-addition :initform 0 :allocation :class :reader get-hear-addition)
   (shock-value :initform 0 :type fixnum :reader get-shock-value)
   (blood-symbol :initform 'map:blood :allocation :class :reader blood-symbol)
   (state :initform (creature-control:make-state 'creature-control::standard-state) :type state :initarg :state :reader get-state)
   (effects :initform nil :type list :accessor get-effects)))

(defun set-state(creature state)
  (setf (-> creature state) state)
  (creature-control:init-state state creature))

(defmethod has-effect(creature effect)
  (find-if (of-type effect) (get-effects creature)))

(defclass flying(creature) ())

(defclass corpse(entity)
  ((gramma :initform (ui:static-gramma #\& (ui:layer-color :white)) :initarg :gramma)
   (entity :initarg :entity :reader corpse-owner)))

(defmethod get-name((entity corpse))
  (formatted "a corpse of ~a" (get-name (corpse-owner entity))))

;;Oh, life and death...
(defmethod report-death((creature creature))
  (report (get-pos creature)
	  (msg (formatted "~a dies." (word-capitalize (get-name creature))))
	  (snd "I hear a fall of body." 8)))

(defmethod spawn-corpse((creature creature))
  (awith (make-instance 'corpse :gramma (ui:make-gramma (char-code #\&) (ui:gramma-color (get-gramma creature))) :entity creature)
    (level:add-entity (get-pos creature)
		      it)
    it))

(defmethod die((creature creature))
  (report-death creature)
  (level:remove-entity (get-pos creature) creature)
  (set-state creature (creature-control:make-state 'creature-control:dead))
  (spawn-corpse creature))

(defmethod alivep(creature)
  (and (not (typep (get-state creature) 'creature-control:dead))
       (> (-> creature hp) 0)))

;;Movement

(defmethod perform-movement((creature creature) (delta pos))
  (progn
    (level:remove-entity (-> creature pos) creature)
    (amutf (-> creature pos) (add it delta))
    (level:add-entity (-> creature pos) creature)))

(defmethod perform-movement :after ((creature creature) (delta pos))
  (declare (ignorable delta))
  (map:interact creature (map:search-terrain (get-pos creature) t)))

(defmethod can-move-p((creature creature) (dir pos))
  (and (null (level:get-entities (add (get-pos creature) dir) 'creature)) (not (map:obstaclep (add (get-pos creature) dir)))))

(defmethod get-melee-damage((creature creature))
  (-> creature melee-damage))

;;Damage & combat

(defmethod get-protection-type(entity damage-type)
  (slot-value (get-protection entity) damage-type))

(defmethod dodges(victim damage-info)
  (<= (rnd:throw-dices (-> damage-info hit-check)) (rnd:throw-dices (get-dodge-coeff victim))))

(defmethod take-damage(attackee damage-info)
  (decrease-health attackee (- (rnd:throw-dices (-> damage-info damage)) (get-protection-type attackee (-> damage-info type)))))

(defmethod take-damage :after(attackee damage-info)
  (awith (damage-info-type damage-info)
    (when (member it '(mechanic firearm))
      (map:decorate-terrain (get-pos attackee) (blood-symbol attackee)))))

(defmethod decrease-health(attackee count)
  (when (> count 0)
    (decf (-> attackee hp) count)))

(defmethod decrease-health :after(attackee count)
  (when (not (alivep attackee))
    (die attackee)))

;;Perception

(defmethod seesp(caller (_ null))
  nil)

(defmethod seesp(caller (creature creature))
  "Standard LOS-based algorithm"
  (seesp caller (get-pos creature)))

(defmethod seesp(caller (cell pos))
  (coordinates:trace-line (lambda (p) (not (map:solidp p)))
			  (coordinates:cell-line cell (get-pos caller))))

(defmethod can-shoot-p(creature)
  nil)

;;;Item

(defgeneric same-item-p(item1 item2))


;;Usable
(defgeneric use(item user))
(defclass usable(item) ())

(defmethod use((item usable) user)
  (take-stack (get-inventory user) item))

;;Weapon

(defgeneric reload(firearm ammo-stack))
(defgeneric take-shot(firearm))
(defgeneric get-melee-damage(weapon))
(defgeneric get-ranged-damage(weapon)
  (:method (weapon)
  (-> weapon ranged-damage)))
(defgeneric get-reload-time(firearm))
(defgeneric get-speed(weapon))

(defmacro define-melee(name &body class-properties)
  `(progn
     (simple-defclass ,name (melee)
		      nil
		      ,class-properties)
     (export ',name)))


(defmacro define-weapon(name supers &body class-properties)
  `(progn
     (simple-defclass ,name ,supers
	 nil
	 ,class-properties)
     (export ',name)))



(defclass weapon(item)
  ((melee-damage :initform (dmg (rnd:dices 1 2) (rnd:dices 1 6)) :allocation :class)))

(defclass firearm(weapon)
  ((reload-time :initform 100 :reader get-reload-time :allocation :class)
   (ammo-count :initform 0 :initarg :ammo)
   (fire-modes :initform (circular-list 1) :type circular-list :allocation :class :reader get-fire-modes)
   (loudness :initform 10 :allocation :class :reader get-loudness)))

(defmethod get-description((item firearm))
  (formatted "~a~&Damage: ~a~&Ammunition: ~a" (call-next-method) (rnd:dices-string (-> (get-ranged-damage item) damage)) (-> item ammo-type)))

(defclass primary(firearm) ())
(defclass secondary(firearm) ())
(defclass melee(weapon) ())
(defclass ammo(item) ())

(defmethod same-item-p((item1 firearm) (item2 firearm))
  (and (call-next-method) (= (-> item1 ammo-count) (-> item2 ammo-count))))

(defmethod get-melee-damage(weapon)
  (-> weapon melee-damage))

(defmethod get-speed(weapon)
  (-> weapon speed))

(defmethod get-full-name((entity firearm))
  (formatted "~a (~a/~a)" (get-name entity) (-> entity ammo-count) (-> entity mag-size)))

(defmethod take-shot(firearm)
  (unless (zerop (-> firearm ammo-count))
    (decf (-> firearm ammo-count))
    (get-ranged-damage firearm)))

(defmethod copy-item((item firearm))
  (make-instance (type-of item) :condition (-> item condition) :ammo (-> item ammo-count)))
   
;;Ammo

(defmacro define-ammo(name &body class-properties)
  `(progn
     (simple-defclass ,name(ammo)
	 nil
	 ,class-properties)
     (export ',name)))

;;Armor

(defclass armor(item)
  ((protection :reader get-protection :allocation :class)))

(defmacro define-armor(name &body class-properties)
  `(progn
     (simple-defclass ,name(armor)
		      nil
		      ,class-properties)
     (export ',name)))

;;Interface to items

(defgeneric stack-count(stack))
(defgeneric item-name(stack))
(defgeneric item-description(stack))
(defgeneric same-item-p(stack1 stack2))
(defgeneric merge-stacks(stack1 stack2))
(defgeneric item-actions(stack creature))

;;Final
(defclass item(entity)
  ((condition :initform 1 :type real :initarg :condition)))

(defmacro define-item(name &rest properties)
  (let-be [pivot (position :instance-propeties properties)
	   class-properties (subseq properties 0 pivot)
	   instance-properties (when pivot (subseq properties (1+ pivot)))]
	  `(progn
	     (simple-defclass ,name (item)
			      ,instance-properties
			      ,class-properties)
	     (export ',name))))

(defmethod get-description((item item))
  (formatted "~a~&Weight: ~a~&Condition: ~a%~&" (-> item description) (-> item weight) (* 100(-> item condition))))

(defmacro define-usable(name &body class-properties)
  `(simple-defclass ,name(usable)
       nil
       ,class-properties))
(defmethod copy-item(item)
  (make-instance (type-of item) :condition (-> item condition)))

(defclass item-stack(entity)
  ((item :type item :initarg :item)
   (count :initform 1 :type fixnum :initarg :count)))

(defun make-item(type &rest key-pairs)
  (apply #'make-stack (list* type 1 key-pairs)))

(defun make-stack(type count &rest key-pairs)
  (make-instance 'item-stack :item (apply #'make-stereo-item (cons type key-pairs)) :count count))

(defun make-stereo-item(type &rest key-pairs)
  (apply #'make-instance (cons type key-pairs)))

(defmethod same-item-p((item1 item) (item2 item))
  (and (eq (class-of item1) (class-of item2)) (= (-> item1 condition) (-> item2 condition))))

(defmethod stack-count((stack item-stack))
  (-> stack count))

(defmethod get-name((stack item-stack))
  (with-slots (count item) stack
    (if (> count 1)
	(formatted "~a ~a" count (get-plural-name (-> stack item)))
	(get-singular-name (-> stack item)))))

(defmethod get-description((stack item-stack))
  (get-description (-> stack item)))

(defmethod item-description((stack item-stack))
  (get-description (-> stack item)))

(defmethod merge-stacks((stack1 item-stack) (stack2 item-stack))
  (assert (same-item-p (-> stack1 item) (-> stack2 item)))
  (amutf (-> stack1 count) (+ it (-> stack2 count))
	 (-> stack2 count) 0)
  stack1)

(defmethod same-item-p((stack1 item-stack) (stack2 item-stack))
  (same-item-p (-> stack1 item) (-> stack2 item)))

(defmethod get-gramma((stack item-stack))
  (get-gramma (-> stack item)))

(defmethod item-name((stack item-stack))
  (-> stack item name))

(defmethod free-item((stack item-stack))
  (progn
    (decf (-> stack count))
    (copy-item (-> stack item))))

(defun stack-type(stack creature)
  (type-of (-> stack item)))

(defun stack-item(stack)
  (-> stack item))

(defun item-stack(item)
  (make-instance 'item-stack :item item :count 1))

;;Traps

(defmacro define-anomaly(name &key instance-properties class-properties)
  `(progn (simple-defclass ,name(anomaly)
			   ,instance-properties
			   ,class-properties)
	  (export ',name)))

(defclass anomaly(trap)
  ((activep :initform t :accessor activep)))
(defclass trap(entity)
  ((speed :initform 1 :allocation :class :reader get-speed)))

;;;Props

(defmacro define-prop(name &body slots)
  `(simple-defclass ,name (entity) nil ,slots))

;;;Entity

(defclass entity() ())

(defstruct (names (:constructor make-names (singular concrete plural)))
  (plural "" :type string)
  (singular "" :type string)
  (concrete "" :type string))

(defmethod get-gramma(entity)
  (-> entity gramma))

(defmethod get-description(entity)
  (-> entity description))

(defun get-plural-name(entity)
  (names-plural (-> entity name)))

(defun get-singular-name(entity)
  (names-singular (-> entity name)))

(defun get-concrete-name(entity)
  (names-concrete (-> entity name)))

(defmethod get-full-name(entity)
  (get-name entity))

(defmethod get-name(entity)
  (get-singular-name entity))

(defmethod get-damage(entity)
  (-> entity damage))

;;;Refactoring needed

(defun message-report(message &optional (receiver level:*actor*))
  (ui:add-message (get-message-buffer receiver) message))

(defun msg(str &optional (color (ui:color :gray)))
  (ui:make-gui-string str color))

(defun snd(str intensity &optional (color (ui:color :gray)))
  (make-sound (ui:make-gui-string str color) intensity))

(defstruct (sound (:constructor make-sound(message intensity)))
  (message (ui:str "I hear a sound.") :type ui:gui-string)
  (intensity 8 :type fixnum))

(defun simulate-message(pos message)
  (when (and level:*actor* (seesp level:*actor* pos))
    (ui:add-message (get-message-buffer level:*actor*) message)))

(defun simulate-sound(pos sound &key if-not-see)
  (when (and level:*actor*
	     (< (perception:way-distance pos (way-to level:*actor*) :center (get-pos level:*actor*)) (sound-intensity sound))
	     (or (not if-not-see) (not (seesp level:*actor* pos))))
    (ui:add-message (get-message-buffer level:*actor*) (sound-message sound))
    (memory:add-sound (get-memory level:*actor*) (sound-message sound) pos)))

(defgeneric simulate-noise(entity sound))

(defmethod simulate-noise((entity creature) sound)
  (simulate-sound (get-pos entity) sound))

(defun report(pos visual-message sound)
  (if (and level:*actor* (seesp level:*actor* pos))
      (message-report visual-message level:*actor*)
      (simulate-sound pos sound)))

(defun simple-message(creature format-string &rest args)
  (message-report (msg (apply #'formatted format-string args)) creature))

(defun simple-note(creature format-string &rest args)
  (ui:add-note (get-message-buffer creature) (msg (apply #'formatted format-string args))))
