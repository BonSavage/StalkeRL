(in-package :entity)
;;;;Defines of entities

;;;Creatures

;;Stalker Gear
(defstruct simple-gear
  (weapon nil :type (or null weapon))
  (armor nil :type (or null armor)))

;;;----Start Actor----
(define-creature actor()
  :slots
  ((hp :initform 10 :type real)
   (max-hp :initform 10 :type real)
   (speed :initform 1 :type real)
   ;;(recoil :initform 0 :type real)
   (dodge-coeff :initform (rnd:dices 1 6) :type rnd:dices)
   (way-to :type perception:lee-map :initform (perception:make-lee-map) :reader way-to)
   (memory :type memory:actor-memory
	   :initform (memory:make-actor-memory
		      :fov (perception:make-fov))
	   :accessor get-memory)
   (message-buffer :initform (ui:make-message-buffer) :type ui:message-buffer :accessor get-message-buffer)
   (inventory :initform (make-actor-inventory))
   (willpower :initform 20 :type real)
   (shock :initform 0 :type fixnum :accessor get-shock)
   (insanity :initform 100 :type fixnum :accessor get-insanity)
   (flashlight :initform nil :accessor get-flashlight))
  :class-properties
  ((gramma (ui:static-gramma #\@ (ui:layer-color :olivine)))
   (name (make-names "I" "me" "us"))))

;;Init

(defmethod initialize-instance :after((act actor) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (-> act flashlight) (map:create-light (get-pos act) 6)))

;;Sounds

(defmethod simulate-noise((act actor) (sound-intensity number))
  (level:do-entities (p ents)
    (dolist (ent ents)
      (when (and (typep ent 'creature)
		 (not (eq act ent))
		 (< (perception:way-distance p (way-to act) :center (get-pos act))
		    (+ sound-intensity (get-hear-addition ent))))
	(memory:report-enemy-position (entity:get-memory ent) act)))))

;;Life & death

(defmethod report-death((creature actor))
  (simple-message creature "I am dead!"))

(defmethod die((creature actor))
  (call-next-method)
  (setf level:*actor* nil))

;;Perception

(defmethod seesp((creature actor) pos)
  (perception:vision-visiblep (memory:get-fov creature) pos))

(defmethod step-to(creature (actor actor))
  (let* ((center (get-pos creature))
	 (near (perception:near-step (-> actor way-to) (get-pos creature))))
    (sub 
     (if (matchesp near center)
	 (or (find-if (complement #'map:blockedp) (stream-car (coordinates:cell-line center (get-pos actor)))) center)
	 near)
     center)))

(defun add-shock(actor count)
  (incf (-> actor shock) count))

;;Movement

(defmethod perform-movement((act actor) delta)
  (declare (ignorable delta))
  (call-next-method)
  (perception:lee (way-to act) (get-pos act) 30))

(defmethod perform-movement :after((act actor) delta)
  (declare (ignore delta))
  (map:replace-light (get-flashlight act) (get-pos act)))

;;Inventory

(defstruct (actor-gear (:conc-name gear-))
  (used nil :type (or null weapon))
  (prepared nil :type (or null weapon))
  (helmet nil :type (or null helmet))
  (armor nil :type (or null armor))
  (accessories nil :type (list accessory)))

(defun equip-gear(gear type stack)
  (setf (slot-value gear type) (free-item stack)))

(defun equip-weapon(gear type stack)
  (equip-gear gear type stack))

(defun equip-accessory(gear stack)
  (push (free-item stack) (gear-accessories gear)))

(defun remove-weapon(gear category)
  (awith (slot-value gear category)
    (psetf (slot-value gear category) nil)
    (when it
      (item-stack it))))

(defun current-weapon(gear)
  (-> gear used))

(defun swap-slots(gear)
  (awith (-> gear prepared)
	 (psetf (-> gear prepared) (-> gear used)
		(-> gear used) it)))

;;Gear

(defstruct actor-inventory
  (backpack nil :type list)
  (gear (make-actor-gear) :type actor-gear))

(defun add-stack(inventory item-stack)
  (unless (zerop (-> item-stack count))
    (aif (find-if (lambda (istack) (and istack (same-item-p (-> item-stack item) (-> istack item)))) (-> inventory backpack))
	 (merge-stacks it item-stack)
	 (push item-stack (-> inventory backpack)))))

(defun remove-stack(inventory item-stack)
  (amutf (-> inventory backpack) (delete item-stack it :count 1 :test #'eq)))

(defun take-stack(inventory stack &key (test #'same-item-p))
  "Takes stack by non strict pattern"
  (aif (find (stack-item stack) (get-backpack inventory) :key #'stack-item :test test)
       (relation-case ((-> stack count) (-> it count))
		      (< (progn
			   (decf (-> it count) (-> stack count))
			   stack))
		      (= (progn
			   (remove-stack inventory it)
			   stack))
		      (> (progn
			   (remove-stack inventory it)
			   it)))))

(defun get-backpack(inventory &optional items-type)
  (awith (-> inventory backpack)
    (if items-type
	(remove-if (complement (lambda (stack) (subtypep (stack-item stack) items-type))) it)
	it)))

;;Selectors

(defmethod get-weapon((creature actor))
  (aif (current-weapon (-> creature inventory gear))
       it
       (make-stereo-item 'hands)))

(defmethod get-inventory((creature actor))
  (-> creature inventory))

(defmethod get-gear((creature actor))
  (-> creature inventory gear))

(defmethod get-damage((creature actor))
  (get-melee-damage (get-weapon creature)))

(defmethod get-protection-type((ent actor) type)
  (let-be [gear (get-gear ent)]
    (reduce (lambda (res item) (if item
				   (+ res (slot-value (get-protection item) type))
				   res))
	    (list* (gear-helmet gear)
		   (gear-armor gear)
		   (remove-if (of-type '(not armor)) (gear-accessories gear)))
	    :initial-value 0)))

(defun find-accessory(actor type)
  (find-if (lambda (item) (eq type (item-accessory-type item))) (gear-accessories (get-gear actor))))

;;Firearms
(defmethod perform-reload((creature actor) &aux (wep (get-weapon creature)))
  "Subpredicate"
  (when (< (-> wep ammo-count) (-> wep mag-size))
    (aif (take-stack (get-inventory creature) (make-stack (-> wep ammo-type) (- (-> wep mag-size) (-> wep ammo-count))))
	 (progn
	   (incf (-> wep ammo-count) (stack-count it))
	   (stack-count it)))))

;;Predicates
(defmethod can-shoot-p((creature actor))
  (and (typep (get-weapon creature) 'firearm) (not (zerop (-> (get-weapon creature) ammo-count)))))
;;;----End Actor----

;;;----Start Zombie----
(define-creature zombie()
  :slots
  ((hp :initform 30)
   (max-hp :initform 30)
   (speed :initform 0.8)
   (memory :initform (memory:make-movement-memory) :reader get-memory)
   (willpower :initform 3 :type real))
  :class-properties
  ((gramma (ui:static-gramma #\z (ui:layer-color :gray)))
   (protection (make-protection :mechanic 1 :firearm 1 :radiation 2 :thermal -1))
   (name (make-names "zombie" "the zombie" "zombies"))
   (description "Zone's spawn, a human ghoul of ghastly appearance. They usually wear broken remains of equipment like military uniform and gasmasks. Their hands are malformed with bruises and cuts, and their fingers are usually grown into claws. They walk around mindlessly in a clumsy manner and attack passers-by. They can occasionally wake up after being stuck down and always do it right after a blowout unless their brain is destroyed. Telephatic monsters easily dominate them and use as footsoldiers.
They are unsettling to behold.")
   (melee-damage (dmg (rnd:dices 2 4) (rnd:dices 1 4 :base 2)))
   (dodge-coeff (rnd:dices 1 4))
   (shock-value 2)
   (hear-addition -2)
   (fall-threshold 10)))

;;Life & death

(defmethod decrease-health :before ((creature zombie) count)
  (when (zerop (random (- 10 (grant-bounds count 0 9))))
    (report (get-pos creature)
	    (msg "Zombie moans.")
	    (snd "I hear a moan." 4))))

(defun zombie-fall(zombie duration &key restore)
  (report-death zombie)
  (creature-control:zombie-fall zombie (spawn-corpse zombie)
				:duration duration
				:restore restore))

(defmethod decrease-health :after ((creature zombie) count)
  (when (and (alivep creature) (>= count (get-fall-threshold creature)))
    (zombie-fall creature 800 :restore nil)))

(defmethod report-death((creature zombie))
  (simulate-message (get-pos creature) (msg "Zombie falls down.")))

(defun get-fall-threshold(zombie)
  (-> zombie fall-threshold))

(defmethod die((creature zombie))
  (if (rnd:bernoulli)
      (zombie-fall creature (+ 2000 (* 1000 (random 3))) :restore t)
      (call-next-method)))

;;Selectors

(defmethod get-damage((creature creature))
  (-> creature melee-damage))

;;;----End Zombie----

;;;----Start Zombied----
(define-creature zombied()
  :slots
  ((hp :initform 10)
   (max-hp :initform 10)
   (speed :initform 0.8)
   (memory :initform (memory:make-movement-memory) :reader get-memory)
   (willpower :initform 5 :type real)
   (weapon :initform (make-stereo-item 'aksu :ammo 30 :condition 0.5))
   (armor :initform (make-stereo-item 'stalker-outfit :condition 0.7)))
  :class-properties
  ((gramma (ui:static-gramma #\@ (ui:layer-color :gray)))
   (name (make-names "zombied stalker" "the zombied" "the zombied"))
   (shock-value 3)
   (description "Human that was possessed by a great telepathic power. Now this is an agressive brain-dead marionette attacking mind-healthy people. It is said their bodies are physiologically and anatomically mutated.
They are frightening to behold.")))

;;;----End Zombied---

;;;----Start Fungal Zombie----
(define-creature fungal-zombie(zombie)
  :class-properties
  ((gramma (ui:static-gramma #\z (ui:layer-color :blue)))
   (name (make-names "fungal zombie" "the fungal zombie" "fungal zombies"))
   (description "Zombie taken by parasitic fungus. It's head is covered with chitin. It behaves like normal zombie, but also can infect nearer creatures with fungal spores.")))
;;;----End Fungal Zombie

;;;---Start Rat---
(define-creature rat()
  :slots
  ((hp :initform 5)
   (max-hp :initform 5)
   (speed :initform 1.5))
  :class-properties
  ((gramma (ui:static-gramma #\r (ui:layer-color :gray)))
   (name (make-names "rat" "the rat" "rats"))
   (description "Gray rodent with long tail. General inhabitant of the underground. May be agressive and dangerous in swarms.")
   (melee-damage (dmg (rnd:dices 1 4) (rnd:dices 1 6)))
   (dodge-coeff (rnd:dices 1 6))))

;;;----End Rat----

;;;----Start Rat Wolf----
(define-creature rat-wolf(rat)
  :slots
  ((hp :initform 10)
   (max-hp :initform 10)
   (speed :initform 2))
  :class-properties
  ((gramma (ui:static-gramma #\R (ui:layer-color :gray)))
   (description "At the first sight it can be considered as dog, but really it is a giant rat with proportions adapted for it's new sizes. They are known as agressive beasts. Usually they lead swarms of lesser rats.
They are unsetting to behold.")
   (melee-damage (dmg (rnd:dices 1 8) (rnd:dices 1 6)))
   (dodge-coeff (rnd:dices 1 4 :base 2))))
;;;----End Rat Wolf----

;;;----Start Blind Dog----
(define-creature blind-dog()
  :slots
  ((hp :initform 10)
   (max-hp :initform 10)
   (speed :initform 2))
  :class-properties
  ((gramma (ui:static-gramma #\d (ui:layer-color :red)))
   (name (make-names "blind dog" "the blind dog" "blind dogs"))
   (description "Dog adapted to live in the Zone. ")
   (melee-damage (dmg (rnd:dices 1 8) (rnd:dices 1 6)))
   (hear-addition 3)
   (dodge-coeff (rnd:dices 1 4 :base 2))))
;;;----End Blind Dog----

;;;----Start Black Dog----
(define-creature black-dog()
  :slots
  ((hp :initform 15)
   (max-hp :initform 15)
   (speed :initform 2))
  :class-properties
  ((gramma (ui:static-gramma #\d (ui:layer-color :gray)))
   (name (make-names "black dog" "the black dog" "black dogs"))
   (description "Mutated wolf. Unlike domestic counterpart, they saved sight and, what is much worse, developed telephatic abilities. Stalkers view black dog as a symbol of death.")
   (melee-damage (dmg (rnd:dices 1 8 :base 2) (rnd:dices 1 6 :base 2)))
   (dodge-coeff (rnd:dices 1 4 :base 4))))
;;;----End Black Dog----

;;;----Start Raven----
(define-creature raven(flying)
  :slots
  ((hp :initform 5)
   (max-hp :initform 5)
   (speed :initform 4))
  :class-properties
  ((gramma (ui:static-gramma #\b (ui:layer-color :gray)))
   (name (make-names "raven" "the raven" "ravens"))
   (description "You don't understand what has taken this bird here. Neverthless, it is another symbol of death.")
   (melee-damage (dmg (rnd:dices 1 4) (rnd:dices 1 3)))
   (dodge-coeff (rnd:dices 1 4 :base 4))))
;;;----End Raven----

;;;----Start Bat----
(define-creature bat(flying)
  :slots
  ((hp :initform 5)
   (max-hp :initform 5)
   (speed 3))
  :class-properties
  ((gramma (ui:static-gramma #\B (ui:layer-color :gray)))
   (name (make-names "screaming bat" "the screaming bat" "screaming bats"))
   (melee-damage (dmg (rnd:dices 1 3) (rnd:dices 2 2)))
   (dodge-coeff (rnd:dices 1 4 :base 3))))

;;;----End Bat----

;;;----Start Boar----
;;;----End Boar----

;;;----Start Flesh----
;;;----End Flesh----

;;;----Start Chimera----
(define-creature chimera()
  :slots
  ((hp :initform 20)
   (max-hp :initform 20)
   (speed :initform 2.5))
  :class-properties
  ((gramma (ui:static-gramma #\C (ui:layer-color :brown)))
   (name (make-names "chimera" "the chimera" "chimeras"))
   (melee-damage (dmg (rnd:dices 2 5 :base 4) (rnd:dices 1 4 :base 4)))
   (dodge-coeff (rnd:dices 1 4 :base 4))))
;;;----End Chimera----

;;;----Start Giant----
(define-creature giant()
  :slots
  ((hp :initform 40)
   (max-hp :initform 40)
   (speed :initform 1.25)
   (memory :initform (memory:make-movement-memory) :reader get-memory))
  :class-properties
  ((gramma (ui:static-gramma #\M (ui:layer-color :brown)))
   (name (make-names "giant" "the giant" "giants"))
   (protection (make-protection :thermal 1 :firearm 2 :mechanic 2))
   (melee-damage (dmg (rnd:dices 2 5) (rnd:dices 1 4 :base 2)))
   (dodge-coeff (rnd:dices 1 3))))

;;Movement

(defmethod perform-movement :after ((creature giant) delta)
  (simulate-sound (get-pos creature) (snd "*THUMB*" 8) :if-not-see t))

;;;----End Giant----


;;;----Start Bloodsucker----
(define-creature bloodsucker()
  :slots
  ((hp :initform 10)
   (max-hp :initform 10)
   (speed :initform 1))
  :class-properties
  ((gramma (ui:static-gramma #\M (ui:layer-color :orange)))))
;;;----End Bloodsucker----

;;;----Start Controller----
(define-creature mind-controller()
  :slots
  ((hp :initform 10)
   (max-hp :initform 10)
   (speed :initform 1))
  :class-properties
  ((gramma (ui:static-gramma #\h (ui:layer-color :orange)))
   (name (make-names "controller" "the controller" "controllers"))))
;;;----End Controller----

;;;----Start Dwarf----
(define-creature dwarf()
  :slots
  ((hp :initform 10)
   (max-hp :initform 10)
   (speed :initform 1))
  :class-properties
  ((gramma (ui:static-gramma #\h (ui:layer-color :gray)))
   (name (make-names "dwarf" "the dwarf" "dwarfs"))
   (description "A small wretch with faint skin. They may look comic until you see a countenance of bitter and malignity on him, and their telekinetic abilities as an evidence. They live in tribes led by greater dwarfs - burers. They can use clubs and their own invention - gravy-shotgun - as weapon.
They are unsetting to behold.")
   (melee-damage (dmg (rnd:dices 1 4) (rnd:dices 1 4)))
   (dodge-coeff (rnd:dices 1 4 :base 2))))
;;;----End Dwarf----


;;;----Start Black Thorns----
(define-creature black-thorns()
  :slots
  ((hp :initform 6)
   (max-hp :initform 6)
   (speed :initform 0.5))
  :class-properties
  ((gramma (ui:static-gramma #\P (ui:layer-color :gray)))
   (name (make-names "black thorns" "the black thorns" "black thorns"))
   (description "Mutated plant that shoots poisonous missiles.")
   (blood-symbol 'map:plant-blood)))
;;;----End Black Thorns----


;;;----Start Triffid----
(define-creature triffid()
  :slots
  ((hp :initform 10)
   (max-hp :initform 10)
   (speed :initform 1)
   (memory :initform (memory:make-movement-memory) :reader get-memory))
  :class-properties
  ((gramma (ui:static-gramma #\P (ui:layer-color :green)))
   (name (make-names "triffid" "the triffid" "triffids"))
   (desription "Carnivorous plant 1.5 - 2 meters high, with sharp, agile and poisonous sting on the head. They have three root-like legs making them mobile. While being totally blind, they can easily find targets due to their ability to sense sounds very accurately. It is also reported somehow sensing fast motions, they are dangerous melee fighters. Most mutants avoid them, exceed amebas, which often hunt them.
They are unsetting to behold.")
   (melee-damage (dmg (rnd:dices 2 4 :base 2) (rnd:dices 2 3 :base 4)))
   (dodge-coeff (rnd:dices 3 3))
   (protection (make-protection
		:mechanic 1
		:thermal -2
		:firearm 2
		:chemical -1))
   (hear-addition 3)
   (blood-symbol 'map:plant-blood)))

(defmethod seesp((creature triffid) (pos pos))
  (when (< (coordinates:distance (get-pos creature) pos) 4)
    (call-next-method)))

;;;----End Triffid----

;;;----Start Ameba----
(define-creature ameba()
  :slots
  ((hp :initform 4)
   (max-hp :initform 4)
   (speed :initform 0.5))
  :class-properties
  ((gramma (ui:static-gramma #\o (ui:layer-color :gray)))
   (melee-damage (dmg (rnd:dices 1 3) (rnd:dices 2 4)))
   (dodge-coeff (rnd:dices 1 2))
   (protection (make-protection :mechanic 5
				:thermal 2
				:chemical 3
				:firearm 8))
   (name (make-names "ameba" "the ameba" "amebas"))
   (description "A slowly moving black spot")))
;;;----End Ameba----

;;;Other

;;----Start Fireball----
(simple-defclass fireball(creature)
    ((hp 3)
      (max-hp 3)
      (speed 4))
    ((gramma (ui:static-gramma #\' (ui:layer-color :blue)))
      (name (make-names "fireball" "the fireball" "fireballs"))))
;;----End Fireball----
;;----Start Poltergeist----
(define-creature poltergeist(creature)
  :slots
  ((hp :initform 6)
   (max-hp :initform 6)
   (speed :initform 1))
  :class-properties
    ((gramma (ui:static-gramma #\' (ui:layer-color :blue)))
     (name (make-names "poltergeist" "the poltergeist" "postergeists"))))
;;----End Poltergeist----

(deftype abnormal()
  '(or anomaly poltergeist fireball))

;;Secret enemy
(simple-defclass strange-color(creature anomaly)
		 ((hp 3)
		  (max-hp 3)
		  (speed .5))
    ((name (make-names "strange color" "the strange color" "some strange color"))))


;;;Multimethods, general methods

;;Slots

(defun slot-item-type(type)
  (cond ((member type '(used prepared)) 'weapon)
	(t type)))

(defun slot-equip(creature slot-type)
  (awith (panels:item-choose (remove-if (complement (lambda (stack) (typep (-> stack item) (slot-item-type slot-type)))) (get-backpack (get-inventory creature))))
	 (when it
	   (equip-weapon (get-gear creature) slot-type (take-stack (get-inventory creature) (item-stack (stack-item it)))))))

(defun put-off-slot(creature category)
  (awith (remove-weapon (get-gear creature) category)
    (add-stack (get-inventory creature) it)))


(defmethod get-max-hp(creature)
  (-> creature max-hp))

;;;Weapons

;;Melee
(define-melee knife
  (name (make-names "knife" "the knife" "knives"))
  (description "Sharp and light knife.")
  (melee-damage (dmg (rnd:dices 2 4) (rnd:dices 1 6)))
  (speed 1.25)
  (weight .3)
  (gramma (ui:static-gramma #\/ (ui:color :silver))))

(define-melee club
  (name (make-names "club" "the club" "clubs"))
  (description "")
  (melee-damage (dmg (rnd:dices 3 5) (rnd:dices 1 4)))
  (speed 0.75)
  (weight 1)
  (gramma (ui:static-gramma #\/ (ui:color :brown))))

(define-melee machete
    (ranged-damage (dmg (rnd:dices 2 4 :base 4) (rnd:dices 1 5)))
  (name (make-names "machete" "the machete" "machetes"))
  (description "Fast chopping weapon.")
  (weight 0.8)
  (speed 1)
  (gramma (ui:static-gramma #\/ (ui:color :gray))))

(define-melee hands
    (ranged-damage (dmg (rnd:dices 1 3) (rnd:dices 1 6)))
  (name (make-names "hands" "the hands" "hands"))
  (desription "My own hands.")
  (weight 0.0)
  (speed 1.25)
  (gramma (ui:static-gramma #\Space (ui:color :black))))

;;Firearms

(define-weapon pb(secondary)
  (ranged-damage (dmg (rnd:dices 1 4 :base 4) (rnd:dices 1 6 :base 2)))
  (mag-size 8)
  (ammo-type 'ammo-9x18)
  (speed 1.5)
  (reload-time 150)
  (weight .5)
  (loudness 3)
  (gramma (ui:static-gramma #\{ (ui:color :gray)))
  (name (make-names "PB" "the PB" "PBs"))
  (description "Small and silent pistol. May be useful in the underground to avoid undesirable attention."))

(define-weapon m1911(secondary)
  (ranged-damage (dmg (rnd:dices 1 8 :base 4) (rnd:dices 1 6 :base 2)))
  (mag-size 7)
  (ammo-type 'ammo-45ACP)
  (speed 1)
  (reload-time 150)
  (weight .5)
  (loudness 10)
  (gramma (ui:static-gramma #\{ (ui:color :silver)))
  (name (make-names "M1911A1" "Colt M1911A1" "M1911A1"))
  (description "Old and still efficient pistol. Has hight stopping power and lethality, but a bit noisy."))

(define-weapon aksu(primary)
  (ranged-damage (dmg (rnd:dices 1 6 :base 4) (rnd:dices 1 4)))
  (mag-size 30)
  (fire-modes (circular-list 3 1))
  (fix-time 1)
  (fix-bonus 3)
  (ammo-type 'ammo-5.45x39)
  (speed 4)
  (reload-time 250)
  (weight 2.5)
  (loudness 15)
  (gramma (ui:static-gramma #\} (ui:color :brown)))
  (name (make-names "AKS-74U" "the AKS-74U" "AKS-74Us"))
  (description "Generic firearm you can trust for."))

(define-weapon as(primary)
  (ranged-damage (dmg (rnd:dices 2 4 :base 4) (rnd:dices 2 2)))
  (mag-size 20)
  (fix-time 1)
  (fix-bonus 2)
  (fire-modes (circular-list 3 1))
  (ammo-type 'ammo-9x39)
  (speed 4.5)
  (reload-time 200)
  (weight 2.5)
  (loudness 3)
  (gramma (ui:static-gramma #\} (ui:color :dark-brown)))
  (name (make-names "AS Val" "the AS Val" "AS Vals"))
  (description "Special operations automatic weapon with intergrated silencer. Uses armor piercing 9X39 ammo."))

;;Ammo

(define-ammo ammo-5.45x39
    (name (make-names "5.45X39" "5.45X39" "5.45X39"))
  (description "Widespread ammo. Used by AKS-74U.")
  (weight .01)
  (gramma (ui:static-gramma #\= (ui:color :green))))

(define-ammo ammo-9x18
    (name (make-names "9X18" "9X18" "9X18"))
  (description "Used by PB.")
  (weight .01)
  (gramma (ui:static-gramma #\= (ui:color :olivine))))

(define-ammo ammo-9x39
    (name (make-names "9X39" "9X39" "9X39"))
  (description "Silent ammo capable of piercing armor. Used by AS Val.")
  (gramma (ui:static-gramma #\= (ui:color :brown)))
  (weight .015))

(define-ammo ammo-45ACP
  (name (make-names ".45ACP" ".45ACP" ".45ACP"))
  (weight .01)
  (gramma (ui:static-gramma #\= (ui:color :olivine)))
  (description "Also known as \"man-stopper\" for it's ballistic characteristics. Used by Colt M1911A1."))

;;Anomalies

(define-anomaly mosquito-bald
    :class-properties
  ((speed .75)
   (damage (dmg (rnd:dices 1 3 :base 4) (rnd:dices 0 0 :base 4)))
   (gramma (ui:static-gramma #\^ (ui:color :brown)))
   (description "")
   (name (make-names "mosquito bald" "the mosquito bald" "mosquito balds"))))

(define-anomaly electro
    :class-properties
  ((speed .8)
   (damage (dmg (rnd:dices 2 5) (rnd:dices 0 0 :base 6) 'electricity))
   (gramma (ui:static-gramma #\^ (ui:color :blue)))
   (description "")
   (name (make-names "electro" "the electro" "the electro"))))

;;Armor

(define-armor stalker-outfit
  (protection (make-protection :mechanic 2 :thermal 3 :firearm 3 :radiation 4 :electricity 3 :chemical 3))
  (weight 2)
  (gramma (ui:static-gramma #\] (ui:color :green)))
  (name (make-names "stalker outfit" "the stalker outfit" "stalker-outfits"))
  (description "Self-made outfit generally used by stalkers."))

;;Artifacts

(define-usable blue-clay
    (gramma (ui:static-gramma #\* (ui:color :blue)))
  (description "Healing artifact.")
  (name (make-names "some blue clay" "the blue clay" "blue clay")))

(define-usable skullhead
  (gramma (ui:static-gramma #\* (ui:color :green)))
  (description "Acrivation of this artifact allows to detect nearby creatures, but they detect user as well.")
  (name (make-names "skullhead" "the skullhead" "skullheads")))

(define-usable mercury-ball
    (gramma (ui:static-gramma #\* (ui:color :gray)))
  (description "Highly radioactive artifact. You should drop it, the sooner is better!")
  (name (make-names "mercury ball" "the mercury ball" "mercury balls")))

;;Accesories

(defgeneric try-to-detect(owner detector abnormal distance))

(define-item detector-simple
    (gramma (ui:static-gramma #\- (ui:color :orange)))
  (accessory-type 'detector :reader item-accessory-type)
  (description "Anomaly detector. Denotes user with a red light.")
  (name (make-names "basic detector" "the basic detector" "basic detectors")))

(defmethod try-to-detect((owner actor) (detector detector-simple) abnormal distance)
  (declare (ignore abnormal detector))
  (if (and (< distance 9) (rnd:bernoulli distance))
      (simple-message owner "The light on my detector blinks red.")))

;;Other

(define-prop skeleton
    (gramma (ui:static-gramma #\& (ui:layer-color :white)))
  (name (make-names "human skeleton" "the human skeleton" "human skeletons"))
  (description "Remains of unfortunate explorer."))
