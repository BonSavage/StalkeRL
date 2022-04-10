(in-package :map)

;;Terrain definitions
(define-terrain floor
  :gramma (ui:static-gramma #\. (ui:color :gray))
  :obstacle nil
  :solid nil
  :name "concrete floor")

(define-terrain tunnel
  :gramma (ui:static-gramma #\, (ui:color :gray))
  :obstacle nil
  :solid nil
  :name "tunnel")

(define-terrain limit
  :gramma (ui:static-gramma #\W (ui:cell-color :limit))
  :obstacle t
  :solid t
  :name "wall marked as \"Wall\"")

(define-terrain vertical-railtrack
  :gramma (ui:static-gramma (code-char 186) (ui:color :gray))
  :obstacle nil
  :solid nil
  :name "railtrack")

(define-terrain horizontal-railtrack
  :gramma (ui:static-gramma (code-char 205) (ui:color :gray))
  :obstacle nil
  :solid nil
  :name "railtrack")

(define-terrain heavy-gate
  :gramma (ui:static-gramma (code-char 219) (ui:color :silver))
  :obstacle t
  :solid t
  :name "heavy metal gate")

(define-terrain trash
  :gramma (ui:static-gramma #\" (ui:color :dark-gray))
  :obstacle nil
  :solid nil
  :name "metal junk")

;;Decorator

(defclass blood(terrain-decorator) ())

(defclass moss(terrain-decorator) ())

(defclass liana(terrain-decorator)
  ((solidp :initform t :allocation :class :reader terrain-solidp)
   (gramma :initform (ui:static-gramma #\" (ui:color :green)) :allocation :class :reader terrain-gramma)
   (name :initform "hanging liana" :allocation :class :reader terrain-name)))

(defmethod terrain-gramma((terrain moss))
  (ui:augment-gramma (terrain-gramma (decorated terrain)) :color (ui:color :olivine)))

(defmethod terrain-gramma((terrain blood))
  (ui:augment-gramma (terrain-gramma (decorated terrain)) :color (ui:color :crimson)))

(defmethod terrain-obstaclep((dec terrain-decorator))
  (terrain-obstaclep (decorated dec)))

(defmethod terrain-solidp((dec terrain-decorator))
  (terrain-solidp (decorated dec)))

(defmethod terrain-blockingp((dec terrain-decorator))
  (terrain-blockingp (decorated dec)))

(defmethod terrain-name((dec blood))
  (formatted "blood stained ~a" (terrain-name (decorated dec))))

(defmethod terrain-name((dec moss))
  (formatted "moss grown ~a" (terrain-name (decorated dec))))

(defmethod hit-terrain((dec terrain-decorator) count pos)
  (hit-terrain (decorated dec) count pos))

(defgeneric hit-terrain(terrain count pos))

(defclass door(terrain)
  ((name :initform "metal door" :allocation :class :reader terrain-name)
   (hp :type fixnum :initform 20 :accessor hp :initarg :hp)
   (openp :type boolean :accessor openp :initarg :opened)
   (blockingp :type boolean :reader terrain-blockingp :initform nil :allocation :class)))

(defclass grate-door(door)
  ((name :initform "metal grate door" :allocation :class :reader terrain-name)
   (solidp :initform nil :type boolean :allocation :class :reader terrain-solidp)))

;;Methods

(defmethod decorator-instance((decorator blood))
  (if (typep (decorated decorator) 'blood)
      (decorated decorator)
      decorator))

(defmethod terrain-gramma((terrain door))
  (if (openp terrain)
      (ui:static-gramma #\\ (ui:color :gray))
      (ui:static-gramma #\+ (ui:color :gray))))

(defmethod terrain-gramma((terrain grate-door))
  (if (openp terrain)
      (ui:static-gramma #\\ (ui:color :dark-gray))
      (ui:static-gramma #\+ (ui:color :dark-gray))))

(defmethod terrain-obstaclep((terrain door))
  (not (openp terrain)))

(defmethod terrain-solidp((terrain door))
  (not (openp terrain)))

(defmethod hit-terrain((door door) count pos)
  (when (<= (decf (-> door hp) count) 0)
    (psetf (pref *map* pos) (terrain tunnel))
    (update-lights pos)))

(defmethod terrain-interact((junk (eql (terrain trash))) (creature entity:actor))
  (entity:simple-message creature "I noisly step on some metal junk.")
  (entity:simulate-noise creature 8))

(defmethod terrain-interact((junk (eql (terrain trash))) creature)
  (entity:simulate-sound (entity:get-pos creature) (entity:snd "I hear a metal noise." 8)))
