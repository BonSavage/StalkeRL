(in-package :level)

;;Entities on the map

(defparameter *actor* nil)
(defparameter *entities* (make-hash-table :test 'equalp :size 40 :key-type 'pos :value-type 'list :rehash-size 1.5 :rehash-threshold .8))

(defgeneric get-entities(pos &optional type))
(defgeneric remove-entity(pos entity))
(defgeneric add-entity(pos entity))

(defmethod get-entities((pos pos) &optional type)
  (let ((lst (gethash pos *entities*)))
    (aif type
	 (remove-if (lambda (entity) (not (typep entity it))) lst)
	 lst)))

(defmacro do-entities((pvar entities &optional result) &body forms)
  `(dohash (,pvar ,entities *entities* ,result)
	   ,@forms))

(defun (setf get-entities)(new-val place)
  (setf (gethash place *entities*) new-val))

(defmethod remove-entity((pos pos) (entity entity))
  (when (null (setf (get-entities pos) (delete entity (get-entities pos) :count 1)))
    (rem-entities pos)))

(defmethod add-entity((pos pos) (entity entity))
  (push entity (gethash pos *entities*)))

(defun rem-entities(pos)
  (remhash pos *entities*))

(defun clear-level()
  (clrhash *entities*))

;;;Map interface

(defun get-gramma(fov-info pos) ;Creature | Item & Trap | Item | Trap | Terrain
  (acond
   ((get-entities pos 'entity:creature)
    (entity:get-gramma (first it)))
   ((get-entities pos '(or entity:item-stack entity:corpse))
    (if (get-entities pos 'entity:trap)
        (ui:augment-gramma (entity:get-gramma (first it)) :background (ui:color :crimson))
	(entity:get-gramma (first it))))
   ((get-entities pos 'entity:trap)
    (entity:get-gramma (first it)))
   (t (perception:get-gramma fov-info pos))))
