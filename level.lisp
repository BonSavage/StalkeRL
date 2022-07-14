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

(defun pos-entities-alist()
  (iter
    (for (k v) in-hashtable *entities*)
    (collect (cons k v))))

;;;Map interface

(defun get-entities-gramma(entities)
  (localf f(type) (find-if (of-type type) entities)
    (acond
      ((f 'entity:creature) (entity:get-gramma it))
      ((f '(or entity:item-stack entity:corpse))
       (if (f 'entity:trap)
	   (ui:augment-gramma (entity:get-gramma it) :background (ui:color :orange))
	   (entity:get-gramma it)))
      ((f 'entity:trap) (entity:get-gramma it)))))
