;;;;Map without entities. Only terrain.
;;;;
;;;;

(in-package :map)

(defconstant +size+ (make-pos 48 48) "Size of the map")
(defconstant +map-rect+ (make-rect (make-pos 0 0) +size+))

(defun make-map-array(&rest args)
  (apply #'make-array (cons (list (x +size+) (y +size+)) args)))

(deftype map-array(elem-type)
  `(array ,elem-type (,(x +size+) ,(y +size+))))

;;Terrain
(defgeneric terrain-interact(terrain entity)
  (:method  (terrain entity) (declare (ignore terrain entity)) nil))
(defgeneric terrain-obstaclep(terrain))
(defgeneric terrain-solidp(terrain))
(defgeneric terrain-gramma(terrain))
(defgeneric terrain-name(terrain))

(defclass terrain()
  ((movement-coeff :initform 1 :allocation :class :reader movement-coeff)))

(defclass terrain-info(terrain)
  ((gramma :type ui:gramma :reader terrain-gramma :initarg :gramma)
   (blockingp :type boolean :reader terrain-blockingp :initarg :blockingp :documentation "Used by pathfinding algorithms.")
   (obstaclep  :type boolean :reader terrain-obstaclep :initarg :obstaclep :documentation "General property used when the creature tries to move and many other cases.")
   (solidp :type boolean :reader terrain-solidp :initarg :solidp)
   (name :type string :reader terrain-name :initarg :name)))

(eval-when (:compile-toplevel)
  (defmethod print-object((object terrain-info) stream)
    (format stream "<Terrain ~a>" (-> object name))))

(defmacro define-terrain(terrain-name &key gramma obstacle solid name)
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (progn
       (setf (get ',terrain-name 'terrain ) (load-time-value (make-instance 'terrain-info :gramma ,gramma :obstaclep ,obstacle :solidp ,solid :blockingp ,obstacle :name ,name) t))
       (deftype ,terrain-name()
	 `'(eql ,(get ',terrain-name 'terrain))))))

(define-terrain wall
  :gramma (ui:static-gramma #\# (ui:color :gray))
  :obstacle t
  :solid t
  :name "concrete wall")

(defmacro terrain(symbol)
  `(load-time-value (or (get ',symbol 'terrain) (error "MAP: Undefined terrain name: ~a" ',symbol))))

(defclass terrain-decorator(terrain)
  ((decorated :type terrain :initarg :decorated :accessor decorated)))

(defun attack-cell(pos count)
  (hit-terrain (mref pos) count pos))

(defun door-open(door pos)
  (psetf (openp door) t)
  (update-lights pos))

(defun door-close(door pos)
  (psetf (openp door) nil)
  (update-lights pos))

(defmethod destroyedp(door)
  (<= (-> door hp) 0))

(defgeneric decorator-instance(decorator))

(defmethod decorator-instance(dec)
  dec)

(defun decorate-terrain(pos decorator-name &rest args)
  (psetf (pref *map* pos) (decorator-instance (apply #'make-instance decorator-name :decorated (mref pos) args))))

(defun remove-decorator(pos decorator)
  (psetf (pref *map* pos) (delete decorator (mref pos) :test #'eq)))

;;Map

(defvar *map* (make-map-array :initial-element (terrain wall) :element-type 'terrain-info))
(declaim (type (map-array terrain-info) *map*))

(defvar *lightmap* (make-map-array :initial-element 0 :element-type 'bit))
(declaim (type (map-array bit) *lightmap*))

(defun mref(p)
  (declare (type coordinates:pos p))
  (pref *map* p))

(defun in-map-bound-p(pos)
  (in-bound-p pos +map-rect+))

(defun grant-on-map(pos)
  (make-pos (grant-bounds (x pos) 0 (1- (x +size+)))
	    (grant-bounds (y pos) 0 (1- (y +size+)))))


;;;Light

(defstruct (light-source (:conc-name light-))
  (center (coordinates:make-pos 0 0) :type pos)
  (radius 5 :type number))

(defvar *light-sources* nil)
(declaim (type (list light-source) *light-sources*))

(defun lights-intersectp(source1 source2)
  (<= (coordinates:pythagorean-distance (light-center source1) (light-center source2))
      (abs (- (light-radius source1) (light-radius source2)))))

(defun light-neighbours(source)
  (remove-if (lambda (src) (or (eq source src) (not (lights-intersectp source src))))  *light-sources*))

(defun radius-bound(source &aux (radius2 (expt (light-radius source) 2)))
  (lambda (i j)
    (awith (isqrt (- radius2 (expt j 2)))
      (grant-bounds i (- it) it))))

(defun light-setter(source)
  (lambda (pos)
    (unless (obstaclep pos)
      (setf (pref *lightmap* pos) 1))))

(defun shadow-setter(source)
  (lambda (pos)
    (setf (pref *lightmap* pos) 0)))

(defun in-radius(source)
  (lambda(p) (< (floor (coordinates:pythagorean-distance p (light-center source))) (light-radius source))))

(defun light-on(source &optional (ctg-sequence '((-1 1) (-1 1) (-1 1) (-1 1))))
  (awith (light-setter source)
    (funcall it (light-center source))
    (shadowcast it (light-center source) :bound (radius-bound source) :bound-test (in-radius source) :ctg-sequence ctg-sequence)))

(defun light-off(source)
  (fill-circle (shadow-setter source) source)
  (dolist (light (light-neighbours source))
    (light-on light)))

(defun update-light(source)
  (light-on source))

(defun add-light(source)
  (push source *light-sources*)
  (light-on source))

(defun remove-light(source)
  (setf *light-sources* (delete source *light-sources* :test #'eq))
  (light-off source))

(defun replace-light(source pos)
  (light-off source)
  (setf (light-center source) pos)
  (light-on source))

(defun relight-area(source)
  (light-off source)
  (light-on source))

(defun update-lights(pos)
  "Must be optimized"
  (awith (remove-if (complement (lambda (src) (funcall (in-radius src) pos))) *light-sources*)
    (dolist (src it)
      (light-off src))
    (dolist (src it)
      (light-on src))))

(defun create-light(pos radius)
  (awith (make-light-source :center pos :radius radius)
    (add-light it)
    it))

;;Map accessors

(defstruct (map-cell (:conc-name cell-))
  (terrain nil :type terrain)
  (pos nil :type pos))

(defun obstaclep(p)
  (terrain-obstaclep (mref p)))

(defun solidp(p)
  (terrain-solidp (mref p)))

(defun blockedp(p)
  (terrain-blockingp (mref p)))

(defun litp(p)
  (not (zerop (pref *lightmap* p))))

(defun pos-gramma(p)
  (terrain-gramma (mref p)))

(defun pos-name(p)
  (terrain-name (mref p)))

(defun search-terrain(pos type)
  (alet [it (mref pos)]
    (cond ((typep it 'terrain-decorator) (self (decorated it)))
	  ((typep it type) (make-map-cell :terrain it :pos pos)))))

(defun open-door(cell)
  (door-open (cell-terrain cell) (cell-pos cell)))

(defun close-door(cell)
  (door-close (cell-terrain cell) (cell-pos cell)))

(defmethod destroyedp((cell map-cell))
  (destroyedp (cell-terrain cell)))

(defmethod openp((cell map-cell))
  (openp (cell-terrain cell)))

(defun interact(creature cell)
  (terrain-interact (cell-terrain cell) creature))

(defun map-cell(p)
  (make-map-cell :terrain (mref p) :pos p))

;;Algorithms

(defun shadowcast(setter center &key (bound (lambda (i j) i)) (bound-test #'in-map-bound-p) (ctg-sequence '((-1 1) (-1 1) (-1 1) (-1 1))))
  "Classic shadowcast implementation"
  (iter
    (for map-pos in (combine (lambda (s n) (lambda (p) (coordinates:add center (funcall s (funcall n p)))))
			     (list #'identity #'coordinates:transpose)
			     (list #'identity #'coordinates:negate)))
    (for (init-start-ctg init-end-ctg) in ctg-sequence)
    (macrolet ((map-pos (p) `(funcall map-pos ,p))
	       (bounded-i (i) `(funcall bound ,i j))
	       (build-pos (x &optional (y 'j)) `(grant-on-map (map-pos (make-pos (bounded-i ,x) ,y)))))
      (alet [start-ctg init-start-ctg
		       end-ctg init-end-ctg
		       j 1]
	(let*((start-i (bounded-i (ceiling (* start-ctg j))))
	      (end-i (bounded-i (floor (* end-ctg j))))
	      (blockedp (solidp (grant-on-map (map-pos (make-pos start-i j))))))
	  (when (and (< start-ctg end-ctg) (funcall bound-test (map-pos (make-pos 0 j))))
	    (when (solidp (build-pos (round-down (* start-ctg j))))
	      ;;   (funcall setter (build-pos (round-down (* start-ctg j))))
	      (psetf start-ctg (grant-bounds (/ (- start-i .5) j) start-ctg end-ctg)))
	    (when (solidp (build-pos (round-up (* end-ctg j))))
	      ;;  (funcall setter (build-pos (round-up (* end-ctg j))))
	      (psetf end-ctg (grant-bounds (/ (+ end-i .5) j) start-ctg end-ctg)))
	    (iter (for i from start-i to end-i)
	      (awith (map-pos (make-pos i j))
		(when (in-map-bound-p it)
		  (funcall setter it)
		  (if (solidp it)
		      (unless blockedp
			(self start-ctg (/ (- i .5) j) (1+ j))
			(psetf blockedp t))
		      (when blockedp ;And not obstaclep
			(psetf start-ctg (/ (- i .5) j)
			       blockedp nil)))))
	      (finally (unless blockedp (self start-ctg end-ctg (1+ j)))))))))))


(defun fill-circle(setter source &aux (radius (light-radius source)) (center (light-center source)) (radius2 (expt radius 2)))
  (let-be [pos (make-pos 0 0)]
    (iter
      (for i from (- radius) to radius)
      (awith (isqrt (- radius2 (expt i 2)))
	(iter
	  (for j from (- it) to it)
	  (funcall setter (grant-on-map (add center (amutf (x pos) i (y pos) j)))))))))

