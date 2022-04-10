(in-package :perception)


;;Actor vision
(defstruct vision
  (plan (make-map-array :element-type 'bit) :type (map-array bit))
  (visible (make-map-array :element-type 'bit) :type (map-array bit)))

(defun make-fov()
  (make-vision))

(defun vision-visiblep(vision p)
  (= (pref (vision-visible vision) p) 1))

(defun make-visible-setter(fov center)
  (with-accessors ((visible vision-visible)
		   (plan vision-plan))
      fov
    (lambda (pos)
      (awith (or (solidp pos) (obstaclep pos))
	(if (or (litp pos)
		(and it (every (lambda (pos) (= (pref visible pos) 1)) (stream-car (coordinates:cell-line pos center)))))
	    (progn
	      (setf (pref visible pos) 1)
	      (setf (pref plan pos) (if (and it (blockedp pos)) 1 0))))))))

;;Shadowcast algorithm
(defun fov-shadowcast(fov center)
  "Entry point of the shadowcast algorithm"
  (awith (vision-visible fov)
    (doarea (pos map:+map-rect+)
      (setf (pref it pos) 0)))
  (awith (make-visible-setter fov center)
    (setf (pref (vision-visible fov) center) 1)
    (map:shadowcast it center)))

;;Lee

(defstruct (lee-map (:conc-name lee-))
  (array (make-map-array :element-type '(unsigned-byte 8)) :type (map-array (unsigned-byte 8)))
  (center (make-pos 0 0) :type pos))

(defun make-lee-check(lee-array)
  (lambda(pos heat)
    (and (in-map-bound-p pos) (not (blockedp pos)) (< (pref lee-array pos) heat) (/= heat 0))))

(defun near-step(lee-map pos)
  "Somewhat slow"
  (awith (lee-array lee-map)
	 (find-best (lambda (p last) (and (in-map-bound-p p) (>= (pref it p) (pref it last))))
		    (cons pos (neighbours pos)))))

(defun near-ways(lee-map pos)
  (awith (lee-array lee-map)
	 (remove-if (lambda (p) (= (pref it p) 0)) (neighbours pos))))

(defun lee-implementation(lee-map lee-check)
  "Efficient Lee-algorithm implementation. Returns a thunk of type (or thunk nil). That thunk has the same type."
  (alambda (heat pos)
	   (declare (dynamic-extent pos heat) (optimize (speed 3)))
	   (when (funcall lee-check pos heat)
	     (setf (pref lee-map pos) heat)
	     (delay
	      (alet ((next (mapcar (curry #'self (1- heat)) (coordinates:neighbours pos)))) ;This is the first self, not this one. Это вам тут не это.
		    (declare (dynamic-extent next))
		    (delay (if (null next)
			       nil
			       (self (mapcar #'force (remove nil next))))))))))

(defun lee(lee-map pos heat)
  "Lee-algorithm entry point. Currently it is bottleneck of performance."
  (psetf (lee-center lee-map) pos)
  (awith (lee-array lee-map)
	 (doarea (pos +map-rect+)
		 (setf (pref it pos) 0))
	 (alet ((thunk (funcall (lee-implementation it (make-lee-check it)) heat pos))) ;Looks like continuation-passing style
	       (declare (dynamic-extent thunk))
	       (when thunk
		 (self (force thunk))))))

(defun way-distance(pos lee-map &key center)
  (- (pref (lee-array lee-map) center) (pref (lee-array lee-map) pos)))

;;Fov info

(defgeneric update-fov(fov pos))

(defclass fov-info()
  ((fov :type vision :initarg :fov :reader get-fov)
   (center :type pos :reader get-center :initarg :center)
   (entity-positions :initform nil :type list :reader fov-entity-positions :initarg :visible-entities)))

(defmethod visiblep((info fov-info) pos)
  (= (pref (-> info fov visible) pos) 1))

(defmethod seenp((info fov-info) pos)
  (= (pref (-> info fov plan) pos) 1))

(defun visible-creatures(fov)
  (remove nil (apply #'append (mapcar (lambda (p) (level:get-entities p 'entity:creature)) (fov-entity-positions fov)))))

(defmethod update-fov((fov vision) pos)
  (fov-shadowcast fov pos)
  (awith (make-instance 'fov-info :fov fov :center pos)
	 (level:do-entities (p entities it)
	   (when (/= (pref (vision-visible fov) p) 0)
	     (push p (-> it entity-positions))))))

(defun get-gramma(fov-info pos)
  (declare (type fov-info fov-info))
  (cond ((visiblep fov-info pos)
	 (map:pos-gramma pos))
	(t get-plan-gramma)))

(defun get-plan-gramma(fov-info pos)
  (if (seenp fov-info pos)
      (ui:static-gramma (code-char 219) (ui:color :dark-gray))
      (ui:static-gramma #\Space (ui:color :black))))

(defun fov-pos-description(fov-info pos)
  (cond ((not (visiblep fov-info pos)) "I have no vision here.")
	((find pos (fov-entity-positions fov-info) :test #'equalp) (formatted "~{~a ~}" (mapcar #'entity:get-name (level:get-entities pos))))
	(t (map:pos-name pos))))
