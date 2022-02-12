;;;;Generator
;;;;TODO: Constants must be separated from logic

;;;Logic

(in-package :generator)

;;;Generation
(defun generate-sector(rect deepness keys doors)
  (let ((lst (make-room-list rect deepness keys doors)))
    (dolist (info lst lst) (apply #'add-room info))))

(defun fill-area(rect terrain)
  (coordinates:doarea (p rect)
    (setf (pref *map* p) terrain)))

(defun add-door(pos)
  (setf (pref *map* pos) (make-instance 'door :opened (rnd:bernoulli))))

(defun add-room(rect doors)
  (let ((pos0 (start rect))
	(pos1 (end rect)))
    (fill-area rect (terrain floor))
    (dolist (door-pos doors)
      (aif door-pos (add-door door-pos)))
    (when (and (>= (x (size rect)) 7) (>= (y (size rect)) 7))
      (let ((inferior-p0  (add pos0 (make-pos (1+ (random 2)) (1+ (random 2)))))
	    (inferior-p1  (sub pos1 (make-pos (1+ (random 2)) (1+ (random 2)))))
	    (rnd (random 4)))
	(format t "Generating inferior : ~a ~a~%" inferior-p0 inferior-p1)
	(fill-area (make-rect inferior-p0  (distance-point inferior-p1 inferior-p0)) (terrain wall))
	(generate-sector (make-rect inferior-p0 (sub inferior-p1 inferior-p0))
			 3
			 (make-compass-rose :north (make-door 0) :south (make-door 0) :west (make-door 0) :east (make-door 0))
			 (make-compass-rose :north (when (= rnd 0) (make-door 3))
					    :south (when (= rnd 1) (make-door 3))
					    :east (when (= rnd 2) (make-door 3))
					    :west (when (= rnd 3) (make-door 3))))))))

;;;Keys & doors

(defmacro make-vertical-keys(north south)
  `(list ,north ,south))
(defmacro make-horizontal-keys(west east)
  `(list ,west ,east))
(defmacro north(key)
  `(car ,key))
(defmacro south(key)
  `(cadr ,key))
(defmacro west(key)
  `(car ,key))
(defmacro east(key)
  `(cadr ,key))

(defun make-door(deepness)
  (if (< (random (1+ deepness)) 7)
      (let* ((first (and (rnd:bernoulli (+ deepness 3)) (make-door (1+ deepness))))
	     (second (if (or (rnd:bernoulli (+ deepness 3)) (not first)) (make-door (1+ deepness)))))
	(if (rnd:bernoulli)
	    (list first second)
	    (list second first)))
      inftree))

;;;Data types

(defstruct (compass-rose (:conc-name rose-) (:type list))
  (north)
  (south)
  (west)
  (east))

(defstruct (room-data (:conc-name room-) (:include rect))
  (doors (make-compass-rose) :type compass-rose))


(defun make-tunnel(rect doors)
  (let* ((hlength (+ (round-down (half (x (size rect)))) (if (rnd:bernoulli) 1 -1))) ;Be careful with them. They are not true halves
	 (hheight (+ (round-down (half (y (size rect)))) (if (rnd:bernoulli) 1 -1)))
	 (center (add (start rect) (make-pos hlength hheight))))
    (remove nil (list (when (rose-north doors) (make-room-data :start (make-pos (x center) (y (start rect)))
							       :size (make-pos 1 (y (distance-point center (start rect))))
							       :doors (list nil nil nil nil)))
		      (when (rose-south doors) (make-room-data :start center
							       :size (make-pos 1 (1+ (y (distance-point (end rect) center))))
							       :doors (list nil nil nil nil)))
		      (when (rose-west doors) (make-room-data :start (make-pos (x (start rect)) (y center))
							      :size (make-pos (x (distance-point center (start rect))) 1)
							      :doors (list nil nil nil nil)))
		      (when (rose-east doors) (make-room-data :start center
							      :size (make-pos (1+ (x (distance-point (end rect) center))) 1)
							      :doors (list nil nil nil nil)))))))

(defun make-room(rect doors)
  (symbol-macrolet ((length (x (size rect)))
		    (height (y (size rect))))
    (let* ((opens (make-compass-rose :north (and (rose-north doors) (< length height) (rnd:bernoulli 8))
				     :south (and (rose-south doors) (< length height) (rnd:bernoulli 8))
				     :west (and (rose-west doors) (< height length) (rnd:bernoulli 8))
				     :east (and (rose-east doors) (< height length) (rnd:bernoulli 8))))
	   (inferior-start (add (start rect)
				(make-pos (if (rose-west opens) 0 1)
					  (if (rose-north opens) 0 1))))
	   (inferior-end (add (end rect)
			      (make-pos (if (rose-east opens) 1 0)
					(if (rose-south opens) 1 0)))))
      (symbol-macrolet ((hlength (+ (x (start rect)) (round-down (half length)) (if (rnd:bernoulli) 1 -1)))
			(hheight (+ (y (start rect)) (round-down (half height)) (if (rnd:bernoulli) 1 -1))))
	(make-room-data :start inferior-start :size (distance-point inferior-end inferior-start)
			:doors (mapcar (lambda (door openedp pos) (when (and door (not openedp))
								    pos))
				       doors opens
				       (make-compass-rose :north (make-pos hlength (1- (y inferior-start))) ;Need modf!
							  :south (make-pos hlength (1+ (y inferior-end)))
							  :west (make-pos (1- (x inferior-start)) hheight)
							  :east (make-pos (1+ (x inferior-end)) hheight))))))))

(defun split-room(rect deepness keys &optional doors)
  (cond ((not (any keys))
	 (if (> (count-of doors) 1)
	     (values
	      (make-tunnel rect doors)
	      (mapcar (lambda (d) (when d inftree)) doors))
	     (format t "Start: ~d End: ~d. Room is not created. ~%"  (start rect) (add (start rect) (size rect)))))
	((or (< (- (random 6) deepness) -2) (< (x (size rect)) 6) (< (y (size rect)) 6))
	 (values (list (make-room rect doors))
		 inftree))
	(t (apply (case (random 4)
		    (0 #'slice-north)
		    (1 #'slice-south)
		    (2 #'slice-west)
		    (3 #'slice-east))
		  (list rect deepness keys doors)))))

(defmacro define-side(side counter-side side-rect counter-rect)
  "Use macroexpand for more concrete description"
  (labels ((make-rose(sym) (add-prefix sym "rose-"))
	   (make-dotted(sym) (add-prefix sym ":"))
	   (rose (primary source
		 &key (north (list primary `(rose-north ,source))) (south (list primary `(rose-south ,source))) (west (list primary `(rose-west ,source))) (east (list primary `(rose-east ,source))))
	     `(make-compass-rose :north ,north :south ,south :west ,west :east ,east))
	   (final-rose
	       (&key (north '(make-horizontal-keys west-keys east-keys)) (south '(make-horizontal-keys west-keys east-keys)) (west '(make-vertical-keys north-keys south-keys)) (east '(make-vertical-keys north-keys south-keys)))
	     `(make-compass-rose :north ,north :south ,south :west ,west :east ,east)))
    `(defun ,(add-prefix side "slice-")(rect deepness keys doors)
       (symbol-macrolet ((,(add-prefix "-keys" side) side-keys)
			 (,(add-prefix "-keys" counter-side) counter-keys))
	 (mvb (,side side-keys)  (split-room ,side-rect (1+ deepness)
					     ,(rose side 'keys
						    (make-dotted side) `(,(make-rose side) keys)
						    (make-dotted counter-side) 'nil)
					     ,(rose side 'doors
						    (make-dotted side) `(,(make-rose side) doors)
						    (make-dotted counter-side) '(make-door deepness)))
	      (mvb (,counter-side counter-keys) (split-room ,counter-rect (1+ deepness)
							    ,(rose counter-side 'keys
								   (make-dotted counter-side) `(,(make-rose counter-side) keys)
								   (make-dotted side) `(,(make-rose counter-side) side-keys))
							    ,(rose counter-side 'doors
								   (make-dotted side) 'nil
								   (make-dotted counter-side) `(,(make-rose counter-side) doors)))
		   (values (append ,side ,counter-side)
			   ,(final-rose (make-dotted side) `(,(make-rose side) side-keys)
					(make-dotted counter-side) `(,(make-rose counter-side) counter-keys)))))))))

(define-side north south (upper-slice rect) (lower-slice rect))
(define-side south north (lower-slice rect) (upper-slice rect))
(define-side west east (left-slice rect) (right-slice rect))
(define-side east west (right-slice rect) (left-slice rect))




;;Room list
(defun make-room-list(rect &optional (deepness 0) (keys (make-compass-rose :north (make-door 2) :south (make-door 2))) doors)
  (format t "Seed: ~a ~%" *random-state*) ;TODO: Must be room tree
  (mapcar (lambda (room) (list (make-rect (start room) (size room)) (-> room doors))) (split-room rect deepness keys doors)))

(defun fill-room(rect)
  "Naively fills room with monsters and traps"
  (let ((content (if (rnd:bernoulli)
		     (lambda (p) (event:add-event (event:make-turn (entity:make-zombie (x p) (y p)) 100)))
		     (lambda (p) (level:add-entity p (make-instance 'entity:mosquito-bald))))))
    (dotimes (i (round (/ (random (max (x (size rect)) (y (size rect)))) 3)))
      (awith (make-pos (random (+ (x (start rect))(x (size rect)))) (random (+ (y (start rect)) (y (size rect)))))
	(unless (obstaclep it)
	  (funcall content it))))))

(defun add-actor(rect)
  (awith (add (start rect) (make-pos (round (/ (x (size rect)) 2)) (round (/ (y (size rect)) 2))))
    (dolist (item (list (entity:make-item 'entity:aksu) (entity:make-item 'entity:knife) (entity:make-item 'entity:pb) (entity:make-stack 'entity:ammo-5.45x39 90) (entity:make-stack 'entity:ammo-9x18 40)))
      (level:add-entity it item))
    (event:add-event (event:make-turn (setf level:*actor* (entity:make-actor (x it) (y it))) 0))))
;;Generator

(defun generate(rect)
  (awith (make-room-list rect)
	 (dolist (room it)
	   (apply #'generator:add-room room))
	 (add-actor (car (first it)))
	 (dolist (room (cdr it))
	   (fill-room (car room)))
	 (dorectangle (pos +map-rect+)
		      (setf (pref *map* pos) (terrain limit)))
	 it))
