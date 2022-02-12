;;;; ui.lisp
;;; Todo:

(in-package :ui)

;;;Constants
(eval-when (:compile-toplevel :load-toplevel)
  (def-symbol-map color-map
    (:black (0 0 0))
    (:dark-gray (64 64 64))
    (:gray (128 128 128))
    (:white (255 255 255))
    (:crimson (192 0 0))
    (:red (128 0 0))
    (:brown (128 24 24))
    (:dark-brown (64 24 24))
    (:silver (192 192 192))
    (:green (0 128 0))
    (:olivine (0 96 48))
    (:blue (0 0 128))
    (:orange (128 64 64)))

  (def-symbol-map cell-color-map
    (:default (:gray :black))
    (:title (:white :black))
    (:selected (:black :white))
    (:hp (:crimson :black))
    (:armor (:blue :black))
    (:selected-alpha (:white :black))
    (:limit (:black :gray))))


;;;Low-level

(defstruct (simple-color (:conc-name color-) (:constructor make-color (red green blue)))
  (red 0 :type (unsigned-byte 8))
  (green 0 :type (unsigned-byte 8))
  (blue 0 :type (unsigned-byte 8)))

(defstruct (cell-color (:constructor make-cell-color (char-color background-color)))
  (char-color nil :type color)
  (background-color nil :type color))

(deftype color()
  '(or simple-color cell-color))

(defstruct (gramma (:constructor construct-gramma(code color)) (:predicate grammap))
  (code 0 :type (unsigned-byte 8))
  (color nil :type (or color cell-color)))

(defmethod print-object((object simple-color) stream)
  (format stream "<Color>"))

(defmethod print-object((object gramma) stream)
  (format stream "<Gramma>"))

(defun make-gramma(code color)
  (construct-gramma code color))

(defun free-gramma(cell-gramma)
  (make-gramma (-> cell-gramma code) (-> cell-gramma color char-color)))

(defun cell-gramma-p(gramma)
  (and (grammap gramma) (cell-color-p (-> gramma color))))

(defun simple-gramma-p(gramma)
  (and (grammap gramma) (simple-color-p (gramma-color gramma))))

(deftype simple-gramma() '(satisfies simple-gramma-p))

(deftype cell-gramma() '(satisfies cell-gramma-p))

(defmacro let-foreign-pos((cpos-name pos) &body body)
  `(cffi:with-foreign-object (,cpos-name '(:struct api:cpos))
     (cffi:with-foreign-slots ((api:x api:y) ,cpos-name (:struct api:cpos))
       (psetf api:x (x ,pos)
	      api:y (y ,pos)))
     ,@body))

(defmacro let-foreign-gramma((cgramma-name gramma) &body body)
  `(cffi:with-foreign-object (,cgramma-name '(:struct api:gramma))
     (cffi:with-foreign-slots ((api:char api:red api:green api:blue) ,cgramma-name (:struct api:gramma))
       (awith (-> gramma color)
	 (psetf api:char (-> ,gramma code)
		api:red (-> it red)
		api:green (-> it green)
		api:blue (-> it blue))))
     ,@body))

(defmacro let-foreign-color((ccolor-name color) &body body)
  `(cffi:with-foreign-object (,ccolor-name '(:struct api:color))
     (cffi:with-foreign-slots ((api:red api:green api:blue) ,ccolor-name (:struct api:color))
       (psetf api:red (-> ,color red)
	      api:green (-> ,color green)
	      api:blue (-> ,color blue)))
     ,@body))

(defmacro let-foreign-rect((crect-name rect) &body body)
  `(cffi:with-foreign-object (,crect-name '(:struct api:rect))
     (cffi:with-foreign-slots ((api:start api:size) ,crect-name (:struct api:rect))
       (let-foreign-pos (strt (start ,rect))
	 (let-foreign-pos (size (size ,rect))
	   (psetf api:start strt
		  api:size size))))
     ,@body))

(defun draw-simple-gramma(gramma pos)
  (let-foreign-gramma(gr gramma)
    (let-foreign-pos (ps pos)
      (api:draw-gramma gr ps))))

(defun fill-cell(color pos)
  (let-foreign-color (clr color)
    (let-foreign-pos (ps pos)
      (api:fill-cell clr ps))))

(defun draw-cell-gramma(cell-gramma pos &aux (gramma (free-gramma cell-gramma)))
  (let-foreign-gramma (gr gramma)
    (let-foreign-color (clr (-> cell-gramma color background-color)) ;Ad-hocery for performance
      (let-foreign-pos (ps pos)
	(api:fill-cell clr ps)
	(api:draw-gramma gr ps)))))


;;;High-order routines

(defvar *drawable* nil "List of static drawable widgets")
(declaim (special *drawable*))

(defun draw-start()
  "Must be called in draw-procedures"
  (progn
    (api:render-clear)
    (dolist (drw (reverse *drawable*))
      (draw-view drw))))

(defun draw-end()
  (api:render-present))

(defun draw-all()
  (draw-start)
  (draw-end))

(defun map-color(i)
  (apply #'make-color (cdr (index->pair +color-map+ i))))

(defun map-cell-color(i)
  (apply #'make-cell-color
	 (mapcar (lambda (sym)
		   (apply #'make-color (cdr (assoc sym +color-map+))))
		 (cdr (index->pair +cell-color-map+ i)))))

(defmacro cell-color(symbol)
  `(load-time-value (map-cell-color (symbol->index +cell-color-map+ ,symbol))))

(defmacro color(symbol)
  `(load-time-value (map-color (symbol->index +color-map+ ,symbol))))

(defmacro layer-color(symbol)
  `(make-cell-color (color ,symbol) (color :black)))

;;Types

(defmacro static-gramma(char color)
  "To be used outside this package"
  `(load-time-value (make-gramma (char-code ,char)
				 ,color)
		    t))

(defstruct (gui-string (:constructor make-gui-string (chars color)))
  (chars "" :type string)
  (color (color :gray) :type (or color cell-color)))

(defun gramma(gstr i)
  (make-gramma (char-code (aref (gui-string-chars gstr) i)) (gui-string-color gstr)))

(defun augment-gramma(gramma &key (background (make-color 0 0 0)) color)
  (let* ((gramma-color (gramma-color gramma))
	 (char-color (cond(color color)
			  ((cell-color-p gramma-color) (cell-color-char-color gramma-color))
			  (t gramma-color)))
	 (background-color (cond (background background)
				 ((cell-color-p gramma-color) (cell-color-background-color gramma-color))
				 (t background))))
    (construct-gramma (gramma-code gramma) (make-cell-color char-color background-color))))

(defun gstr-length(gui-string)
  (length (gui-string-chars gui-string)))

;;String lists

(defun str-lines(str line-length)
  (mvb [sliced rest] (str-slice str line-length)
       (if rest
	   (cons sliced (str-lines rest line-length))
	   (list sliced))))

(defun str-slice(str slice-pos)
  (let [slice-pos (if (< (length str) slice-pos) (length str) slice-pos)
	real-pos (aif (position #\Newline str) (min (1+ it) slice-pos) slice-pos)]
    (values (remove #\Newline (subseq str 0 real-pos) :from-end t :count 1) (when (< real-pos (length str)) (subseq str real-pos)))))

(defun gstr-lines(gstr line-length)
  (mapcar
   (lambda (str)
     (make-gui-string str (-> gstr color)))
   (str-lines (-> gstr chars) line-length)))

(defun gstr-slice(gstr slice-pos)
  (let [(sliced rest) (str-slice (-> gstr chars) slice-pos)]
    (values (make-gui-string sliced (-> gstr color)) (when rest (make-gui-string rest (-> gstr color))))))

(defun text-lines(gstr-list line-length &aux (line-length (1+ line-length)))
  "Formatted gui-string lines"
  (pm/amatch [gstr-list line-length nil]
	     self(nil _ res) (when res (list (reverse res)))
	     self(gstr-list 0 res) (cons (reverse res) (self gstr-list line-length nil))
	     self(gstr-list 1 res) (self gstr-list 0 res)
	     self((gstr . next) [len (integer 2)] res) (let  [(sliced rest) (gstr-slice gstr (1- len))]
							 (self (aif rest (cons it next) next)
							       (if rest
								   0
								   (- len (gstr-length sliced) 1))
							       (cons sliced res)))))

(defun catalogue-lines(gstr-list sub-length)
  "Formatted and aligned lines"
  (apply #'append (mapcar #'(lambda (gstr) (gstr-lines gstr sub-length)) gstr-list)))

(defun one-lines(page line-length)
  "Aligned, elem per line"
  (mapcar (lambda (elem)
	    (pm/amatch [elem line-length]
		       self(_ 0) nil
		       self([str gui-string] len) (gstr-slice str len)
		       self((gstr . rest) len) (awith (gstr-slice gstr len)
						      (cons it (self rest (- (length it) len))))))
	  page))

(defun select-line(page selected-index &key (convert #'gstr-select))
  (when page ;TODO: Should throw exception
    (awith (elt page selected-index)
	   (substitute (funcall convert it) it page :start selected-index :count 1 :test #'eq))))

(defun page-alphabetic(page)
  (mapcar (lambda (alpha title) (cons (alpha-string alpha) (if (listp title) title (list title))))
	  (range #\a #\z (lambda (ch) (code-char (1+ (char-code ch)))) #'char=) page))

(defun gstr-select(gstr &optional (color (cell-color :selected)))
  (declare (ignore len))
  (make-gui-string (-> gstr chars) color))

(defun alpha-select(alpha-string)
  (declare (ignore len))
  (make-gui-string (-> alpha-string chars) (cell-color :selected-alpha)))

(defun alpha-string(char &optional (color (color :gray)))
  (make-gui-string (formatted "~a)" char) color))

;;Level model

(defclass buffer()
  ((unprinted :initform nil :type list)))

(defclass message-buffer(buffer)
  ((print-once :initform (make-buffer) :type buffer)
   (printed :initform nil :type list)))

(defun add-message(buf gui-string)
  (push gui-string (-> buf unprinted))
  nil)

(defun add-note(buf gui-string)
  (add-message (-> buf print-once) gui-string))

(defmethod emptyp((buf buffer))
  (null (-> buf unprinted)))

(defmethod pop-unprinted((buf message-buffer))
  (if (-> buf unprinted)
      (awith (call-next-method buf)
	     (setf (-> buf printed) (nconc (-> buf printed) it))
	     it)
      (pop-unprinted (-> buf print-once))))

(defmethod pop-unprinted((buf buffer))
  (awith (-> buf unprinted)
	 (psetf (-> buf unprinted) nil)
	 (reverse it)))

(defun make-message-buffer()
  (make-instance 'message-buffer))

(defun make-buffer()
  (make-instance 'buffer))

(defun get-printed(buf)
  (-> buf printed))


;;Dynamic

(defstruct (handler (:constructor handler(closure)))
  (closure nil :type (function (handler) t))
  (dependent nil))

(defmethod set-dependent(handler dep)
  (assert (null (-> handler dependent)))
  (setf (-> handler dependent) dep))

(defmethod reset-dependent(handler dep)
  (setf (-> handler dependent) dep))

;;Scrollable

(defstruct indexed-sequence
  (sequence nil :type sequence :read-only t)
  (index 0 :type fixnum))

(defun indexed-subsequence(is)
  (with-slots (sequence index) is
    (subseq sequence (grant-bounds index 0 (length sequence)))))

(defstruct (scrollable (:include indexed-sequence))
  (size (make-pos 1 1) :type coordinates:coordinates :read-only t))

(defun scroll-pages(scrollable count)
  (scroll-lines scrollable (* count (y (-> scrollable size)))))

(defun scroll-lines(scrollable count &aux (len (length (-> scrollable sequence))))
  (progn
    (amutf (-> scrollable index) (grant-bounds (+ it count) 0 (1- len)))
    nil))

(defun end-reached-p(scrollable)
  "Is index pointing to the last line?"
  (with-slots (index sequence) scrollable
    (>= index (1- (length sequence)))))

(defun last-page-p(scrollable)
  "Is index inside the last page?"
  (with-slots (index sequence size) scrollable
    (<= (- (length sequence) index) (y size))))

(defun page-index(scrollable)
  "Number of the current page and dependent index of the selected line"
  (with-slots (index size) scrollable
    (floor index (y size))))

(defun page-bounds(scrollable)
  "Start index and end index of the current page"
  (awith (page-index scrollable)
	 (values it (grant-bounds (+ it (y (-> scrollable size))) it (length (-> scrollable sequence))))))

(defun scrollable-page(scrollable &key  aligned)
  "Current page of the scrollable"
  (with-slots (sequence index size) scrollable
    (multiple-value-call #'scrollable-subseq scrollable
			 (if aligned
			     (page-bounds scrollable)
			     (values (grant-bounds index 0 (length sequence))
				     (grant-bounds (+ index (y size)) index (length sequence)))))))

(defun scrollable-subseq(scrollable start &optional end)
  "Just get the subsequence of the scrollable lines"
  (with-slots (sequence index) scrollable
    (subseq sequence start end)))

(defun menu-page(scrollable &key (convert #'gstr-select))
  "Scrollable page with selected line"
  (select-line (scrollable-page scrollable :aligned t) (value-second (page-index scrollable)) :convert convert))

(defun menu-selected(scrollable)
  "Selected line"
  (when (-> scrollable sequence)
    (elt (-> scrollable sequence) (-> scrollable index))))

(defun alphabetic-page(scrollable &key (convert  #'gstr-select))
  (select-line (page-alphabetic (menu-page scrollable :convert convert)) (value-second (page-index scrollable))
	       :convert (lambda (line) (cons (alpha-select (first line)) (rest line)))))

;;Dynamic

(defmacro control(controller &optional else)
  (alexandria:with-gensyms (res)
    `(let ((,res (funcall ,controller (api:get-key-event))))
       (cond
	 ((eq 'exit ,res) (return nil))
	 ((null ,res) ,else)
	 (t (return ,res))))))

(defmacro controller(&rest key2expr)
  `(lambda (key) (controller-body key ,@key2expr)))

(defmacro handle-input(&rest key2expr)
  `(controller-body (api:get-key-event) ,@key2expr))

(defmacro controller-let(&rest key2expr)
  `(control (controller ,@key2expr)))

(defmacro controller-body(event &rest key2expr)
  `(case ,event
     ,@(mapcar
	(lambda (cs)
	  (cons (awith (car cs)
		  (predicate-case it
		    (characterp (api:key-to-event (char-code it)))
		    (^(eq 't _) 't)
		    (symbolp (case it
			       (up 82)
			       (down 81)
			       (right 79)
			       (left 80)
			       (t (name-char it))))))
		(cdr cs)))
	key2expr)))

;;;Primitive drawers (must be called indirectly through closures)

(defun draw-gramma(gramma cursor)
  (typecase gramma
    (simple-gramma (draw-simple-gramma gramma cursor))
    (cell-gramma (draw-cell-gramma gramma cursor)))
  (incf (-> cursor x)))

(defun draw-string(gui-string cursor)
  "Draw a string on a line"
  (awith (gstr-length gui-string)
    (unless (zerop it)
      (let* ((gramma (gramma gui-string 0))
	     (draw (if (cell-gramma-p gramma) #'draw-cell-gramma #'draw-simple-gramma)))
	(dotimes (i it cursor)
	  (funcall draw (amutf (gramma-code gramma) (char-code (aref (-> gui-string chars) i))) cursor)
	  (incf (-> cursor x)))))))

(defun draw-cascade(cascade cursor)
  "Draw some strings on a line"
  (doseq (elem cascade cursor)
	 (predicate-case elem
			 (gui-string-p (progn (draw-string elem cursor) (amutf (x cursor) (1+ it))))
			 (grammap (draw-gramma elem cursor)))))

(defun draw-line(elem cursor)
  (predicate-case elem
		  (listp (draw-cascade elem cursor))
		  (gui-string-p (draw-string elem cursor))
		  (grammap (draw-gramma elem cursor))))

(defun draw-page(page cursor &aux (cursor (make-pos (x cursor) (y cursor))) (start-x (x cursor)))
  "Draw on some lines"
  (predicate-case page
		  (gui-string-p (draw-line page cursor))
		  (t (doseq (elem page cursor)
			    (draw-line elem cursor)
			    (amutf (x cursor) start-x
				   (y cursor) (1+ it))))))

(defun draw-scrollable(scrollable) ;Shoud be removed. Or not?
  (with-slots (rectangle index sequence) scrollable
    (progn
      (draw-page (scrollable-page scrollable)
		 (start (-> scrollable rectangle))))))

(defun draw-frame(color rect)
  (let* ((background-color (if (cell-color-p color) (-> color background-color)))
	 (frame-color (if (cell-color-p color) (-> color char-color) color)))
    (let-foreign-rect (r rect)
      (let-foreign-color (fc frame-color)
	(when background-color
	  (let-foreign-color (bc background-color)
	    (api:draw-rectangle r bc)))
	(api:draw-frame r fc)))))

(defun draw-rectangle(color rect)
  (let-foreign-color (c color)
    (let-foreign-rect (r rect)
      (api:draw-rectangle r c))))

;;Other drawers
(defun draw-standard-frame(rect)
  (awith (ui:static-gramma #\# (color :gray))
    (dotimes (i (x (size rect)))
      (draw-simple-gramma it (add (start rect) (make-pos i 0)))
      (draw-simple-gramma it (sub (end rect) (make-pos i 0))))))

(defun draw-scroll-bar(scrollable start)
  (awith (static-gramma #\* (color :green))
    (draw-simple-gramma it
			(make-pos (1- (+ (x start) (x (-> scrollable size))))
				  (+ (y start) (round (* (y (-> scrollable size)) (/ (-> scrollable index) (length (-> scrollable sequence))))))))))
