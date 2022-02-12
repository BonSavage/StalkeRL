(in-package :api)

(eval-when (:load-toplevel)
  (load "sdl-init.lisp"))

(defcstruct color
  (red :uint8)
  (green :uint8)
  (blue :uint8))

(defcstruct gramma
  (char :uint8)
  (red :uint8)
  (green :uint8)
  (blue :uint8))

(defcstruct cpos
  (x :uint8)
  (y :uint8))

(defcstruct rect
  (start (:struct cpos))
  (size (:struct cpos)))

(defcfun ("draw_gramma") :void
  (gramma (:pointer (:struct gramma)))
  (pos (:pointer (:struct cpos))))

(defcfun ("fill_cell") :void
  (color (:pointer (:struct color)))
  (pos (:pointer (:struct cpos))))

(defcfun (render-clear "clear") :void)
(defcfun (render-present "present") :void)
(defcfun ("get_key_event") :int)
(defcfun ("key_to_event") :int (key :int))
(defcfun ("draw_rectangle") :void
  (rect (:pointer (:struct rect)))
  (color (:pointer (:struct color))))
(defcfun ("draw_frame") :void
  (rect (:pointer (:struct rect)))
  (color (:pointer (:struct color))))

(defun draw-test(i j)
  (with-foreign-object (pos '(:struct cpos))
    (with-foreign-slots ((x y) pos (:struct cpos))
      (with-foreign-object (gr '(:struct gramma))
	(with-foreign-slots ((char red green blue) gr (:struct gramma))
	  (with-foreign-object (color '(:struct color))
	    (psetf char (char-code #\@) red 100 green 100 blue 100)
	    (with-foreign-slots ((red green blue) color (:struct color))
	      (psetf red 200 green 200 blue 200)
	      (dotimes (i i)
		(psetf x i)
		(dotimes (j j)
		  (psetf y j)
		  (fill-cell color pos)
		  (draw-gramma gr pos)
		  )))))))))

(defun present-test()
  (clear)
  (draw-test 80 30)
  (present)
  (get-key-event))

(defmacro with-foreign-temporary((name type &rest slots) &body forms)
  `(cffi:with-foreign-object (,name ',type)
     (macrolet ,(mapcar (lambda (slot-name) `(,slot-name (object) (foreign-slot-value ,object ,type ,slot-name))) slots)
       ,@forms)))
