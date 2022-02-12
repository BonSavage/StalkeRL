(ql:quickload :cffi)

(cffi:defcstruct color
  (red :uint8)
  (green :uint8)
  (blue :uint8))

(cffi:defcstruct gramma
  (char :uint8)
  (red :uint8)
  (green :uint8)
  (blue :uint8))

(cffi:defcstruct cpos
  (x :uint8)
  (y :uint8))

(cffi:load-foreign-library "c:/roguelike/roguelike/roguelike/RL_SDL/bin/release/RL_SDL.dll")

(cffi:defcfun ("initialize") :void)
(initialize)

(cffi:defcfun ("draw_gramma") :void
  (gramma (:pointer (:struct gramma)))
  (pos (:pointer (:struct cpos))))

(cffi:defcfun ("fill_cell") :void
  (color (:pointer (:struct color)))
  (pos (:pointer (:struct cpos))))

(cffi:defcfun ("clear") :void)
(cffi:defcfun ("present") :void)
(cffi:defcfun ("get_key_event") :int)

(defun draw-test(i j)
  (cffi:with-foreign-object (pos '(:struct cpos))
    (cffi:with-foreign-slots ((x y) pos (:struct cpos))
      (cffi:with-foreign-object (gr '(:struct gramma))
	(cffi:with-foreign-slots ((char red green blue) gr (:struct gramma))
	  (cffi:with-foreign-object (color '(:struct color))
	    (psetf char (char-code #\@) red 100 green 100 blue 100)
	    (cffi:with-foreign-slots ((red green blue) color (:struct color))
	      (psetf red 200 green 200 blue 200)
	      (dotimes (i i)
		(psetf x i)
		(dotimes (j j)
		  (psetf y j)
		  (fill-cell color pos)
		  (draw-gramma gr pos)
		  )))))))))

(defun present-test()
  (dotimes (i 10)
    (clear)
    (draw-test 10 10)
    (present)
    (get-key-event)))
