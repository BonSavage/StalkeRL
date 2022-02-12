(in-package :api)
(defvar *loaded* cl:nil)

(unless *loaded*
  (setf *loaded* t)
  (load-foreign-library "rl_sdl.dll")
  (defcfun ("initialize") :void)
  (initialize))
