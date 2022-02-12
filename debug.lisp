(export 'build-info)
(asdf:load-system "roguelike")

;;;; Entry-point from the LISP-REPL
;;;; The first system

;;;;TODO:
;;; 1. More monsters
;;;

(use-package :coordinates)

(defmacro profile(function-name)
  (assert (and (symbol-function function-name)))
  (setf (symbol-function function-name) (awith (symbol-function function-name) (lambda (&rest args) (format t "Profiling: ~a" function-name) (time (apply it args))))))

(defmacro render-test()
  `(panels::game-panel (panels::actor-controller level:*actor*) (entity::do-fov level:*actor*) (entity:get-message-buffer level:*actor*)))

(defmacro do-events(&optional (count 100))
  `(progn
     (dotimes (i ,count)
       (event:process-events))))

(defmacro generate-map()
  `(generator:generate (make-rect (make-pos 4 4) (make-pos 40 40))))

(defmacro zombie-map()
  `(let ((rooms (generate-map)))
     (dolist (r rooms rooms)
       (event:add-event (event:make-turn (entity:make-zombie (x (start (first r))) (y (start (first r)))) 200)))))

(defun light-area(x y &optional (rad 5))
  (map::light-area (map::make-light-source :center (make-pos x y) :radius rad)))

(defun shadow-area(x y &optional (rad 5))
  (map::shadow-area (map::make-light-source :center (make-pos x y) :radius rad)))

(defun add-creature(creature)
  (event:add-event (event:make-turn creature 100)))

(defun spawn-actor(x y &optional (event-count 0))
  (event:add-event (event:make-turn (setf level:*actor* (entity:make-actor x y)) 100))
  (dolist (item (list (entity:make-item 'entity:aksu) (entity:make-item 'entity:knife) (entity:make-item 'entity:pb) (entity:make-stack 'entity:ammo-5.45x39 90) (entity:make-stack 'entity:ammo-9x18 40)))
    (level:add-entity (make-pos x y) item))
  (do-events event-count))

(defun event-loop()
  (with-simple-restart (end "Kill event loop")
    (iter (while (event:process-events)))))

(defun init-function()
  (load "init.lisp"))

(defconstant +build-info+ "Debug version.")

(defun build-info()
  +build-info+)

(defun build(&optional version-note)
  (mvb (second minute hour day month year) (get-decoded-time)
       (defconstant +build-info+ (formatted "~a~&Build date: ~a:~a:~a, ~a.~a.~a" (or version-note "") hour minute second day month year))
       (gc)
       (ext:saveinitmem "stalkerl" :init-function #'init-function :executable t)))
