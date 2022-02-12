(in-package :panels)
;;;;GUI panels

;;;Controllers

(defmacro dir-controller(((x y) &rest forms) &body key2expr)
  (alexandria:with-gensyms (mv)
    `(flet ((,mv (,x ,y) ,@forms))
       (controller
	(#\9 (,mv 1 -1))
	(#\8 (,mv 0 -1))
	(#\7 (,mv -1 -1))
	(#\6 (,mv 1 0))
	(#\5 (,mv 0 0))
	(#\4 (,mv -1 0))
	(#\3 (,mv 1 1))
	(#\2 (,mv 0 1))
	(#\1 (,mv -1 1))
	,@key2expr))))

(defun actor-controller(actor fov-info)
  (dir-controller ((x y) (event:interact-with-cell! actor (make-pos x y)))
    (#\x (lookup actor fov-info))
    (#\i (change-layer (inventory actor)))
    (#\m (change-layer (book (catalogue (ui:get-printed (entity:get-message-buffer actor))))))
    (#\f (fire actor fov-info))
    (#\e (actor-equipment actor))
    (#\c (event:close-near! actor fov-info))
    (#\r (event:reload-weapon! actor))
    (#\p (event:pickup-items! actor))
    (#\z (event:switch-weapon! actor))
    (#\. (with-simple-restart (done "Continue game")
	   (progn
	     (message (text (list (str "Debugger invoked. Use console to interact with it."))))
	     (invoke-debugger "Interactive debugger called."))))))

(defun dummy-controller()
  (controller
   (t 'exit)))

(defmethod lookup-controller((fov ui:lookup-fov))
  (dir-controller ((x y) (move-focus fov (make-pos x y)))
		  (#\Escape 'exit)))

(defmethod target-controller((fov ui:target-fov) actor)
  (let* ((weapon (entity:get-weapon actor))
	 (fire-modes (entity:get-fire-modes weapon)))
    (dir-controller ((x y) (move-focus fov (make-pos x y)))
      (#\0 (progn
	     (ui:add-note (entity:get-message-buffer actor) (formatted "Fire mode: ~a" (or (second fire-modes) (first fire-modes))))
	     (psetf fire-modes (cdr fire-modes))))
      (#\f (event:perform-shot! actor (get-map-focus fov) (first fire-modes)))
      (#\Tab (next-target fov))
      (#\Escape 'exit))))

(defun close-controller(actor)
  (dir-controller ((x y)
		   (aif (event:try-to-close! actor (make-pos x y)) it 'exit))
    (#\Escape 'exit)))

(defun lookup(actor fov)
  (awith (make-lookup-fov fov)
	 (change-layer (panels:game-panel actor (lookup-controller it) it (fov-description it)))))

(defun fire(actor fov)
  (when (entity:can-shoot-p actor)
    (awith (make-target-fov fov actor)
	   (change-layer (game-panel actor (target-controller it actor) it (sources-append (buffer-source (entity:get-message-buffer actor))
											   (fov-description it)))))))
;;;Presenters

(defmacro framed-fields(&rest forms)
  `(views
     (field (rect (0 0) (79 24)) (standard-frame))
     ,@forms))

(defun item-select(line)
  (line (first line) (string-select (second line))))

(defun item-label(item len &key (item-name #'entity:get-name))
  (line (entity:get-gramma item) (str (word-capitalize (funcall item-name item)))))

(defun item-description(item)
  (line (if item
	    (str (entity:get-description item))
	    (str "<none>"))))

(define-view item-slot
  #'item-label
  #'item-select)

(defun item-action-name(action)
  (cdr (assoc action `((,#'event:drop-items! . "Drop")
		       ))))

(define-view item-action
    (lambda (act len) (str (item-action-name act))) ;It is better to use :selected key
  (lambda (str) (line (string-select str))))

(define-view equipment-slot
    (lambda (slot len) (list* (str (formatted "~a:"(string-downcase (symbol-name (entity:slot-category slot))))) (aif (entity:slot-stack slot) (item-label it 10) (list (str "<none>")))))
  (lambda (line) (list* (string-select (first line)) (rest line))))

(defun quit-game()
  (quit))

(defun start-game()
  (generator:generate (make-rect (make-pos 4 4) (make-pos 40 40)))
  (loop
     (event:process-events)))

(define-panel main-menu()
  :named
  ((menu (simple-menu ((str "Start game") (change-layer (start-game)))
		      ((str " Quit game") (quit-game)))))
  :drawable
  (framed-fields
   (field (rect (50 15) (61 20)) (framed (color :gray) (view-instance menu)))
   (field (rect (20 5) (40 10)) (page (text (list (str "StalkeRL v. 0.1")))))
   (field (rect (0 20) (40 30)) (page (text (list (str (build-info)))))))
  :controller
  (call-handler menu))

(defun actor-stats(actor)
  (text (list (str (formatted "HP: ~4@a/~5a" (entity:get-hp actor) (entity:get-max-hp actor)) (color :crimson))
	      (str " Armor: " (color :blue)) (str (formatted "~5a" 0))
	      (str "Speed: ") (str (formatted "~5a" (entity:get-speed actor)))
	      (str "Item: ") (aif (entity:get-weapon actor) (str (entity:get-full-name it)) (str "<none>")))))

(define-panel game-panel(actor controller fov-info message-source)
  :named
  ((log (buffer message-source)))
  :drawable
  (views
   (field (rect (0 5) (79 23)) (framed (color :gray) (instance fov-info)))
   (field (rect (0 0) (79 4)) (framed (color :gray) (view-instance log)))
   (field (rect (0 23) (79 26)) (framed (color :gray) (page (actor-stats actor)))))
  :controller
  (when (call-handler log)
    (control controller)))

(define-panel message(string-content)
  :named ((content (scrollable string-content
			       (lambda (dependent)
				 (awith (api:get-key-event)
				   (controller-body it
						    (#\Escape 'exit)
						    (t (control-scrollable dependent it))))))))
  :drawable
  (field (rect (30 10) (50 20)) (framed (cell-color :default) (view-instance content)))
  :controller
  (handle content))

(define-panel book(string-content)
  :named
  ((content (scrollable string-content
			(lambda (dependent)
			  (awith (api:get-key-event)
				 (controller-body it
						  (#\Escape 'exit)
						  (t (control-scrollable dependent it))))))))
  :drawable
  (field (rect (0 0) (79 24)) (standard-frame (view-instance content :from-end t :scroll-bar t)))
  :controller
  (handle content))

(define-panel inventory(creature &aux (slots (entity:get-backpack (entity:get-inventory creature))))
  :named
  ((menu (alphabetic-menu slots item-slot
			  :action (lambda (index) (item-panel (elt slots index) creature)))))
  :drawable
  (field (rect (0 0) (79 24))
	 (standard-frame (horizontal-split (framed (color :gray) (view-instance menu))
					   (framed (color :gray) (page (selected-description menu #'item-description))))))
  :controller
  (handle menu))

(define-panel item-panel(stack creature &aux (actions (event:item-actions stack creature)))
  :named
  ((menu (alphabetic-menu actions item-action
			  :action (lambda (index) (funcall (elt actions index) creature stack)))))
  :drawable
  (field (rect (30 10) (50 20)) (framed (cell-color :default) (background (color :black) (view-instance menu))))
  :controller
  (handle menu))

(define-panel item-choose(items)
  :named
  ((menu (alphabetic-menu items item-slot
			  :action (lambda (index) (elt items index)))))
  :drawable
  (field (rect (30 10) (50 20)) (framed (cell-color :default) (view-instance menu)))
  :controller
  (handle menu))

(define-panel actor-equipment(actor &aux (slots (entity:equipment-slots (entity:get-gear actor))))
  :named
  ((gear (alphabetic-menu slots equipment-slot
			  :action (lambda (index) (event:use-slot! actor (elt slots index))))))
  :drawable
  (field (rect (20 5) (60 16)) (horizontal-split (framed (cell-color :default) (view-instance gear))
						 (framed (cell-color :default) (page (selected-description gear (lambda (slot) (item-description (entity:slot-stack slot))))))))
  :controller
  (handle gear))

(define-panel frame-test()
  :drawable
  (field (rect (20 0) (40 20)) (framed (color :gray) (page (text (list (str "Abcd"))))))
  :controller
  (controller-let
   (#\q 'exit)))
