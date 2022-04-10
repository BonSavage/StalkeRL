

(defpackage :c-api
  (:documentation "API to use C functions")
  (:use :cffi :cl-user)
  (:nicknames :api :sdl)
  (:shadowing-import-from :cffi :foreign-pointer)
  (:export :key-to-event :get-key-event :process-key :render-present :render-clear
	   :cpos :color :rect :gramma :x :y :red :green :blue :char :start :size
   :draw-gramma :fill-cell
   :draw-frame :draw-rectangle
   :with-foreign-temporary))

(defpackage :random
  (:nicknames :rnd)
  (:use :cl-user :cl)
  (:export :dices :dices-string :throw-dices :bernoulli :fbernoulli))

(defpackage :coordinates
  (:use :cl-user :common-lisp)
  (:export :raw-line :cell-line :trace-line :cast-ray :doray :cell-find-if
   :coordinates :pos :rect :start :end :size :x :y :make-pos :add :sub :make-rect :distance-point
   :right-slice :left-slice :upper-slice :lower-slice :transpose :negate
	   :pref
   :in-bound-p
	   :distance :pythagorean-distance
   :neighbours :neighbours-delta
	   :doarea :rect-scale
	   :dorectangle))

(defpackage :entity
  (:use :common-lisp :cl-user :coordinates)
  (:export :entity :creature :corpse :item-stack :trap :anomaly :firearm :melee :can-move-p :step-to :activep
	   :get-pos :get-hp :get-max-hp :get-speed :get-damage :alivep :seesp :can-shoot-p :dodges
	   :damage-count
	   :actor
   :corpse-owner
   :get-message-buffer :update-fov
   :get-gramma :get-name :get-concrete-name :get-full-name :get-description :get-weapon :get-damage
	   :make-stack :make-item :get-melee-damage :get-ranged-damage :get-reload-time :get-fire-modes :get-recoil-coeff
   :get-inventory :get-backpack :get-slots
	   :slot-stack :slot-category
	   :get-gear :equipment-slots
   :get-memory
   :msg :snd
   :simulate-message :simulate-sound :simulate-noise
   :simple-message :simple-note
   :report)
  (:intern :melee-attack :perform-movement :pickup-items :take-damage :hit :add-stack :remove-stack :update-trap
	   :slot-equip :put-off-slot :swap-slots :perform-reload :take-shot :message)) ;entity classes are exported dynamically

(defpackage :map
  (:use :common-lisp :cl-user :coordinates)
  (:export  :make-map-array :+size+ :+map-rect+ :in-map-bound-p :grant-on-map :map-array
	    :pos-gramma :pos-name :obstaclep :solidp :blockedp :litp
	    :door :grate-door :openp :open-door :close-door
   :search-terrain :attack-cell :destroyedp
   :interact
   :decorate-terrain :blood :moss :liana
   :shadowcast
   :replace-light
   :create-light
   :cell-pos)
  (:intern :terrain :floor :wall :tunnel :limit :trash :horizontal-railtrack :vertical-railtrack :*map*))

(defpackage :level
  (:use :common-lisp :cl-user :coordinates :map)
  (:shadow :get-gramma)
  (:export :*actor* :make-room-list :get-entities :add-entity :remove-entity
	   :get-gramma :obstaclep :solidp
	   :check-traps :do-entities)
  (:import-from :entity :entity))

(defpackage :generator
  (:use :common-lisp :cl-user :coordinates :level :map)
  (:export :make-room-list :add-room :generate)
  (:import-from :map :terrain :floor :wall :tunnel :limit :door :trash :horizontal-railtrack :vertical-railtrack :*map*))

(defpackage :event
  (:use :cl-user :entity :coordinates :cl)
  (:export
   :take-turn
   :try-to-move! :try-to-close! :add-event :make-turn :make-thunk-event :process-events :interact-with-cell! :pickup-items! :drop-items! :equip-item! :use-slot!
   :item-actions :switch-weapon! :reload-weapon! :perform-shot! :close-near! :move-random! :melee-attack!)
  (:import-from :entity :melee-attack :perform-movement :pickup-items :take-damage :add-stack :remove-stack :update-trap :slot-equip :put-off-slot :swap-slots :perform-reload :take-shot))

(defpackage :creature-control
  (:nicknames :ai)
  (:export :make-movement-memory :report-enemy-position)
  (:use :event :entity :coordinates :cl :cl-user))

(defpackage :user-interface
  (:use :common-lisp :cl-user :coordinates)
  (:nicknames :ui)
  (:import-from :cl-user :def-symbol-map)
  (:export
   :message-buffer :add-message :add-note :pop-messages :make-message-buffer :get-printed :pop-unprinted :buffer-source
   :static-gramma
   :color :make-color
   :gramma :gramma-code :make-gramma :augment-gramma :gramma-color
   :cell-color :layer-color
   :gui-string :make-gui-string
   ;;DSL
   :change-layer
   :define-panel
   :lookup-fov :make-lookup-fov :make-target-fov :fov-description :move-focus :target-fov :get-map-focus :next-target
   :field :rect :pos :str :line 
   :page :catalogue :text
   :views :framed :background :standard-frame
   :call-handler :controller-let :controller :controller-body :control :control-scrollable :exit
   :handle
   :end-reached-p :last-page-p
   :scrollable :simple-menu :menu :alphabetic-menu :buffer
   :define-view
   :string-select
   :view-instance :selected-description :instance
   :horizontal-split :vertical-split
   :text-source :sources-append)
  (:intern :draw-map :draw :make-scrollable :draw-terrain :get-marked-list))

(defpackage perception ;Mix of UI and entity
  (:use :common-lisp :cl-user :coordinates :entity :ui :map)
  (:export :fov :fov-info :make-fov :update-fov :get-gramma :get-plan-gramma :visiblep :seenp :get-center :get-fov :fov-entity-positions :visible-creatures
   :lee-map :make-lee-map :lee :near-step :way-distance
   :movement-memory :set-movement :update-movement :need-reset-p
   :vision-visiblep :fov-pos-description)
  (:shadow :get-gramma :update-fov))

(defpackage :panels
  (:use :ui :cl-user :common-lisp :coordinates)
  (:export :main-menu :game-panel :actor-controller :map-drawer :dummy-controller :item-choose :close-controller :change-layer))

(shadowing-import 'str :user-interface)

