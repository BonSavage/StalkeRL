(in-package :asdf-user)

(defsystem "roguelike"
  :description "A roguelike build system."
  :version "0.1.1"
  :depends-on ("cffi" "BonSavage")
  :components ((:file "packages")
	       (:file "sdl")
	       (:file "coordinates" :depends-on ("packages"))
	       (:file "random")
	       (:file "map" :depends-on ("coordinates" "ui"))
	       (:file "terrain" :depends-on ("map" "entity-defs"))
	       (:file "ui" :depends-on ("coordinates" "sdl"))
	       (:file "ui-dsl" :depends-on ("ui" "perception"))
	       (:file "panels" :depends-on ("ui-dsl" "entity-defs"))
	       (:file "entity" :depends-on ("coordinates" "map" "ui" "random"))
	       (:file "entity-defs" :depends-on ("entity" "coordinates" "perception" "ui"))
	       (:file "perception" :depends-on ("coordinates" "level" "ui"))
	       (:file "event" :depends-on ("entity-defs" "panels" "coordinates"))
	       (:file "control" :depends-on ("event"))
	       (:file "level" :depends-on ("map" "entity"))
	       (:file "generator" :depends-on ("level" "terrain" "random"))))
