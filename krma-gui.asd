(defsystem krma-gui
  :description "A platform independent gui library."
  :depends-on (:clui-with-krma
	       :adhoc
	       :3d-vectors :3d-matrices :3d-quaternions
	       :trivial-gray-streams
	       :bordeaux-threads
	       :iterate
	       :conium
	       :osicat
	       :cl-ppcre)
  :author "Andrew K Wolven <awolven@gmail.com>"
  :license "GPLv3"
  :serial t
  :components
  ((:file "hemlock/package")

   (:file "src/package")
   (:file "src/setup")
   (:file "src/scene-graph")
   (:file "src/metaclasses")
   (:file "src/style")
   (:file "src/views")
   (:file "src/event-queue")
   
   
   (:file "hemlock/lispdep")
   (:file "hemlock/hemlock-ext")
   (:file "hemlock/decls")
   ;;(:file "hemlock/classes")
   (:file "hemlock/struct")
   (:file "hemlock/charmacs")

   (:file "hemlock/macros")

   (:file "src/classes")

   (:file "hemlock/rompsite")
   (:file "hemlock/bit-stuff")

   (:file "hemlock/table")
   (:file "hemlock/line")
   (:file "hemlock/ring")
   (:file "hemlock/htext1")
   
   (:file "hemlock/buffer")
   (:file "hemlock/vars")
   (:file "hemlock/interp")
   (:file "hemlock/syntax")
   (:file "hemlock/htext2")
   (:file "hemlock/htext3")
   (:file "hemlock/htext4")
   (:file "hemlock/files")
   (:file "hemlock/search1")
   (:file "hemlock/search2")

   (:file "hemlock/window")
   (:file "hemlock/cursor")
   (:file "hemlock/screen")
   (:file "hemlock/linimage")
   (:file "hemlock/winimage")

   (:file "hemlock/display")
   (:file "hemlock/exp-syntax")

   (:file "hemlock/kbdmac")
   (:file "hemlock/font")
   (:file "hemlock/streams")

   (:file "hemlock/main-hemlock")
   
   (:file "hemlock/stream")

   (:file "hemlock/echo")
   (:file "hemlock/new-undo")


   (:file "hemlock/hunk-draw")

   (:file "hemlock/echocoms")
   (:file "hemlock/killcoms")
   (:file "hemlock/command")
   
   (:file "hemlock/undo")

   (:file "hemlock/searchcoms")
   (:file "hemlock/filecoms")
   (:file "hemlock/indent")
   (:file "hemlock/grep")
   (:file "hemlock/apropos")
   (:file "hemlock/typeout")
   (:file "hemlock/morecoms")
   (:file "hemlock/doccoms")
   (:file "hemlock/srccom")
   (:file "hemlock/group")
   (:file "hemlock/fill")
   (:file "hemlock/text")

   (:file "hemlock/lispmode")
   (:file "hemlock/eval-server")
   (:file "hemlock/lispbuf")

   (:file "hemlock/defsyn")

   (:file "hemlock/auto-save")

   (:file "hemlock/highlight")
   (:file "hemlock/dired")
   (:file "hemlock/diredcoms")

   (:file "hemlock/ll")
   (:file "hemlock/cmd")

   
   
   (:file "src/gui-application")
   (:file "src/text-editor-view")
   (:file "src/text-editor-window")
   (:file "src/echo-area-window")
   (:file "src/command-tables")
   (:file "src/commands")
   (:file "src/gestures")))
	  
