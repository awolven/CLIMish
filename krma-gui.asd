(defsystem krma-gui
  :description "A platform independent gui library."
  :depends-on (:krma
	       :adhoc
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
   (:file "src/input-event-ids")

   
   
   (:file "src/hemlock-core/lispdep")
   (:file "src/hemlock-core/hemlock-ext")
   (:file "src/hemlock-core/decls")
   ;;(:file "src/hemlock-core/classes")
   (:file "src/hemlock-core/line")
   (:file "src/hemlock-core/struct")
   (:file "src/hemlock-core/charmacs")

   (:file "src/command-generic-function")
   (:file "src/hemlock-core/macros")

   (:file "src/hemlock-core/table")
   (:file "src/classes")
   (:file "src/hemlock-core/vars")


   (:file "src/hemlock-core/key-event")
   (:file "src/hemlock-core/rompsite")
   (:file "src/hemlock-core/bit-stuff")



   (:file "src/hemlock-core/ring")
   (:file "src/hemlock-core/htext1")

   (:file "src/hemlock-core/window")
   (:file "src/hemlock-core/buffer")

   (:file "src/hemlock-core/interp")
   (:file "src/event-queue")

   (:file "src/hemlock-core/htext2")
   (:file "src/hemlock-core/htext3")
   (:file "src/hemlock-core/htext4")
   (:file "src/hemlock-core/files")
   (:file "src/hemlock-core/search1")
   (:file "src/hemlock-core/search2")


   (:file "src/hemlock-core/cursor")
   #+NIL(:file "src/hemlock-core/screen")

   (:file "src/hemlock-core/linimage")
      (:file "src/hemlock-core/syntax")
   (:file "src/hemlock-core/winimage")



   #+NIL(:file "src/hemlock-core/display")


   (:file "src/hemlock-core/kbdmac")
   (:file "src/hemlock-core/font")
   (:file "src/hemlock-core/streams")

   (:file "src/hemlock-core/main-hemlock")
   
   (:file "src/hemlock-core/stream")

   (:file "src/hemlock-core/echo")
   (:file "src/hemlock-core/new-undo")


   #+NIL(:file "src/hemlock-core/hunk-draw")

   (:file "src/hemlock-core/echocoms")
   (:file "src/hemlock-core/killcoms")
   (:file "src/hemlock-core/command")
   
   (:file "src/hemlock-core/undo")

   (:file "src/hemlock-core/searchcoms")
   (:file "src/hemlock-core/filecoms")
   (:file "src/hemlock-core/indent")
   (:file "src/hemlock-core/grep")
   (:file "src/hemlock-core/apropos")
   (:file "src/hemlock-core/typeout")
   (:file "src/hemlock-core/morecoms")
   (:file "src/hemlock-core/doccoms")
   (:file "src/hemlock-core/srccom")
   (:file "src/hemlock-core/group")
   (:file "src/hemlock-core/fill")
   (:file "src/hemlock-core/text")



   (:file "src/hemlock-core/lispmode")
   (:file "src/hemlock-core/eval-server")
   (:file "src/hemlock-core/lispbuf")

   (:file "src/hemlock-core/defsyn")

   (:file "src/hemlock-core/auto-save")

   (:file "src/hemlock-core/highlight")
   (:file "src/hemlock-core/dired")
   (:file "src/hemlock-core/diredcoms")

   (:file "src/hemlock-core/ll")
   (:file "src/hemlock-core/cmd")

   
   (:file "src/kill-ring")
   (:file "src/text-editor-view")
   (:file "src/text-editor-window")
   (:file "src/popup-stream")
   (:file "src/hemlock-core/exp-syntax")
   (:file "src/echo-area-window")
   ;;(:file "src/command-tables")
   (:file "src/commands")
   (:file "src/debugger")
   (:file "src/context-menu")
   (:file "src/gui-application")
   (:file "src/hemlock-core/completion")
   (:file "src/accelerators")
   ;;(:file "src/gestures")
   ))
	  
