(defsystem krma-gui
  :description "A platform independent gui library."
  :depends-on (:adhoc :clui-with-krma :3d-quaternions)
  :author "Andrew K Wolven <awolven@gmail.com>"
  :license "GPLv3"
  :serial t
  :components
  ((:file "package")
   (:file "setup")
   (:file "scene-graph")
   (:file "views")
   (:file "event-queue")))
