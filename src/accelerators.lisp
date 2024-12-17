(in-package :climish)

(defaccelerator "Next Line" #k"DownArrow")
(defaccelerator "Next Line" #k"Ctrl-n")
(defaccelerator "Previous Line" #k"UpArrow")
(defaccelerator "Previous Line" #k"Ctrl-p")
(defaccelerator "Enter" #k"Enter")
(defaccelerator "Save File" #k"C-x" #k"C-s")
(defaccelerator "Beginning of Line" #k"Home")
(defaccelerator "Beginning of Line" #k"Ctrl-a")
(defaccelerator "Beginning of Line" #k"Home")
(defaccelerator "Beginning of Line" #k"Ctrl-a")
(defaccelerator "End of Line" #k"End")
(defaccelerator "End of Line" #k"Ctrl-e")
(defaccelerator "End of Line" #k"End")
(defaccelerator "End of Line" #k"Ctrl-e")

(defaccelerator "Forward Character" #k"RightArrow")
(defaccelerator "Forward Character" #k"Ctrl-f")

(defaccelerator "Backward Character" #k"LeftArrow")
(defaccelerator "Backward Character" #k"Ctrl-b")

(defaccelerator "Backward Character" #k"LeftArrow")
(defaccelerator "Backward Character" #k"Ctrl-b")

(defaccelerator "Transpose Characters" #k"Ctrl-t")

(defaccelerator "Delete Next Character" #k"Delete")
(defaccelerator "Delete Next Character" #k"Ctrl-d")

(defaccelerator "Delete Previous Character" #k"Backspace")
(defaccelerator "Echo Area Delete Previous Character" #k"Backspace")

(defaccelerator "Forward Word" #k"Meta-f")

(defaccelerator "Backward Word" #k"Meta-b")

(defaccelerator "Find File" #k"Ctrl-x" #k"Ctrl-f")

(defaccelerator "Extended Command" #k"Meta-x")

(defaccelerator "Mousewheel Scroll" #k"MouseWheel")

(defaccelerator "Beginning of Buffer" #k"Ctrl-Home")
(defaccelerator "End of Buffer" #k"Ctrl-End")
(defaccelerator "Scroll Window Up" #k"PageUp")
(defaccelerator "Scroll Window Down" #k"PageDown")

(defaccelerator "Kill Line" #k"Ctrl-k")

(defaccelerator "Kill Next Word" #k"Meta-d")

(defaccelerator "Kill Previous Word" #k"Meta-Backspace")

(defaccelerator "Forward Kill Form" #k"Ctrl-Meta-k")
(defaccelerator "Backward Kill Form" #k"Ctrl-Meta-Backspace")
(defaccelerator "Beginning of Defun" #k"Ctrl-Meta-a")
(defaccelerator "End of Defun" #k"Ctrl-Meta-e")
(defaccelerator "Forward List" #k"Ctrl-Meta-n")
(defaccelerator "Backward List" #k"Ctrl-Meta-p")
(defaccelerator "Forward Form" #k"Ctrl-Meta-f")
(defaccelerator "Backward Form" #k"Ctrl-Meta-b")
(defaccelerator "Mark Form" #k"Ctrl-Meta-Spacebar")
(defaccelerator "Mark Defun" #k"Ctrl-Meta-h")
(defaccelerator "Extract List" #k"Ctrl-Meta-x")
(defaccelerator "Indent Form" #k"Ctrl-Meta-q")

(defaccelerator "Indent Or Complete" #k"Tab")
(defaccelerator "Indent" #k"Tab")

(defaccelerator "Complete Field" #k"Tab")

(defaccelerator "Lisp Insert )" #k"Shift-0")
(defaccelerator "Insert ()" #k"Shift-Meta-9")
(defaccelerator "Move over )" #k"Shift-Meta-0")

(defaccelerator "Yank" #k"Ctrl-y")
(defaccelerator "Yank" #k"Ctrl-y")

(defaccelerator "Query Replace" #k"Shift-Meta-5")
(defaccelerator "Reverse Incremental Search" #k"Ctrl-r")
(defaccelerator "Incremental Search" #k"Ctrl-s")
(defaccelerator "Forward Search" #k"Meta-s")
(defaccelerator "Reverse Search" #k"Meta-r")

;;(defaccelerator "Universal Argument" #k"Ctrl-u")


;;(defaccelerator "Help" #k"Ctrl-h")
;;(defaccelerator "Help" #k"Shift-Ctrl-\-")
;;(defaccelerator "Help" #k"F1")

;;(defaccelerator "Describe Key" #k"Shift-Meta-/")
;;(defaccelerator "Describe Key" #k"Shift-Meta-/")

;;(defaccelerator "Top Line to Here" #k"RightMouse")


(defaccelerator "Generic Pointer Up" #k"LeftMouse_Up")
(defaccelerator "Generic Pointer Up" #k"RightMouse_Up")
(defaccelerator "Generic Pointer Up" #k"MiddleMouse_Up")


;;(defaccelerator "Generic Pointer Move" #k"MouseMove")

(defaccelerator "Point to Here" #k"LeftMouse")

#+NOTYET(defaccelerator "Insert Kill Buffer" #k"Super-RightMouse")

(defaccelerator "Insert File" #k"Ctrl-x" #k"Ctrl-r")
(defaccelerator "Visit File" #k"Ctrl-x" #k"Ctrl-v")
(defaccelerator "Write File" #k"Ctrl-x" #k"Ctrl-w")
(defaccelerator "Backup File" #k"Ctrl-x" #k"Meta-b")
(defaccelerator "Save All Files" #k"Ctrl-x" #k"s")
(defaccelerator "Save All Files and Exit" #k"Ctrl-x" #k"Ctrl-c")

(defaccelerator "Lisp Listener" #k"Ctrl-x" #k"l")
(defaccelerator "Options" #k"RightMouse")


;;(defaccelerator "Bufed" #k"Ctrl-x" #k"Ctrl-b")
(defaccelerator "Buffer Not Modified" #k"Shift-Meta-`")
(defaccelerator "Check Buffer Modified" #k"Ctrl-x" #k"Shift-`")
(defaccelerator "Select Buffer" #k"Ctrl-x" #k"b")
(defaccelerator "Select Previous Buffer" #k"Ctrl-Meta-l")
(defaccelerator "Circulate Buffers" #k"Shift-Ctrl-Meta-l")
(defaccelerator "Create Buffer" #k"Ctrl-x" #k"Meta-b")
(defaccelerator "Kill Buffer" #k"Ctrl-x" #k"k")
;;(defaccelerator "Select Random Typeout Buffer" #k"Hyper-t")

(defaccelerator "Cycle Windows" #k"Ctrl-Tab")
(defaccelerator "Cycle Windows" #k"Ctrl-x" #k"o")

(defaccelerator "Reverse Cycle Windows" #k"Shift-Ctrl-Tab")

(defaccelerator "Cycle Windows" #k"Ctrl-Tab")
(defaccelerator "Cycle Windows" #k"Ctrl-x" #k"o")

(defaccelerator "Reverse Cycle Windows" #k"Shift-Ctrl-Tab")

;;(defaccelerator "Exit Recursive Edit" "Ctrl-Meta-z")

(defaccelerator "Interrupt Lisp" #k"Ctrl-c" #k"Ctrl-c")

(defaccelerator "Iconify Window" #k"Ctrl-x" #k"0")
(defaccelerator "Iconify Window" #k"Ctrl-x" #k"0")

(defaccelerator "Set/Pop Mark" #k"Ctrl-Spacebar")
(defaccelerator "Save Region" #k"Meta-w")
(defaccelerator "Set/Pop Mark" #k"Ctrl-Spacebar")
(defaccelerator "Save Region" #k"Meta-w")

(defaccelerator "Editor Error Test" #k"Ctrl-x" #k"Ctrl-e")
(defaccelerator "Debugger Test" #k"Pause")


(defaccelerator "Dired" #k"Ctrl-x" #k"Ctrl-Meta-d")

(defaccelerator "Dired Delete File and Down Line" #k"d")
(defaccelerator "Dired Delete File with Pattern" #k"Shift-d")
(defaccelerator "Dired Delete File" #k"Ctrl-d")
(defaccelerator "Dired Delete File" #k"Delete")
(defaccelerator "Dired Delete File" #k"k");;

(defaccelerator "Dired Undelete File and Down Line" #k"u")
(defaccelerator "Dired Undelete File with Pattern" #k"Shift-u")
(defaccelerator "Dired Undelete File" #k"Ctrl-u")

(defaccelerator "Dired Expunge Files" #k"Shift-1")
(defaccelerator "Dired Update Buffer" #k"Hyper-u")
(defaccelerator "Dired Edit File" #k"Spacebar")
(defaccelerator "Dired Edit File" #k"e")
(defaccelerator "Dired Edit File" #k"Enter")
(defaccelerator "Dired Edit File" #k"Ctrl-m")
(defaccelerator "Dired Up Directory" #k"Shift-6")
(defaccelerator "Dired Quit" #k"q")
;;(defaccelerator "Dired Help" #k"Shift-/")
;;(defaccelerator "Dired Help" #k"F1")

(defaccelerator "Dired Copy File" #k"c")
(defaccelerator "Dired Copy with Wildcard" #k"Shift-c")
(defaccelerator "Dired Rename File" #k"r")
(defaccelerator "Dired Rename with Wildcard" #k"Shift-r")

;;(defaccelerator "Next Line" #k"n")
;;(defaccelerator "Previous Line" #k"p")

(defaccelerator "Editor Compile Defun" #k"Ctrl-c" #k"Ctrl-c")
(defaccelerator "Editor Compile Buffer File"  #k"Ctrl-c" #k"Ctrl-k")

	       
					
     
	   
(defaccelerator "Yank From Lisp Listener History" #k"Meta-p")
(defaccelerator "Yank Next From Lisp Listener History" #k"Meta-n")
(defaccelerator "Interrupt Lisp" #k"Ctrl-c" #k"Ctrl-c")

(setf (logical-key-event-p #k"c-s" :forward-search) t)
(setf (logical-key-event-p #k"c-r" :backward-search) t)
(setf (logical-key-event-p #k"c-r" :recursive-edit) t)
(setf (logical-key-event-p #k"delete" :cancel) t)
(setf (logical-key-event-p #k"backspace" :cancel) t)
(setf (logical-key-event-p #k"c-g" :abort) t)
(setf (logical-key-event-p #k"escape" :exit) t)
(setf (logical-key-event-p #k"y" :yes) t)
(setf (logical-key-event-p #k"Spacebar" :yes) t)
(setf (logical-key-event-p #k"n" :no) t)
(setf (logical-key-event-p #k"backspace" :no) t)
(setf (logical-key-event-p #k"delete" :no) t)
(setf (logical-key-event-p #k"Shift-1" :do-all) t)
(setf (logical-key-event-p #k"." :do-once) t)
(setf (logical-key-event-p #k"c-h" :help) t)
(setf (logical-key-event-p #k"h" :help) t)
(setf (logical-key-event-p #k"Shift-/" :help) t)
(setf (logical-key-event-p #k"Shift-c-\-" :help) t)
(setf (logical-key-event-p #k"Enter" :confirm) t)
(setf (logical-key-event-p #k"c-q" :quote) t)
(setf (logical-key-event-p #k"k" :keep) t)
