(in-package :climish)

(defun print-pretty-gesture (gesture stream)
  (loop for keys on gesture
	do (print-pretty-input-event-id (car keys) stream)
	   (when (cdr keys)
	     (princ " " stream)))
  gesture)

(defun hemlock-ext::print-pretty-key (thing stream)
  (print-pretty-gesture thing stream))

(setf (hi::logical-key-event-p #k"Ctrl-s" :forward-search) t)
(setf (hi::logical-key-event-p #k"Ctrl-r" :backward-search) t)
(setf (hi::logical-key-event-p #k"Ctrl-r" :recursive-edit) t)
(setf (hi::logical-key-event-p #k"Delete" :cancel) t)
(setf (hi::logical-key-event-p #k"Backspace" :cancel) t)
(setf (hi::logical-key-event-p #k"Ctrl-g" :abort) t)
(setf (hi::logical-key-event-p #k"Escape" :exit) t)
(setf (hi::logical-key-event-p #k"y" :yes) t)
(setf (hi::logical-key-event-p #k"Spacebar" :yes) t)
(setf (hi::logical-key-event-p #k"n" :no) t)
(setf (hi::logical-key-event-p #k"Backspace" :no) t)
(setf (hi::logical-key-event-p #k"Delete" :no) t)
(setf (hi::logical-key-event-p #k"Shift-1" :do-all) t)
(setf (hi::logical-key-event-p #k"." :do-once) t)
(setf (hi::logical-key-event-p #k"Ctrl-h" :help) t)
(setf (hi::logical-key-event-p #k"h" :help) t)
(setf (hi::logical-key-event-p #k"Shift-/" :help) t)
(setf (hi::logical-key-event-p #k"Shift-Ctrl-\-" :help) t)
(setf (hi::logical-key-event-p #k"Enter" :confirm) t)
(setf (hi::logical-key-event-p #k"Ctrl-q" :quote) t)
(setf (hi::logical-key-event-p #k"k" :keep) t)

(define-gesture "Next Line" 'text-editor-window-mixin #k"DownArrow")
(define-gesture "Next Line" 'text-editor-window-mixin #k"Ctrl-n")
(define-gesture "Previous Line" 'text-editor-window-mixin #k"UpArrow")
(define-gesture "Previous Line" 'text-editor-window-mixin #k"Ctrl-p")
(define-gesture "Insert Newline" 'text-editor-window-mixin #k"Enter")
(define-gesture "Indent New Line" "Lisp" #k"Enter")
(define-gesture "Save File" 'text-editor-window-mixin #k"C-x" #k"C-s")
(define-gesture "Beginning of Line" 'text-editor-window-mixin #k"Home")
(define-gesture "Beginning of Line" 'text-editor-window-mixin #k"Ctrl-a")
(define-gesture "Beginning of Line" 'echo-area-window-mixin #k"Home")
(define-gesture "Beginning of Line" 'echo-area-window-mixin #k"Ctrl-a")
(define-gesture "End of Line" 'text-editor-window-mixin #k"End")
(define-gesture "End of Line" 'text-editor-window-mixin #k"Ctrl-e")
(define-gesture "End of Line" 'echo-area-window-mixin #k"End")
(define-gesture "End of Line" 'echo-area-window-mixin #k"Ctrl-e")

(define-gesture "Forward Character" 'text-editor-window-mixin #k"RightArrow")
(define-gesture "Forward Character" 'text-editor-window-mixin #k"Ctrl-f")
(define-gesture "Forward Character" 'echo-area-window-mixin #k"RightArrow")
(define-gesture "Forward Character" 'echo-area-window-mixin #k"Ctrl-f")

(define-gesture "Backward Character" 'text-editor-window-mixin #k"LeftArrow")
(define-gesture "Backward Character" 'text-editor-window-mixin #k"Ctrl-b")

(define-gesture "Backward Character" 'echo-area-window-mixin #k"LeftArrow")
(define-gesture "Backward Character" 'echo-area-window-mixin #k"Ctrl-b")

(define-gesture "Transpose Characters" 'text-editor-window-mixin #k"Ctrl-t")
(define-gesture "Transpose Characters" 'echo-area-window-mixin #k"Ctrl-t")

(define-gesture "Delete Next Character" 'text-editor-window-mixin #k"Delete")
(define-gesture "Delete Next Character" 'text-editor-window-mixin #k"Ctrl-d")
(define-gesture "Delete Next Character" 'echo-area-window-mixin #k"Delete")
(define-gesture "Delete Next Character" 'echo-area-window-mixin #k"Ctrl-d")

(define-gesture "Delete Previous Character" 'text-editor-window-mixin #k"Backspace")
(define-gesture "Echo Area Delete Previous Character" 'echo-area-window-mixin #k"Backspace")

(define-gesture "Forward Word" 'text-editor-window-mixin #k"Meta-f")
(define-gesture "Forward Word" 'echo-area-window-mixin #k"Meta-f")

(define-gesture "Backward Word" 'text-editor-window-mixin #k"Meta-b")
(define-gesture "Backward Word" 'echo-area-window-mixin #k"Meta-b")

(define-gesture "Find File" :global #k"Ctrl-x" #k"Ctrl-f")

(define-gesture "Confirm Parse" 'echo-area-window-mixin #k"Enter")

(define-gesture "Extended Command" :global #k"Meta-x")

(define-gesture "Confirm Line of Input" "Interactive-Character-Stream" #k"Enter")
(define-gesture "Mousewheel Scroll" 'text-editor-window-mixin #k"MouseWheel")

(define-gesture "Beginning of Buffer" 'text-editor-window-mixin #k"Ctrl-Home")
(define-gesture "End of Buffer" 'text-editor-window-mixin #k"Ctrl-End")
(define-gesture "Scroll Window Up" 'text-editor-window-mixin #k"PageUp")
(define-gesture "Scroll Window Down" 'text-editor-window-mixin #k"PageDown")

(define-gesture "Kill Line" 'text-editor-window-mixin #k"Ctrl-k")
(define-gesture "Kill Line" 'echo-area-window-mixin #k"Ctrl-k")

(define-gesture "Kill Next Word" 'text-editor-window-mixin #k"Meta-d")
(define-gesture "Kill Next Word" 'echo-area-window-mixin #k"Meta-d")

(define-gesture "Kill Previous Word" 'text-editor-window-mixin #k"Meta-Backspace")
(define-gesture "Kill Previous Word" 'echo-area-window-mixin #k"Meta-Backspace")

(define-gesture "Forward Kill Form" "Lisp" #k"Ctrl-Meta-k")
(define-gesture "Backward Kill Form" "Lisp" #k"Ctrl-Meta-Backspace")
(define-gesture "Beginning of Defun" "Lisp" #k"Ctrl-Meta-a")
(define-gesture "End of Defun" "Lisp" #k"Ctrl-Meta-e")
(define-gesture "Forward List" "Lisp" #k"Ctrl-Meta-n")
(define-gesture "Backward List" "Lisp" #k"Ctrl-Meta-p")
(define-gesture "Forward Form" "Lisp" #k"Ctrl-Meta-f")
(define-gesture "Backward Form" "Lisp" #k"Ctrl-Meta-b")
(define-gesture "Mark Form" "Lisp" #k"Ctrl-Meta-Spacebar")
(define-gesture "Mark Defun" "Lisp" #k"Ctrl-Meta-h")
(define-gesture "Extract List" "Lisp " #k"Ctrl-Meta-x")
(define-gesture "Indent Form" 'text-editor-window-mixin #k"Ctrl-Meta-q")

(define-gesture "Indent Or Complete" 'text-editor-window-mixin #k"tab")
(define-gesture "Indent Or Complete" 'echo-area-window-mixin #k"tab")
(define-gesture "Indent" "Lisp" #k"tab")

(define-gesture "Complete Field" 'echo-area-window-mixin #k"Tab")

(define-gesture "Lisp Insert )" "Lisp" #k"Shift-0")
(define-gesture "Insert ()" 'text-editor-window-mixin #k"Shift-Meta-9")
(define-gesture "Move over )" 'text-editor-window-mixin #k"Shift-Meta-0")

(define-gesture "Yank" 'text-editor-window-mixin #k"Ctrl-y")
(define-gesture "Yank" 'echo-area-window-mixin #k"Ctrl-y")

(define-gesture "Query Replace" 'text-editor-window-mixin #k"Shift-Meta-5")
(define-gesture "Reverse Incremental Search" 'text-editor-window-mixin #k"Ctrl-r")
(define-gesture "Incremental Search" 'text-editor-window-mixin #k"Ctrl-s")
(define-gesture "Forward Search" 'text-editor-window-mixin #k"Meta-s")
(define-gesture "Reverse Search" 'text-editor-window-mixin #k"Meta-r")

(define-gesture "Universal Argument" 'text-editor-window-mixin #k"Ctrl-u")
(define-gesture "Universal Argument" 'echo-area-window-mixin #k"Ctrl-u")

(define-gesture "Help" 'text-editor-window-mixin #k"Ctrl-h")
(define-gesture "Help" 'text-editor-window-mixin #k"Shift-Ctrl-\-")
(define-gesture "Help" 'text-editor-window-mixin #k"F1")
(define-gesture "Help" 'echo-area-window-mixin #k"Ctrl-h")
(define-gesture "Help" 'echo-area-window-mixin #k"Shift-Ctrl-\-")
(define-gesture "Help" 'echo-area-window-mixin #k"F1")

(define-gesture "Describe Key" 'text-editor-window-mixin #k"Shift-Meta-/")
(define-gesture "Describe Key" 'echo-area-window-mixin #k"Shift-Meta-/")

(define-gesture "Top Line to Here" 'text-editor-window-mixin #k"RightMouse")


(define-gesture "Generic Pointer Up" :global #k"LeftMouse_Up")
(define-gesture "Generic Pointer Up" :global #k"RightMouse_Up")
(define-gesture "Generic Pointer Up" :global #k"MiddleMouse_Up")


(define-gesture "Generic Pointer Move" :global #k"MouseMove")

(define-gesture "Point to Here" 'text-editor-window-mixin #k"LeftMouse")
(define-gesture "Point to Here" 'echo-area-window-mixin #k"LeftMouse")

#+NOTYET(define-gesture "Insert Kill Buffer" 'text-editor-window-mixin #k"Super-RightMouse")

(define-gesture "Insert File" 'text-editor-window-mixin #k"Ctrl-x" #k"Ctrl-r")
(define-gesture "Visit File" 'text-editor-window-mixin #k"Ctrl-x" #k"Ctrl-v")
(define-gesture "Write File" 'text-editor-window-mixin #k"Ctrl-x" #k"Ctrl-w")
(define-gesture "Backup File" 'text-editor-window-mixin #k"Ctrl-x" #k"Meta-b")
(define-gesture "Save All Files" 'text-editor-window-mixin #k"Ctrl-x" #k"s")
(define-gesture "Save All Files and Exit" 'text-editor-window-mixin #k"Ctrl-x" #k"Ctrl-c")

(define-gesture "Lisp Listener" :global #k"Ctrl-x" #k"l")



;;(define-gesture "Bufed" 'text-editor-window-mixin #k"Ctrl-x" #k"Ctrl-b")
(define-gesture "Buffer Not Modified" 'text-editor-window-mixin #k"Shift-Meta-`")
(define-gesture "Check Buffer Modified" 'text-editor-window-mixin #k"Ctrl-x" #k"Shift-`")
(define-gesture "Select Buffer" 'text-editor-window-mixin #k"Ctrl-x" #k"b")
(define-gesture "Select Previous Buffer" 'text-editor-window-mixin #k"Ctrl-Meta-l")
(define-gesture "Circulate Buffers" 'text-editor-window-mixin #k"Shift-Ctrl-Meta-l")
(define-gesture "Create Buffer" 'text-editor-window-mixin #k"Ctrl-x" #k"Meta-b")
(define-gesture "Kill Buffer" 'text-editor-window-mixin #k"Ctrl-x" #k"k")
(define-gesture "Select Random Typeout Buffer" 'text-editor-window-mixin #k"Hyper-t")

(define-gesture "Cycle Windows" 'text-editor-window-mixin #k"Ctrl-Tab")
(define-gesture "Cycle Windows" 'text-editor-window-mixin #k"Ctrl-x" #k"o")

(define-gesture "Reverse Cycle Windows" 'text-editor-window-mixin #k"Shift-Ctrl-Tab")

(define-gesture "Cycle Windows" 'echo-area-window-mixin #k"Ctrl-Tab")
(define-gesture "Cycle Windows" 'echo-area-window-mixin #k"Ctrl-x" #k"o")

(define-gesture "Reverse Cycle Windows" 'echo-area-window-mixin #k"Shift-Ctrl-Tab")

(define-gesture "Exit Recursive Edit" 'echo-area-window-mixin "Ctrl-Meta-z")

(define-gesture "Interrupt Lisp" "Lisp Listener" #k"Ctrl-c" #k"Ctrl-c")

(define-gesture "Iconify Window" 'homemade-window-mixin #k"Ctrl-x" #k"0")
(define-gesture "Iconify Window" 'text-editor-window-mixin #k"Ctrl-x" #k"0")

(define-gesture "Set/Pop Mark" 'text-editor-window-mixin #k"Ctrl-Spacebar")
(define-gesture "Save Region" 'text-editor-window-mixin #k"Meta-w")
(define-gesture "Set/Pop Mark" 'echo-area-window-mixin #k"Ctrl-Spacebar")
(define-gesture "Save Region" 'echo-area-window-mixin #k"Meta-w")

(define-gesture "Editor Error Test" :global #k"Ctrl-x" #k"Ctrl-e")
(define-gesture "Debugger Test" :global #k"Pause")


(define-gesture "Dired" :global #k"Ctrl-x" #k"Ctrl-Meta-d")

(define-gesture "Dired Delete File and Down Line" "Dired" #k"d")
(define-gesture "Dired Delete File with Pattern" "Dired" #k"Shift-d")
(define-gesture "Dired Delete File" "Dired" #k"Ctrl-d")
(define-gesture "Dired Delete File" "Dired" #k"Delete")
(define-gesture "Dired Delete File" "Dired" #k"k")

(define-gesture "Dired Undelete File and Down Line" "Dired" #k"u")
(define-gesture "Dired Undelete File with Pattern" "Dired" #k"Shift-u")
(define-gesture "Dired Undelete File" "Dired" #k"Ctrl-u")

(define-gesture "Dired Expunge Files" "Dired" #k"Shift-1")
(define-gesture "Dired Update Buffer" "Dired" #k"Hyper-u")
(define-gesture "Dired Edit File" "Dired" #k"Spacebar")
(define-gesture "Dired Edit File" "Dired" #k"e")
(define-gesture "Dired Edit File" "Dired" #k"Enter")
(define-gesture "Dired Edit File" "Dired" #k"Ctrl-m")
(define-gesture "Dired Up Directory" "Dired" #k"Shift-6")
(define-gesture "Dired Quit" "Dired" #k"q")
(define-gesture "Dired Help" "Dired" #k"Shift-/")
(define-gesture "Dired Help" "Dired" #k"F1")

(define-gesture "Dired Copy File" "Dired" #k"c")
(define-gesture "Dired Copy with Wildcard" "Dired" #k"Shift-c")
(define-gesture "Dired Rename File" "Dired" #k"r")
(define-gesture "Dired Rename with Wildcard" "Dired" #k"Shift-r")

(define-gesture "Next Line" "Dired" #k"n")
(define-gesture "Previous Line" "Dired" #k"p")

(define-gesture "Editor Compile Defun" "Lisp" #k"Ctrl-c" #k"Ctrl-c")
(define-gesture "Editor Compile Buffer File" "Lisp"  #k"Ctrl-c" #k"Ctrl-k")

(when (boundp 'krma::*app*)
  (mapcar #'rebuild-command-table
	  (remove-if-not #'(lambda (item)
			     (typep item 'text-editor-view-mixin))
			 (krma::node-children (krma::application-scene krma::*app*)))))
