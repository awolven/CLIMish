;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; This file contains the definitions for the basic Hemlock commands.
;;;

(in-package :hemlock)


;;; Make a mark for buffers as they're consed:

(defun hcmd-new-buffer-hook-fun (buff current-buffer)
  (let ((ring (make-ring 10 #'delete-mark)))
    (defhvar "Buffer Mark Ring"
      "This variable holds this buffer's mark ring."
      :buffer buff
      :value ring
      :current-buffer current-buffer)
    (ring-push (copy-mark (buffer-point buff) :right-inserting) ring)))



#+DISABLE
(defhcommand exit-hemlock-command (window p)
  (:pretty-name "Exit Hemlock")
  (:documentation "Exit hemlock returning to the Lisp top-level read-eval-print loop.")
  (:help-string "Exit hemlock returning to the Lisp top-level read-eval-print loop."))

(defmethod exit-hemlock-command (window p)
  (declare (ignore p))
  (exit-hemlock))

#+DISABLE
(defhcommand pause-hemlock-command (window p)
  (:pretty-name "Pause Hemlock")
  (:documentation
   "Pause the Hemlock/Lisp process returning to the process that invoked the
   Lisp.")
  (:help-string 
   "Pause the Hemlock/Lisp process returning to the process that invoked the
   Lisp."))

(defmethod pause-hemlock-command (window p)
  (declare (ignore p))
  #+nilamb(pause-hemlock))



;;;; Simple character manipulation:

#+NIL
(defcommand "Self Insert" (window p)
  "Insert the last character typed.
  With prefix argument insert the character that many times."
  "Implements ``Self Insert'', calling this function is not meaningful."
  (let* ((window (current-window))
         (buffer (window-buffer window))
         (char (hemlock-ext:key-event-char *last-key-event-typed*)))
    (unless char (editor-error "Can't insert that character."))
    (with-buffer-globals (buffer)
      (if (and p (> p 1))
          (hi::.insert-string. .point. (make-string p :initial-element char))
          (hi::.insert-character. .point. char)))
    (hi::update-all-buffer-windows
     buffer)))

#+DISABLE
(defhcommand quoted-insert-command (window p)
  (:pretty-name "Quoted Insert")
  (:documentation
   "Read a character from the terminal and insert it.
  With prefix argument, insert the character that many times.")
  (:help-string
   "Reads a key-event from *editor-input* and inserts it at the point."))

(defmethod quoted-insert-command (window p)
  (let* ((window window)
         (buffer (window-buffer window))
         (char (hemlock-ext:key-event-char (get-key-event *editor-input* t))))
    (unless char (editor-error "Can't insert that character."))
    (with-buffer-globals (buffer)
      (if (and p (> p 1))
          (hi::.insert-string. .point. (make-string p :initial-element char))
          (hi::.insert-character. .point. char)))
    (mapcar #'climish:redraw-buffer (buffer-windows buffer))))

#+MOVED
(defcommand "Forward Character" (window p)
    "Move the point forward one character.
   With prefix argument move that many characters, with negative argument
   go backwards."
    "Move the point of the current buffer forward p characters."
  (let ((p (or p 1)))
    (let* ((buffer (window-buffer window)))
      (with-buffer-globals (buffer)
        (cond ((character-offset .point. p))
              ((= p 1)
               (editor-error "No next character."))
              ((= p -1)
               (editor-error "No previous character."))
              (t
               (if (plusp p)
                   (buffer-end .point. .buffer.)
                   (buffer-start .point. .buffer.))
               (editor-error "Not enough characters."))))
      (maybe-rebottom-current-on-mark-then-update-all-buffer-windows
       buffer (buffer-point buffer) window))))

#+MOVED
(defcommand "Backward Character" (window p)
  "Move the point backward one character.
  With prefix argument move that many characters backward."
  "Move the point p characters backward."
  (forward-character-command window (if p (- p) -1)))

#|
(defcommand "Delete Next Character" (window p)
  "Deletes the character to the right of the point.
  With prefix argument, delete that many characters to the right
  (or left if prefix is negative)."
  "Deletes p characters to the right of the point."
  (unless (delete-characters (current-point) (or p 1))
    (buffer-end (current-point))
    (editor-error "No next character.")))

(defcommand "Delete Previous Character" (window p)
  "Deletes the character to the left of the point.
  With prefix argument, delete that many characters to the left
  (or right if prefix is negative)."
  "Deletes p characters to the left of the point."
  (unless (delete-characters (current-point) (if p (- p) -1))
    (editor-error "No previous character.")))
|#

#+MOVED
(defcommand "Delete Next Character" (window p)
    "Deletes the character to the right of the point.
   With prefix argument, delete that many characters to the right
  (or left if prefix is negative)."
    "Deletes p characters to the right of the point."
  (cond ((with-buffer-globals ((window-buffer window))
           (.kill-characters. .point. (or p 1))))
        ((and p (minusp p))
         (editor-error "Not enough previous characters."))
        (t
         (editor-error "Not enough next characters."))))

#+MOVED
(defcommand "Delete Previous Character" (window p)
    "Deletes the character to the left of the point.
   Will push characters from successive deletes on to the kill ring."
  "Deletes the character to the left of the point.
   Will push characters from successive deletes on to the kill ring."
  (delete-next-character-command window (- (or p 1))))

#+MOVED
(defcommand "Transpose Characters" (window p)
    "Exchanges the characters on either side of the point and moves forward
  With prefix argument, does this that many times.  A negative prefix
  argument causes the point to be moved backwards instead of forwards."
    "Exchanges the characters on either side of the point and moves forward."
  (let* ((window (current-window))
         (buffer (when window (window-buffer window))))
    (when buffer
      (let ((arg (or p 1))
            (point (buffer-point buffer)))
          (dotimes (i (abs arg))
            (when (or (minusp arg) (end-line-p point)) (mark-before point))
            (let ((prev nil)
                  (next nil))
              (with-buffer-globals (buffer)
                (setq prev (hi::.previous-character. point))
                (setq next (hi::.next-character. point)))
              (cond ((not prev) (editor-error "No previous character."))
                    ((not next) (editor-error "No next character."))
                    (t
                     (with-buffer-globals (buffer)
                       (hi::.set-previous-character. next point)
                       (hi::.set-next-character. prev point))))
            (when (plusp arg) (mark-after point)))))
      (hi::update-all-buffer-windows buffer))))

;;;; Word hacking commands:

;;; WORD-OFFSET
;;;
;;;    Move a mark forward/backward some words.
;;;
(defun word-offset (buffer mark offset)
  "Move Mark by Offset words."
  (if (minusp offset)
      (do ((cnt offset (1+ cnt)))
          ((zerop cnt) mark)
        (cond
          ((null (reverse-find-attribute buffer mark :word-delimiter #'zerop))
           (return nil))
          ((reverse-find-attribute buffer mark :word-delimiter))
          (t
           (move-mark
            mark (buffer-start-mark buffer)))))
      (do ((cnt offset (1- cnt)))
          ((zerop cnt) mark)
        (cond
          ((null (find-attribute buffer mark :word-delimiter #'zerop))
           (return nil))
          ((null (find-attribute buffer mark :word-delimiter))
           (return nil))))))

#+MOVED
(defcommand "Forward Word" (window p)
    "Moves forward one word.
  With prefix argument, moves the point forward over that many words."
    "Moves the point forward p words."
  (with-buffer-globals ((window-buffer (current-window)))
    (cond ((word-offset .point. (or p 1)))
          ((and p (minusp p))
           (buffer-start .point. .buffer.)
           (editor-error "No previous word."))
          (t
           (buffer-end .point. .buffer.)
           (editor-error "No next word.")))))

#+MOVED
(defcommand "Backward Word" (window p)
    "Moves forward backward word.
  With prefix argument, moves the point back over that many words."
  "Moves the point backward p words."
  (forward-word-command window (- (or p 1))))



;;;; Moving around:

;;(defvar *target-column* 0) ; target-column is now a buffer-global

(defmacro .set-target-column. (mark)
  `(if (eq (last-command-type) :line-motion)
       *target-column*
       (setq *target-column* (hi::.mark-column. ,mark))))

(defun set-target-column (buffer mark)
  (with-buffer-globals (buffer)
    (.set-target-column. mark)))

#+MOVED
(defcommand "Next Line" (window p)
    "Moves the point to the next line.
   With prefix argument, moves the point that many lines down (or up if
   the prefix is negative)."
    "Moves the down p lines."
  (let* ((cw (current-window))
         (b (window-buffer cw)))
    (let* ((target (with-buffer-globals (b)
                     (.set-target-column. .point.))))
        (unless (with-buffer-globals (b) (line-offset .point. (or p 1)))
          (cond ((not p)
                 (with-buffer-globals (b)
                   (when (same-line-p .point. (buffer-end-mark .buffer.))
                     (line-end .point.)))
                 ;;
                 ;; The insert-character was probably intentional back when
                 ;; Hemlock was written, but it inserts bogus newlines at
                 ;; the end of the file, and that's a bad idea.
                 ;;
                 ;; (insert-character point #\newline)
                 (editor-error "No next line."))
                ((minusp p)
                 (with-buffer-globals (b)
                   (buffer-start .point. .buffer.))
                 (editor-error "No previous line."))
                (t
                 (with-buffer-globals (b)
                   (buffer-end .point. .buffer.))
                 (when p (editor-error "No next line.")))))
      (with-buffer-globals (b)
        (unless (.move-to-column. .point. target) (line-end .point.)))
      (setf (last-command-type) :line-motion))
    (maybe-recenter-current-on-mark-then-update-all-buffer-windows
     b (buffer-point b) cw)))

#+MOVED
(defcommand "Previous Line" (window p)
    "Moves the point to the previous line.
  With prefix argument, moves the point that many lines up (or down if
  the prefix is negative)."
  "Moves the point up p lines."
  (next-line-command window (- (or p 1))))

#+DISABLE
(defhcommand mark-to-end-of-buffer-command (window p)
  (:pretty-name "Mark to End of Buffer")
  (:documentation "Sets the current region from point to the end of the buffer.")
  (:help-string "Sets the current region from point to the end of the buffer."))

(defmethod mark-to-end-of-buffer-command (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (buffer-end (copy-mark point) buffer)))

#+DISABLE
(defhcommand mark-to-beginning-of-buffer (window p)
  (:pretty-name "Mark to Beginning of Buffer")
  (:documentation "Sets the current region from the beginning of the buffer to point.")
  (:help-string "Sets the current region from the beginning of the buffer to point."))

(defmethod mark-to-beginning-of-buffer (window p)
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (buffer-start (copy-mark point) buffer)))


#+MOVED
(defcommand "Beginning of Buffer" (window p)
  "Moves the point to the beginning of the current buffer."
    "Moves the point to the beginning of the current buffer."
  (let* ((buffer (window-buffer window)))
    (with-buffer-globals (buffer)
      (buffer-start .point. .buffer.))
    ;; the window redisplay scheduler cannot be inside with-buffer-globals
    ;; since it waits for the scheduled window op to be done
    ;; and will therefore deadlock if so
    (maybe-recenter-current-on-mark-then-update-all-buffer-windows
     buffer (buffer-point buffer) window)))

#+MOVED
(defcommand "End of Buffer" (window p)
  "Moves the point to the end of the current buffer."
    "Moves the point to the end of the current buffer."
  (let* ((window (current-window))
         (buffer (window-buffer window)))
    (with-buffer-globals (buffer)
      (buffer-end .point. .buffer.))
    (maybe-rebottom-current-on-mark-then-update-all-buffer-windows
     buffer (buffer-point buffer) window)))

#+MOVED
(defcommand "Beginning of Line" (window p)
    "Moves the point to the beginning of the current line.
  With prefix argument, moves the point to the beginning of the prefix'th
  next line."
    "Moves the point down p lines and then to the beginning of the line."
  (let* ((window (current-window))
         (buffer (window-buffer window)))
    (with-buffer-globals (buffer)
      (let ((point .point.))
        (unless (line-offset point (if p p 0)) (editor-error "No such line."))
        (line-start point)))))

#+MOVED
(defcommand "End of Line" (window p)
    "Moves the point to the end of the current line.
  With prefix argument, moves the point to the end of the prefix'th next line."
    "Moves the point down p lines and then to the end of the line."
  (let* ((window (current-window))
         (buffer (window-buffer window)))
    (with-buffer-globals (buffer)
      (let ((point .point.))
        (unless (line-offset point (if p p 0)) (editor-error "No such line."))
        (line-end point)))))


#+MOVED
(defcommand "Scroll Window Down" (window p)
  "Move down one screenfull.
  With prefix argument scroll down that many lines."
  "If P is NIL then scroll Window, which defaults to the current
  window, down one screenfull.  If P is supplied then scroll that
  many lines."
  ;; if between the time (current-window) is called
  ;; and (window-buffer window) is executed in render thread
  ;; the window-buffer has actually changed
  ;; we're gonna scroll the window with it's new buffer
  ;; rather than confusing the .scroll-window.
  ;; with with buffer globals which apply to some old buffer
  ;; note: .scroll-window. calls update-window-image which
  ;; again grabs the window-buffer of window, which is going to
  ;; be the same if everyone is respecting the window lock

  ;; it would be better if hemlock never allowed you to setf the window-buffer
  ;; of a window, and forced you to make a new window in those cases instead
  (let ((buffer (window-buffer window)))
    (hi::with-render-thread (*app*)
      (with-buffer-globals (buffer)
        (if p
            (hi::.scroll-window. window p)
            (let ((height (window-height window))
                  (overlap (value scroll-overlap)))
              (hi::.scroll-window. window (if (<= height overlap)
                                              height (- height overlap)))))))))

#+MOVED
(defcommand "Scroll Window Up" (window p)
    "Move up one screenfull.
  With prefix argument scroll up that many lines."
    "If P is NIL then scroll Window, which defaults to the current
  window, up one screenfull.  If P is supplied then scroll that
  many lines."
  ;; same considerations as "Scroll Window Down"
  (let ((buffer (window-buffer window)))
    (hi::with-render-thread (*app*)
      (with-buffer-globals (buffer)
        (if p
            (hi::.scroll-window. window (- p))
            (let ((height (- (window-height window)))
                  (overlap (- (value scroll-overlap))))
              (hi::.scroll-window. window (if (>= height overlap)
                                              height (- height overlap)))))))))

#+MOVED
(defcommand "Mousewheel Scroll" (window p)
    "Use mousewheel to scroll."
    "Use mousewheel to scroll."
  (declare (ignore p))
  (multiple-value-bind (x y window)
      (last-key-event-cursorpos)
    (declare (ignore x))
    (unless y (editor-error))
    (hi::with-render-thread (*app*)
      (with-buffer-globals ((window-buffer window))
        (hi::.scroll-window. window (- (* 2 (round y))))))))

#+DISABLE
(defhcommand scroll-next-window-down-command (window p)
  (:pretty-name "Scroll Next Window Down")
  (:documentation "Do a \"Scroll Window Down\" on the next window.")
  (:help-string "Do a \"Scroll Window Down\" on the next window."))

(defmethod scroll-next-window-down-command (window p)
  (let ((win (climish::next window)))
    (when (eq win window) (editor-error "Only one window."))
    (scroll-window-down-command p win)))

#+DISABLE
(defhcommand scroll-next-window-up-command (window p)
  (:pretty-name "Scroll Next Window Up")
  (:documentation "Do a \"Scroll Window Up\" on the next window.")
  (:help-string "Do a \"Scroll Window Up\" on the next window."))

(defmethod scroll-next-window-up-command (window p)
  (let ((win (climish::next window)))
    (when (eq win window) (editor-error "Only one window."))
    (scroll-window-up-command p win)))

#+DISABLE
(defhcommand top-of-window-command (window p)
  (:pretty-name "Top of Window")
  (:documentation "Move the point to the top of the current window.
  The point is left before the first character displayed in the window.")
  (:help-string "Move the point to the top of the current window."))

(defmethod top-of-window-command (window p)
  (declare (ignore p))
  (let ()
    (with-buffer-globals ((window-buffer window))
      (move-mark .point. (window-display-start window)))))

#+DISABLE
(defhcommand bottom-of-window-command (window p)
  (:pretty-name "Bottom of Window")
  (:documentation "Move the point to the bottom of the current window.
  The point is left at the start of the bottom line.")
  (:help-string "Move the point to the bottom of the current window."))

(defmethod bottom-of-window-command (window p)
  (declare (ignore p))
  (with-buffer-globals ((window-buffer window))
    (line-start .point.
                (mark-line (window-display-end window)))))

;;;; Kind of miscellaneous commands:

;;; "Refresh Screen" may not be right with respect to wrapping lines in
;;; the case where an argument is supplied due the use of
;;; WINDOW-DISPLAY-START instead of SCROLL-WINDOW, but using the latter
;;; messed with point and did other hard to predict stuff.
;;;
#+DISABLE
(defhcommand refresh-screen-command (window p)
  (:pretty-name "Refresh Screen")
  (:documentation "Refreshes everything in the window, centering current line.
   Given an argument, scroll that many lines.")
  (:help-string "Refreshes everything in the window, centering current line.
   Given an argument, scroll that many lines."))

(defmethod refresh-screen-command (window p)
  (unless p
    (let* ((buffer (window-buffer window))
           (point (buffer-point buffer)))
      (hi::center-window-on-mark window point)
      (climish:redraw-buffer window))))


#+DISABLE
(defhcommand track-buffer-point-command (window p)
  (:pretty-name "Track Buffer Point")
  (:documentation
   "Make the current window track the buffer's point.
   This means that each time Hemlock redisplays, it will make sure the buffer's
   point is visible in the window.  This is useful for windows into buffer's
   that receive output from streams coming from other processes.")
  (:help-string "Make the current window track the buffer's point."))

(defmethod track-buffer-point-command (window p)
  (declare (ignore p))
  ;; interesting flag
  (setf (window-display-recentering window) t))
;;;
(defun reset-window-display-recentering (window &optional buffer)
  (declare (ignore buffer))
  (setf (window-display-recentering window) nil))
;;;


(defhcommand extended-command-command (window p)
  (:pretty-name "Extended Command")
  (:help-string "Prompts for and executes an extended command.")
  (:documentation "Prompts for and executes an extended command.  The prefix argument is
  passed to the command."))

(defmethod extended-command-command (window p)  
  (let* ((name (prompt-for-keyword window
                                   (list *command-names*)
                                   :prompt "Extended Command: "
                                   :help "Name of a Hemlock command")))
    (climish::redraw-buffer *echo-area-window*)
    (let ((cmd (getstring name *command-names*)))
      (clear-echo-area)
      (message (command-name cmd))
      (climish::invoke-command cmd (climish::send *echo-area-window* climish::parent)))))

#+DISABLE
(defhcommand eval-expression-command (window p)
  (:pretty-name "Eval Expresion")
  (:documentation "")
  (:help-string ""))

(defmethod eval-expression-command (window p)
  (declare (ignore p))
  (message "~S" (eval (prompt-for-expression window))))


#+DISABLE
(defhcommand universal-argument-command (window p)
  (:pretty-name "Universal Argument")
  (:documentation "Sets prefix argument for next command.
  Typing digits, regardless of any modifier keys, specifies the argument.
  Optionally, you may first type a sign (- or +).  While typing digits, if you
  type C-U or C-u, the digits following the C-U form a number this command
  multiplies by the digits preceding the C-U.  The default value for this
  command and any number following a C-U is the value of \"Universal Argument
  Default\".")
  (:help-string "You probably don't want to use this as a function."))

(defmethod universal-argument-command (window p)
  (declare (ignore p))
  (clear-echo-area)
  (write-string "C-U " *echo-area-stream*)
  (finish-output *echo-area-stream*)
  (let* ((key-event (get-key-event *editor-input*))
         (char (hemlock-ext:key-event-char key-event)))
    (if char
        (case char
          (#\-
           (write-char #\- *echo-area-stream*)
           (finish-output *echo-area-stream*)
           (universal-argument-loop (get-key-event *editor-input*) -1))
          (#\+
           (write-char #\+ *echo-area-stream*)
           (finish-output *echo-area-stream*)
           (universal-argument-loop (get-key-event *editor-input*) -1))
          (t
           (universal-argument-loop key-event 1)))
        (universal-argument-loop key-event 1))))

#+DISABLE
(defhcommand negative-argument-command (window p)
  (:pretty-name "Negative Argument")
  (:documentation "This command is equivalent to invoking \"Universal Argument\" and typing
   a minus sign (-).  It waits for more digits and a command to which to give
   the prefix argument.")
  (:help-string "Don't call this as a function."))

(defmethod negative-argument-command (window p)
  (when p (editor-error "Must type minus sign first."))
  (clear-echo-area)
  (write-string "C-U -" *echo-area-stream*)
  (finish-output *echo-area-stream*)
  (universal-argument-loop (get-key-event *editor-input*) -1))

#+DISABLE
(defhcommand argument-digit-command (window p)
    (:pretty-name "Argument Digit")
    (:documentation
     "This command is equivalent to invoking \"Universal Argument\" and typing
   the digit used to invoke this command.  It waits for more digits and a
   command to which to give the prefix argument.")
  (:help-string "Don't call this as a function."))

(defmethod argument-digit-command (window p)
  (declare (ignore p))
  (clear-echo-area)
  (write-string "C-U " *echo-area-stream*)
  (finish-output *echo-area-stream*)
  (universal-argument-loop *last-key-event-typed* 1))

(defun universal-argument-loop (key-event sign &optional (multiplier 1))
  (flet ((prefix (sign multiplier read-some-digit-p result)
           ;; read-some-digit-p and (zerop result) are not
           ;; equivalent if the user invokes this and types 0.
           (* sign multiplier
              (if read-some-digit-p
                  result
                  (value universal-argument-default)))))
    (let* ((stripped-key-event (if key-event (hemlock-ext:make-key-event key-event)))
           (char (hemlock-ext:key-event-char stripped-key-event))
           (digit (if char (digit-char-p char)))
           (result 0)
           (read-some-digit-p nil))
      (loop
        (cond (digit
               (setf read-some-digit-p t)
               (write-char char *echo-area-stream*)
               (finish-output *echo-area-stream*)
               (setf result (+ digit (* 10 result)))
               (setf key-event (get-key-event *editor-input*))
               (setf stripped-key-event (if key-event
                                            (hemlock-ext:make-key-event key-event)))
               (setf char (hemlock-ext:key-event-char stripped-key-event))
               (setf digit (if char (digit-char-p char))))
              ((or (eq key-event #k"C-u") (eq key-event #k"C-U"))
               (write-string " C-U " *echo-area-stream*)
               (finish-output *echo-area-stream*)
               (universal-argument-loop
                (get-key-event *editor-input*) 1
                (prefix sign multiplier read-some-digit-p result))
               (return))
              (t
               (unget-key-event key-event *editor-input*)
               (setf (prefix-argument)
                     (prefix sign multiplier read-some-digit-p result))
               (return))))))
  (setf (last-command-type) (last-command-type)))
