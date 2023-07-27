;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(in-package :hemlock)

;;;
;;; **********************************************************************
;;;
;;; Hemlock LISP Mode commands
;;;
;;; Written by Ivan Vazquez and Bill Maddox.
;;;



;; (declaim (optimize (speed 2))); turn off byte compilation.


;;;; Variables and lisp-info structure.

;;; These routines are used to define, for standard LISP mode, the start and end
;;; of a block to parse.  If these need to be changed for a minor mode that sits
;;; on top of LISP mode, simply do a DEFHVAR with the minor mode and give the
;;; name of the function to use instead of START-OF-PARSE-BLOCK and
;;; END-OF-PARSE-BLOCK.
;;;


#+NIL
(defmacro .delete-horizontal-space. (mark)
  "Deletes all :space characters on either side of mark."
  (let ((mark-sym (gensym "DELETE-HORIZONTAL-SPACE_MARK")))
    `(let ((,mark-sym ,mark))
       (with-mark ((start ,mark-sym))
         (reverse-find-attribute start :space #'zerop)
         (find-attribute ,mark-sym :space #'zerop)
         (hi::.delete-region. (region start ,mark-sym))))))

(defmacro .insert-lisp-indentation. (m)
  (let ((m-sym (gensym "INSERT-LISP-INDENTATION_M")))
    `(let ((,m-sym ,m))
       (delete-horizontal-space .buffer. ,m-sym)
       (indent-to-column .buffer. ,m-sym (lisp-indentation .buffer. ,m-sym)))))

(defun insert-lisp-indentation (buffer m)
  (with-buffer-globals (buffer)
    (.insert-lisp-indentation. m)))


;;; LISP-INFO is the structure used to store the data about the line in its
;;; Plist.
;;;
;;;     -> BEGINS-QUOTED, ENDING-QUOTED are both Boolean slots that tell whether
;;;        or not a line's begining and/or ending are quoted.
;;;
;;;     -> RANGES-TO-IGNORE is a list of cons cells, each having the form
;;;        ( [begining-charpos] [end-charpos] ) each of these cells indicating
;;;        a range to ignore.  End is exclusive.
;;;
;;;     -> NET-OPEN-PARENS, NET-CLOSE-PARENS integers that are the number of
;;;        unmatched opening and closing parens that there are on a line.
;;;
;;;     -> SIGNATURE-SLOT ...
;;;

(defstruct (lisp-info (:constructor make-lisp-info ()))
  (begins-quoted nil)           ; (or t nil)
  (ending-quoted nil)           ; (or t nil)
  (ranges-to-ignore nil)        ; (or t nil)
  (net-open-parens 0 :type fixnum)
  (net-close-parens 0 :type fixnum)
  (signature-slot))



;;;; Macros.

;;; The following Macros exist to make it easy to acces the Syntax primitives
;;; without uglifying the code.  They were originally written by Maddox.
;;;

(defmacro scan-char (buffer mark attribute values)
  `(find-attribute ,buffer ,mark ',attribute ,(attr-predicate values)))

(defmacro rev-scan-char (mark attribute values)
  `(reverse-find-attribute ,mark ',attribute ,(attr-predicate values)))

(defmacro test-char (char attribute values)
  `(let ((x (character-attribute ',attribute ,char)))
     ,(attr-predicate-aux values)))

(eval-when (:compile-toplevel :execute :load-toplevel)
(defun attr-predicate (values)
  (cond ((eq values 't)
         '#'plusp)
        ((eq values 'nil)
         '#'zerop)
        (t `#'(lambda (x) ,(attr-predicate-aux values)))))

(defun attr-predicate-aux (values)
  (cond ((eq values t)
         '(plusp x))
        ((eq values nil)
         '(zerop x))
        ((symbolp values)
         `(eq x ',values))
        ((and (listp values) (member (car values) '(and or not)))
         (cons (car values) (mapcar #'attr-predicate-aux (cdr values))))
        (t (error "Illegal form in attribute pattern - ~S" values))))

); Eval-When

;;;
;;; FIND-LISP-CHAR

(defmacro find-lisp-char (buffer mark)
  "Move MARK to next :LISP-SYNTAX character, if one isn't found, return NIL."
  `(find-attribute ,buffer ,mark :lisp-syntax
                   #'(lambda (x)
                       (member x '(:open-paren :close-paren :newline :comment
                                               :char-quote :string-quote)))))
;;;
;;; PUSH-RANGE

(defmacro push-range (new-range info-struct)
  "Insert NEW-RANGE into the LISP-INFO-RANGES-TO-IGNORE slot of the INFO-STRUCT."
  `(when ,new-range
     (setf (lisp-info-ranges-to-ignore ,info-struct)
           (cons ,new-range (lisp-info-ranges-to-ignore ,info-struct)))))
;;;
;;; SCAN-DIRECTION

(defmacro scan-direction (buffer mark forwardp &rest forms)
  "Expand to a form that scans either backward or forward according to Forwardp."
  (if forwardp
      `(scan-char ,buffer ,mark ,@forms)
      `(rev-scan-char ,mark ,@forms)))
;;;
;;; DIRECTION-CHAR

(defmacro direction-char (buffer mark forwardp)
  "Expand to a form that returns either the previous or next character according
  to Forwardp."
  (if forwardp
      `(next-character ,buffer ,mark)
      `(previous-character ,buffer ,mark)))

;;;
;;; NEIGHBOR-MARK

(defmacro neighbor-mark (buffer mark forwardp)
  "Expand to a form that moves MARK either backward or forward one character,
  depending on FORWARDP."
  (if forwardp
      `(mark-after ,buffer ,mark)
      `(mark-before ,buffer ,mark)))

;;;
;;; NEIGHBOR-LINE

(defmacro neighbor-line (line forwardp)
  "Expand to return the next or previous line, according to Forwardp."
  (if forwardp
      `(line-next ,line)
      `(line-previous ,line)))

;;; PARSE-OVER-BLOCK
;;;
(defmacro .parse-over-block. (start-line end-line &optional (fer-sure-parse nil))
  "Parse over an area indicated from END-LINE to START-LINE."
  (let ((start-line-sym (gensym "PARSE-OVER-BLOCK_START-LINE"))
        (end-line-sym (gensym "PARSE-OVER-BLOCK_END-LINE"))
        (fer-sure-parse-sym (gensym "PARSE-OVER-BLOCK_FER-SURE-PARSE")))
    `(let* ((,start-line-sym ,start-line)
            (,end-line-sym ,end-line)
            (test-line ,start-line-sym)
            prev-line-info
            (,fer-sure-parse-sym ,fer-sure-parse))

       (with-mark ((mark (mark test-line 0)))

         ;; Set the pre-begining and post-ending lines to delimit the range
         ;; of action any command will take.  This means set the lisp-info of the
         ;; lines immediately before and after the block to Nil.

         (when (line-previous ,start-line-sym)
           (setf (getf (line-plist (line-previous ,start-line-sym)) 'lisp-info) nil))
         (when (line-next ,end-line-sym)
           (setf (getf (line-plist (line-next ,end-line-sym)) 'lisp-info) nil))

         (loop
           (let ((line-info (getf (line-plist test-line) 'lisp-info)))

             ;;    Reparse the line when any of the following are true:
             ;;
             ;;      FER-SURE-PARSE is T
             ;;
             ;;      LINE-INFO or PREV-LINE-INFO are Nil.
             ;;
             ;;      If the line begins quoted and the previous one wasn't
             ;;      ended quoted.
             ;;
             ;;      The Line's signature slot is invalid (the line has changed).
             ;;

             (when (or ,fer-sure-parse-sym
                       (not line-info)
                       (not prev-line-info)

                       (not (eq (lisp-info-begins-quoted line-info)
                                (lisp-info-ending-quoted prev-line-info)))

                       (not (eql (line-signature test-line)
                                 (lisp-info-signature-slot line-info))))

               (move-to-position mark 0 test-line)

               (unless line-info
                 (setf line-info (make-lisp-info))
                 (setf (getf (line-plist test-line) 'lisp-info) line-info))

               (parse-lisp-line-info .buffer. mark line-info prev-line-info))

             (when (eq ,end-line-sym test-line)
               (return nil))

             (setq prev-line-info line-info)

             (setq test-line (line-next test-line))))))))

(defun parse-over-block (buffer start-line end-line &optional (fer-sure-parse nil))
  (with-buffer-globals (buffer)
    (.parse-over-block. start-line end-line fer-sure-parse)))


;;;; Parsing functions.

;;; PRE-COMMAND-PARSE-CHECK BUFFER -- Public.
;;;
(defmacro .pre-command-parse-check. (mark &optional (fer-sure-parse nil))
  "Parse the area before the command is actually executed."
  (let ((mark-sym (gensym "PRE-COMMAND-PARSE-CHECK_MARK"))
        (fer-sure-parse-sym (gensym "PRE-COMMAND-PARSE-CHECK_FER-SURE-PARSE")))
    `(let ((,mark-sym ,mark)
           (,fer-sure-parse-sym ,fer-sure-parse))
       (with-mark ((top ,mark-sym)
                   (bottom ,mark-sym))
         (funcall (value parse-start-function) top)
         (funcall (value parse-end-function) bottom)
         (parse-over-block .buffer. (mark-line top) (mark-line bottom) ,fer-sure-parse-sym)))))

(defun pre-command-parse-check (buffer mark &optional (fer-sure-parse nil))
  (with-buffer-globals (buffer)
    (.pre-command-parse-check. mark fer-sure-parse)))






;;;
;;; START-OF-SEARCH-LINE

(defun start-of-search-line (buffer line)
  "Set LINE to the begining line of the block of text to parse."
  (with-mark ((mark (mark line 0)))
    (funcall (value parse-start-function) buffer mark)
    (setq line (mark-line mark))))

;;;
;;; END-OF-SEACH-LINE

(defun end-of-search-line (buffer line)
  "Set LINE to the ending line of the block of text to parse."
  (with-mark ((mark (mark line 0)))
    (funcall (value parse-end-function) buffer mark)
    (setq line (mark-line mark))))


;;;; PARSE-LISP-LINE-INFO.

;;; PARSE-LISP-LINE-INFO -- Internal.
;;;
;;; This parses through the line doing the following things:
;;;
;;;      Counting/Setting the NET-OPEN-PARENS & NET-CLOSE-PARENS.
;;;
;;;      Making all areas of the line that should be invalid (comments,
;;;      char-quotes, and the inside of strings) and such be in
;;;      RANGES-TO-IGNORE.
;;;
;;;      Set BEGINS-QUOTED and ENDING-QUOTED
;;;
(defun parse-lisp-line-info (buffer mark line-info prev-line-info)
  "Parse line and set line information like NET-OPEN-PARENS, NET-CLOSE-PARENS,
   RANGES-TO-INGORE, and ENDING-QUOTED."
  (let ((net-open-parens 0)
        (net-close-parens 0))
    (declare (fixnum net-open-parens net-close-parens))

    ;; Re-set the slots necessary

    (setf (lisp-info-ranges-to-ignore line-info) nil)

    ;; The only way the current line begins quoted is when there
    ;; is a previous line and it's ending was quoted.

    (setf (lisp-info-begins-quoted line-info)
          (and prev-line-info
               (lisp-info-ending-quoted prev-line-info)))

    (if (lisp-info-begins-quoted line-info)
        (deal-with-string-quote buffer mark line-info)
        (setf (lisp-info-ending-quoted line-info) nil))

    (unless (lisp-info-ending-quoted line-info)
      (loop
        (find-lisp-char buffer mark)
        (ecase (character-attribute :lisp-syntax (next-character buffer mark))

          (:open-paren
           (setq net-open-parens (1+ net-open-parens))
           (mark-after buffer mark))

          (:close-paren
           (if (zerop net-open-parens)
               (setq net-close-parens (1+ net-close-parens))
               (setq net-open-parens (1- net-open-parens)))
           (mark-after buffer mark))

          (:newline
           (setf (lisp-info-ending-quoted line-info) nil)
           (return t))

          (:comment
           (push-range (cons (mark-charpos mark) (line-length buffer (mark-line mark)))
                       line-info)
           (setf (lisp-info-ending-quoted line-info) nil)
           (return t))

          (:char-quote
           (mark-after buffer mark)
           (push-range (cons (mark-charpos mark) (1+ (mark-charpos mark)))
                       line-info)
           (mark-after buffer mark))

          (:string-quote
           (mark-after buffer mark)
           (unless (deal-with-string-quote buffer mark line-info)
             (setf (lisp-info-ending-quoted line-info) t)
             (return t))))))

    (setf (lisp-info-net-open-parens line-info) net-open-parens)
    (setf (lisp-info-net-close-parens line-info) net-close-parens)
    (setf (lisp-info-signature-slot line-info)
          (line-signature (mark-line mark)))))



;;;; String quote utilities.

;;; VALID-STRING-QUOTE-P
;;;
(defmacro valid-string-quote-p (buffer mark forwardp)
  "Return T if the string-quote indicated by MARK is valid."
  (let ((test-mark (gensym))
        (buffer-sym (gensym)))
    `(let ((,buffer-sym ,buffer))
       (with-mark ((,test-mark ,mark))
         ,(unless forwardp
            ;; TEST-MARK should always be right before the String-quote to be
            ;; checked.
            `(mark-before ,buffer-sym ,test-mark))
         (when (test-char (next-character ,buffer-sym ,test-mark) :lisp-syntax :string-quote)
           (let ((slash-count 0))
             (loop
               (mark-before ,buffer-sym ,test-mark)
               (if (test-char (next-character ,buffer-sym ,test-mark) :lisp-syntax :char-quote)
                   (incf slash-count)
                   (return t)))
             (not (oddp slash-count))))))))

;;;
;;; FIND-VALID-STRING-QUOTE

(defmacro find-valid-string-quote (buffer mark &key forwardp (cease-at-eol nil))
  "Expand to a form that will leave MARK before a valid string-quote character,
  in either a forward or backward direction, according to FORWARDP.  If
  CEASE-AT-EOL is T then it will return nil if encountering the EOL before a
  valid string-quote."
  (let ((e-mark (gensym))
        (buffer-sym (gensym)))
    `(let ((,buffer-sym ,buffer))
       (with-mark ((,e-mark ,mark))

         (loop
           (unless (scan-direction ,buffer-sym ,e-mark ,forwardp :lisp-syntax
                                   ,(if cease-at-eol
                                        `(or :newline :string-quote)
                                        `:string-quote))
             (return nil))

           ,@(if cease-at-eol
                 `((when (test-char (direction-char ,buffer-sym ,e-mark ,forwardp) :lisp-syntax
                                    :newline)
                     (return nil))))

           (when (valid-string-quote-p ,buffer-sym ,e-mark ,forwardp)
             (move-mark ,mark ,e-mark)
             (return t))

           (neighbor-mark ,buffer-sym ,e-mark ,forwardp))))))

;;;; DEAL-WITH-STRING-QUOTE.

;;; DEAL-WITH-STRING-QUOTE
;;;
;;; Called when a string is begun (i.e. parse hits a #\").  It checks for a
;;; matching quote on the line that MARK points to, and puts the appropriate
;;; area in the RANGES-TO-IGNORE slot and leaves MARK pointing after this area.
;;; The "appropriate area" is from MARK to the end of the line or the matching
;;; string-quote, whichever comes first.
;;;
(defun deal-with-string-quote (buffer mark info-struct)
  "Alter the current line's info struct as necessary as due to encountering a
   string quote character."
  (with-mark ((e-mark mark))
    (cond ((find-valid-string-quote buffer e-mark :forwardp t :cease-at-eol t)
           ;; If matching quote is on this line then mark the area between the
           ;; first quote (MARK) and the matching quote as invalid by pushing
           ;; its begining and ending into the IGNORE-RANGE.
           (push-range (cons (mark-charpos mark) (mark-charpos e-mark))
                       info-struct)
           (setf (lisp-info-ending-quoted info-struct) nil)
           (mark-after buffer e-mark)
           (move-mark mark e-mark))
          ;; If the EOL has been hit before the matching quote then mark the
          ;; area from MARK to the EOL as invalid.
          (t
           (push-range (cons (mark-charpos mark)
                             (1+ (line-length buffer (mark-line mark))))
                       info-struct)
           ;; The Ending is marked as still being quoted.
           (setf (lisp-info-ending-quoted info-struct) t)
           (line-end buffer mark)
           nil))))



;;;; Character validity checking:

;;; Find-Ignore-Region  --  Internal
;;;
;;;    If the character in the specified direction from Mark is in an ignore
;;; region, then return the region and the line that the region is in as
;;; values.  If there is no ignore region, then return NIL and the Mark-Line.
;;; If the line is not parsed, or there is no character (because of being at
;;; the buffer beginning or end), then return both values NIL.
;;;
(defun find-ignore-region (buffer mark forwardp)
  (flet ((scan (line pos)
           (declare (fixnum pos))
           (let ((info (getf (line-plist line) 'lisp-info)))
             (if info
                 (dolist (range (lisp-info-ranges-to-ignore info)
                                (values nil line))
                   (let ((start (car range))
                         (end (cdr range)))
                     (declare (fixnum start end))
                     (when (and (>= pos start) (< pos end))
                       (return (values range line)))))
                 (values nil nil)))))
    (let ((pos (mark-charpos mark))
          (line (mark-line mark)))
      (declare (fixnum pos))
      (cond (forwardp (scan line pos))
            ((> pos 0) (scan line (1- pos)))
            (t
             (let ((prev (line-previous line)))
               (if prev
                   (scan prev (line-length buffer prev))
                   (values nil nil))))))))


;;; Valid-Spot  --  Public
;;;
(defun valid-spot (buffer mark forwardp)
  "Return true if the character pointed to by Mark is not in a quoted context,
  false otherwise.  If Forwardp is true, we use the next character, otherwise
  we use the previous."
  (multiple-value-bind (region line)
                       (find-ignore-region buffer mark forwardp)
    (and line (not region))))


;;; Scan-Direction-Valid  --  Internal
;;;
;;;    Like scan-direction, but only stop on valid characters.
;;;
(defmacro scan-direction-valid (buffer mark forwardp &rest forms)
  (let ((n-mark (gensym))
        (n-line (gensym))
        (n-region (gensym))
        (n-won (gensym))
        (buffer-sym (gensym)))
    `(let ((,buffer-sym ,buffer)
           (,n-mark ,mark) (,n-won nil))
       (loop
         (multiple-value-bind (,n-region ,n-line)
                              (find-ignore-region ,buffer-sym ,n-mark ,forwardp)
           (unless ,n-line (return nil))
           (if ,n-region
               (move-to-position ,n-mark
                                 ,(if forwardp
                                      `(cdr ,n-region)
                                      `(car ,n-region))
                                 ,n-line)
               (when ,n-won (return t)))
           ;;
           ;; Peculiar condition when a quoting character terminates a line.
           ;; The ignore region is off the end of the line causing %FORM-OFFSET
           ;; to infinitely loop.
           (when (> (mark-charpos ,n-mark) (line-length ,buffer ,n-line))
             (line-offset ,buffer-sym ,n-mark 1 0))
           (unless (scan-direction ,buffer-sym ,n-mark ,forwardp ,@forms)
             (return nil))
           (setq ,n-won t))))))


;;;; List offseting.

;;; %LIST-OFFSET allows for BACKWARD-LIST and FORWARD-LIST to be built
;;; with the same existing structure, with the altering of one variable.
;;; This one variable being FORWARDP.
;;;
(defmacro %list-offset (buffer actual-mark forwardp &key (extra-parens 0) )
  "Expand to code that will go forward one list either backward or forward,
   according to the FORWARDP flag."
  (let ((mark (gensym))
        (buffer-sym (gensym)))
    `(let ((,buffer-sym ,buffer)
           (paren-count ,extra-parens))
       (declare (fixnum paren-count))
       (with-mark ((,mark ,actual-mark))
         (loop
           (scan-direction ,buffer-sym ,mark ,forwardp :lisp-syntax
                           (or :close-paren :open-paren :newline))
           (let ((ch (direction-char ,buffer-sym ,mark ,forwardp)))
             (unless ch (return nil))
             (when (valid-spot ,buffer-sym ,mark ,forwardp)
               (case (character-attribute :lisp-syntax ch)
                 (:close-paren
                  (decf paren-count)
                  ,(when forwardp
                     ;; When going forward, an unmatching close-paren means the
                     ;; end of list.
                     `(when (<= paren-count 0)
                        (neighbor-mark ,buffer-sym ,mark ,forwardp)
                        (move-mark ,actual-mark ,mark)
                        (return t))))
                 (:open-paren
                  (incf paren-count)
                  ,(unless forwardp             ; Same as above only end of list
                     `(when (>= paren-count 0)  ; is opening parens.
                        (neighbor-mark ,buffer-sym ,mark ,forwardp)
                        (move-mark ,actual-mark ,mark)
                        (return t))))

                 (:newline
                  ;; When a #\Newline is hit, then the matching paren must lie
                  ;; on some other line so drop down into the multiple line
                  ;; balancing function: QUEST-FOR-BALANCING-PAREN If no paren
                  ;; seen yet, keep going.
                  (cond ((zerop paren-count))
                        ((quest-for-balancing-paren ,buffer-sym ,mark paren-count ,forwardp)
                         (move-mark ,actual-mark ,mark)
                         (return t))
                        (t
                         (return nil)))))))

           (neighbor-mark ,buffer-sym ,mark ,forwardp))))))

;;;
;;; QUEST-FOR-BALANCING-PAREN

(defmacro quest-for-balancing-paren (buffer mark paren-count forwardp)
  "Expand to a form that finds the the balancing paren for however many opens or
  closes are registered by Paren-Count."
  (let ((buffer-sym (gensym)))
    `(let* ((,buffer-sym ,buffer)
            (line (mark-line ,mark)))
       (loop
         (setq line (neighbor-line line ,forwardp))
         (unless line (return nil))
         (let ((line-info (getf (line-plist line) 'lisp-info))
               (unbal-paren ,paren-count))
           (declare (ignorable unbal-paren))
           (unless line-info (return nil))

           ,(if forwardp
                `(decf ,paren-count (lisp-info-net-close-parens line-info))
                `(incf ,paren-count (lisp-info-net-open-parens line-info)))

           (when ,(if forwardp
                      `(<= ,paren-count 0)
                      `(>= ,paren-count 0))
             ,(if forwardp
                  `(line-start ,mark line)
                  `(line-end ,buffer-sym ,mark line))
             (return (goto-correct-paren-char ,buffer-sym ,mark unbal-paren ,forwardp)))

           ,(if forwardp
                `(incf ,paren-count (lisp-info-net-open-parens line-info))
                `(decf ,paren-count (lisp-info-net-close-parens line-info))))))))


;;;
;;; GOTO-CORRECT-PAREN-CHAR

(defmacro goto-correct-paren-char (buffer mark paren-count forwardp)
  "Expand to a form that will leave MARK on the correct balancing paren matching
   however many are indicated by COUNT."
  (let ((buffer-sym (gensym)))
    `(with-mark ((m ,mark))
       (let ((,buffer-sym ,buffer)
             (count ,paren-count))
         (loop
           (scan-direction ,buffer-sym m ,forwardp :lisp-syntax
                           (or :close-paren :open-paren :newline))
           (when (valid-spot ,buffer-sym m ,forwardp)
             (ecase (character-attribute :lisp-syntax (direction-char ,buffer-sym m ,forwardp))
               (:close-paren
                (decf count)
                ,(when forwardp
                   `(when (zerop count)
                      (neighbor-mark ,buffer-sym m ,forwardp)
                      (move-mark ,mark m)
                      (return t))))

               (:open-paren
                (incf count)
                ,(unless forwardp
                   `(when (zerop count)
                      (neighbor-mark ,buffer-sym m ,forwardp)
                      (move-mark ,mark m)
                      (return t))))))
           (neighbor-mark ,buffer-sym m ,forwardp))))))


(defun list-offset (buffer mark offset)
  (if (plusp offset)
      (dotimes (i offset t)
        (unless (%list-offset buffer mark t) (return nil)))
      (dotimes (i (- offset) t)
        (unless (%list-offset buffer mark nil) (return nil)))))

(defun forward-up-list (buffer mark)
  "Moves mark just past the closing paren of the immediately containing list."
  (%list-offset buffer mark t :extra-parens 1))

(defun backward-up-list (buffer mark)
  "Moves mark just before the opening paren of the immediately containing list."
  (%list-offset buffer mark nil :extra-parens -1))



;;;; Top level form location hacks (open parens beginning lines).

;;; NEIGHBOR-TOP-LEVEL is used only in TOP-LEVEL-OFFSET.
;;;
(eval-when (:compile-toplevel :execute)
  (defmacro neighbor-top-level (buffer line forwardp)
    (let ((buffer-sym (gensym)))
      `(let ((,buffer-sym ,buffer))
         (loop
           (when (test-char (line-character ,buffer-sym ,line 0) :lisp-syntax :open-paren)
             (return t))
           (setf ,line ,(if forwardp `(line-next ,line) `(line-previous ,line)))
           (unless ,line (return nil))))))
) ;eval-when

(defun top-level-offset (buffer mark offset)
  "Go forward or backward offset number of top level forms.  Mark is
   returned if offset forms exists, otherwise nil."
  (declare (fixnum offset))
  (let* ((line (mark-line mark))
         (at-start (test-char (line-character buffer line 0) :lisp-syntax :open-paren)))
    (cond ((zerop offset) mark)
          ((plusp offset)
           (do ((offset (if at-start offset (1- offset))
                        (1- offset)))
               (nil)
             (declare (fixnum offset))
             (unless (neighbor-top-level buffer line t) (return nil))
             (when (zerop offset) (return (line-start mark line)))
             (unless (setf line (line-next line)) (return nil))))
          (t
           (do ((offset (if (and at-start (start-line-p mark))
                            offset
                            (1+ offset))
                        (1+ offset)))
                (nil)
             (declare (fixnum offset))
             (unless (neighbor-top-level buffer line nil) (return nil))
             (when (zerop offset) (return (line-start mark line)))
             (unless (setf line (line-previous line)) (return nil)))))))


(defun mark-top-level-form (buffer mark1 mark2)
  "Moves mark1 and mark2 to the beginning and end of the current or next defun.
   Mark1 one is used as a reference.  The marks may be altered even if
   unsuccessful.  if successful, return mark2, else nil."
  (let ((winp (cond ((inside-defun-p buffer mark1)
                     (cond ((not (top-level-offset buffer mark1 -1)) nil)
                           ((not (form-offset buffer (move-mark mark2 mark1) 1)) nil)
                           (t mark2)))
                    ((start-defun-p buffer mark1)
                     (form-offset buffer (move-mark mark2 mark1) 1))
                    ((and (top-level-offset buffer (move-mark mark2 mark1) -1)
                          (start-defun-p buffer mark2)
                          (form-offset buffer mark2 1)
                          (same-line-p mark1 mark2))
                     (form-offset buffer (move-mark mark1 mark2) -1)
                     mark2)
                    ((top-level-offset buffer mark1 1)
                     (form-offset buffer (move-mark mark2 mark1) 1)))))
    (when winp
      (when (blank-after-p buffer mark2) (line-offset mark2 1 0))
      mark2)))

(defun inside-defun-p (buffer mark)
  "T if the current point is (supposedly) in a top level form."
  (with-mark ((m mark))
    (when (top-level-offset buffer m -1)
      (form-offset buffer m 1)
      (mark> m mark))))

(defun start-defun-p (buffer mark)
  "Returns t if mark is sitting before an :open-paren at the beginning of a
   line."
  (and (start-line-p mark)
       (test-char (next-character buffer mark) :lisp-syntax :open-paren)))



;;;; Form offseting.

(defmacro %form-offset (buffer mark forwardp)
  (let ((buffer-sym (gensym)))
    `(let ((,buffer-sym ,buffer))
       (with-mark ((m ,mark))
         (when (scan-direction-valid ,buffer-sym m ,forwardp :lisp-syntax
                                     (or :open-paren :close-paren
                                         :char-quote :string-quote
                                         :constituent))
           (ecase (character-attribute :lisp-syntax (direction-char ,buffer-sym m ,forwardp))
             (:open-paren
              (when ,(if forwardp `(list-offset ,buffer-sym m 1) `(mark-before ,buffer-sym m))
                ,(unless forwardp
                   `(scan-direction ,buffer-sym m nil :lisp-syntax (not :prefix)))
                (move-mark ,mark m)
                t))
             (:close-paren
              (when ,(if forwardp `(mark-after ,buffer-sym m) `(list-offset ,buffer-sym m -1))
                ,(unless forwardp
                   `(scan-direction ,buffer-sym m nil :lisp-syntax (not :prefix)))
                (move-mark ,mark m)
                t))
             ((:constituent :char-quote)
              (scan-direction-valid ,buffer-sym m ,forwardp :lisp-syntax
                                    (not (or :constituent :char-quote)))
              ,(if forwardp
                   `(scan-direction-valid ,buffer-sym m t :lisp-syntax
                                          (not (or :constituent :char-quote)))
                   `(scan-direction-valid ,buffer-sym m nil :lisp-syntax
                                          (not (or :constituent :char-quote
                                                   :prefix))))
              (move-mark ,mark m)
              t)
             (:string-quote
              (cond ((valid-spot ,buffer-sym m ,(not forwardp))
                     (neighbor-mark ,buffer-sym m ,forwardp)
                     (when (scan-direction-valid ,buffer-sym m ,forwardp :lisp-syntax
                                                 :string-quote)
                       (neighbor-mark ,buffer-sym m ,forwardp)
                       (move-mark ,mark m)
                       t))
                    (t (neighbor-mark ,buffer-sym m ,forwardp)
                       (move-mark ,mark m)
                       t)))))))))


(defun form-offset (buffer mark offset)
  "Move mark offset number of forms, after if positive, before if negative.
   Mark is always moved.  If there weren't enough forms, returns nil instead of
   mark."
  (if (plusp offset)
      (dotimes (i offset t)
        (unless (%form-offset buffer mark t) (return nil)))
      (dotimes (i (- offset) t)
        (unless (%form-offset buffer mark nil) (return nil)))))



(macrolet ((frob (step end)
             `(let ((min (value minimum-lines-parsed))
                    (max (value maximum-lines-parsed))
                    (goal (value defun-parse-goal))
                    (last-defun nil))
                (declare (fixnum min max goal))
                (do ((line (mark-line mark) (,step line))
                     (count 0 (1+ count)))
                    ((null line)
                     (,end mark))
                  (declare (fixnum count))
                  (when (char= (line-character (line-buffer line) line 0) #\()
                    (setq last-defun line)
                    (decf goal)
                    (when (and (<= goal 0) (>= count min))
                      (line-start mark line)
                      (return)))
                  (when (> count max)
                    (line-start mark (or last-defun line))
                    (return))))))

;;;; Table of special forms with special indenting requirements.

	       

  (defun start-of-parse-block (mark)
    (frob line-previous buffer-start))

  (defun end-of-parse-block (mark)
    (frob line-next buffer-end)))

(defvar *special-forms* (make-hash-table :test #'equal))

(defun defindent (fname args)
  "Define Fname to have Args special arguments.  If args is null then remove
   any special arguments information."
  (check-type fname string)
  (let ((fname (string-upcase fname)))
    (cond ((null args) (remhash fname *special-forms*))
          (t
           (check-type args integer)
           (setf (gethash fname *special-forms*) args)))))


;;; Hemlock forms.
;;;
(defindent "with-mark" 1)
(defindent "with-random-typeout" 1)
(defindent "with-pop-up-display" 1)
(defindent "defhvar" 1)
(defindent "hlet" 1)
(defindent "defcommand" 2)
(defindent "defattribute" 1)
(defindent "command-case" 1)
(defindent "with-input-from-region" 1)
(defindent "with-output-to-mark" 1)
(defindent "with-output-to-window" 1)
(defindent "do-strings" 1)
(defindent "save-for-undo" 1)
(defindent "do-alpha-chars" 1)
(defindent "do-headers-buffers" 1)
(defindent "do-headers-lines" 1)
(defindent "with-headers-mark" 1)
(defindent "frob" 1) ;cover silly FLET and MACROLET names for Rob and Bill.
(defindent "with-writable-buffer" 1)

;;; Common Lisp forms.
;;;
(defindent "block" 1)
(defindent "case" 1)
(defindent "catch" 1)
(defindent "ccase" 1)
(defindent "compiler-let" 1)
(defindent "ctypecase" 1)
(defindent "defconstant" 1)
(defindent "define-compiler-macro" 2)
(defindent "define-setf-method" 2)
(defindent "destructuring-bind" 2)
(defindent "defmacro" 2)
(defindent "defpackage" 1)
(defindent "defparameter" 1)
(defindent "defstruct" 1)
(defindent "deftype" 2)
(defindent "defun" 2)
(defindent "defvar" 1)
(defindent "do" 2)
(defindent "do*" 2)
(defindent "do-all-symbols" 1)
(defindent "do-external-symbols" 1)
(defindent "do-symbols" 1)
(defindent "dolist" 1)
(defindent "dotimes" 1)
(defindent "ecase" 1)
(defindent "etypecase" 1)
(defindent "eval-when" 1)
(defindent "flet" 1)
(defindent "labels" 1)
(defindent "lambda" 1)
(defindent "let" 1)
(defindent "let*" 1)
(defindent "locally" 0)
(defindent "loop" 0)
(defindent "macrolet" 1)
(defindent "multiple-value-bind" 2)
(defindent "multiple-value-call" 1)
(defindent "multiple-value-prog1" 1)
(defindent "multiple-value-setq" 1)
(defindent "prog1" 1)
(defindent "progv" 2)
(defindent "progn" 0)
(defindent "typecase" 1)
(defindent "unless" 1)
(defindent "unwind-protect" 1)
(defindent "when" 1)
(defindent "with-input-from-string" 1)
(defindent "with-open-file" 1)
(defindent "with-open-stream" 1)
(defindent "with-output-to-string" 1)
(defindent "with-package-iterator" 1)

;;; Error/condition system forms.
;;;
(defindent "define-condition" 2)
(defindent "handler-bind" 1)
(defindent "handler-case" 1)
(defindent "restart-bind" 1)
(defindent "restart-case" 1)
(defindent "with-simple-restart" 1)
;;; These are for RESTART-CASE branch formatting.
(defindent "store-value" 1)
(defindent "use-value" 1)
(defindent "muffle-warning" 1)
(defindent "abort" 1)
(defindent "continue" 1)

;;; Debug-internals forms.
;;;
(defindent "do-debug-function-blocks" 1)
(defindent "di:do-debug-function-blocks" 1)
(defindent "do-debug-function-variables" 1)
(defindent "di:do-debug-function-variables" 1)
(defindent "do-debug-block-locations" 1)
(defindent "di:do-debug-block-locations" 1)
;;;
;;; Debug-internals conditions
;;; (define these to make uses of HANDLER-CASE indent branches correctly.)
;;;
(defindent "debug-condition" 1)
(defindent "di:debug-condition" 1)
(defindent "no-debug-info" 1)
(defindent "di:no-debug-info" 1)
(defindent "no-debug-function-returns" 1)
(defindent "di:no-debug-function-returns" 1)
(defindent "no-debug-blocks" 1)
(defindent "di:no-debug-blocks" 1)
(defindent "lambda-list-unavailable" 1)
(defindent "di:lambda-list-unavailable" 1)
(defindent "no-debug-variables" 1)
(defindent "di:no-debug-variables" 1)
(defindent "invalid-value" 1)
(defindent "di:invalid-value" 1)
(defindent "ambiguous-variable-name" 1)
(defindent "di:ambiguous-variable-name" 1)
(defindent "debug-error" 1)
(defindent "di:debug-error" 1)
(defindent "unhandled-condition" 1)
(defindent "di:unhandled-condition" 1)
(defindent "unknown-code-location" 1)
(defindent "di:unknown-code-location" 1)
(defindent "unknown-debug-variable" 1)
(defindent "di:unknown-debug-variable" 1)
(defindent "invalid-control-stack-pointer" 1)
(defindent "di:invalid-control-stack-pointer" 1)
(defindent "frame-function-mismatch" 1)
(defindent "di:frame-function-mismatch" 1)

;;; Xlib forms.
;;;
(defindent "with-gcontext" 1)
(defindent "xlib:with-gcontext" 1)
(defindent "with-state" 1)
(defindent "xlib:with-state" 1)
(defindent "with-display" 1)
(defindent "xlib:with-display" 1)
(defindent "with-event-queue" 1)
(defindent "xlib:with-event-queue" 1)
(defindent "with-server-grabbed" 1)
(defindent "xlib:with-server-grabbed" 1)
(defindent "event-case" 1)
(defindent "xlib:event-case" 1)

;;; CLOS forms.
;;;
(defindent "with-slots" 1)
(defindent "with-slots*" 2) ; obsolete
(defindent "with-accessors" 2)
(defindent "with-accessors*" 2) ; obsolete
(defindent "defclass" 2)
(defindent "print-unreadable-object" 1)

;;; System forms.
;;;
(defindent "alien-bind" 1)
(defindent "def-c-record" 1)
(defindent "defrecord" 1)
(defindent "add-fd-handler" 2)
(defindent "with-fd-handler" 1)

;;; Wire forms.
(defindent "remote" 1)
(defindent "wire:remote" 1)
(defindent "remote-value" 1)
(defindent "wire:remote-value" 1)
(defindent "remote-value-bind" 3)
(defindent "wire:remote-value-bind" 3)

;;; Multiprocessing forms.
(defindent "with-lock-held" 1)
(defindent "process-wait" 1)

;;; Alien forms.
(defindent "with-alien" 1)

;;;
(defindent "with-gensyms" 1)
(defindent "with-slots" 2)


;;;; Indentation.

;;; LISP-INDENTATION -- Internal Interface.
;;;
(defun lisp-indentation (buffer mark)
  "Compute number of spaces which mark should be indented according to
   local context and lisp grinding conventions.  This assumes mark is at the
   beginning of the line to be indented."
  (with-buffer-globals (buffer)
    (with-mark ((m mark)
                (temp mark))
      ;; See if we are in a quoted context.
      (unless (valid-spot buffer m nil)
        (return-from lisp-indentation (lisp-generic-indentation buffer m)))
      ;; Look for the paren that opens the containing form.
      (unless (backward-up-list buffer m)
        (return-from lisp-indentation 0))
      ;; Move after the paren, save the start, and find the form name.
      (mark-after buffer m)
      (with-mark ((start m))
        (unless (and (scan-char buffer m :lisp-syntax
                                (not (or :space :prefix :char-quote)))
                     (test-char (next-character buffer m) :lisp-syntax :constituent))
          (return-from lisp-indentation (mark-column buffer start)))
        (with-mark ((fstart m))
          (scan-char buffer m :lisp-syntax (not :constituent))
          (let* ((fname (nstring-upcase (region-to-string buffer (region fstart m))))
                 (special-args (or (gethash fname *special-forms*)
                                   (and (> (length fname) 2)
                                        (string= fname "DEF" :end1 3)
                                        (value indent-defanything)))))
            (declare (simple-string fname))
            ;; Now that we have the form name, did it have special syntax?
            (cond (special-args
                   (with-mark ((spec m))
                     (cond ((and (form-offset buffer spec special-args)
                                 (mark<= spec mark))
                            (1+ (mark-column buffer start)))
                           ((skip-valid-space buffer m)
                            (mark-column buffer m))
                           (t
                            (+ (mark-column buffer start) 3)))))
                  ;; See if the user seems to have altered the editor's
                  ;; indentation, and if so, try to adhere to it.  This usually
                  ;; happens when you type in a quoted list constant that line
                  ;; wraps.  You want all the items on successive lines to fall
                  ;; under the first character after the opening paren, not as if
                  ;; you are calling a function.
                  ((and (form-offset buffer temp -1)
                        (or (blank-before-p buffer temp) (not (same-line-p temp fstart)))
                        (not (same-line-p temp mark)))
                   (unless (blank-before-p buffer temp)
                     (line-start temp)
                     (find-attribute buffer temp :space #'zerop))
                   (mark-column buffer temp))
                  ;; Appears to be a normal form.  Is the first arg on the same
                  ;; line as the form name?
                  ((skip-valid-space buffer m)
                   (or (lisp-indentation-check-for-local-def
                        buffer
                        mark temp fstart start t)
                       (mark-column buffer m)))
                  ;; Okay, fall under the first character after the opening paren.
                  (t
                   (or (lisp-indentation-check-for-local-def
                        buffer
                        mark temp fstart start nil)
                       (mark-column buffer start))))))))))



;;; LISP-INDENTATION-CHECK-FOR-LOCAL-DEF -- Internal.
;;;
;;; This is a temporary hack to see how it performs.  When we are indenting
;;; what appears to be a function call, let's look for FLET or MACROLET to see
;;; if we really are indenting a local definition.  If we are, return the
;;; indentation for a DEFUN; otherwise, nil
;;;
;;; Mark is the argument to LISP-INDENTATION.  Start is just inside the paren
;;; of what looks like a function call.  If we are in an FLET, arg-list
;;; indicates whether the local function's arg-list has been entered, that is,
;;; whether we need to normally indent for a DEFUN body or indent specially for
;;; the arg-list.
;;;
(defun lisp-indentation-check-for-local-def (buffer mark temp1 temp2 start arg-list)
  ;; We know this succeeds from LISP-INDENTATION.
  (backward-up-list buffer (move-mark temp1 mark)) ;Paren for local definition.
  (cond ((and (backward-up-list buffer temp1)      ;Paren opening the list of defs
              (form-offset buffer (move-mark temp2 temp1) -1)
              (mark-before buffer temp2)
              (backward-up-list buffer temp1)      ;Paren for FLET or MACROLET.
              (mark= temp1 temp2))          ;Must be in first arg form.
         ;; See if the containing form is named FLET or MACROLET.
         (mark-after buffer temp1)
         (unless (and (scan-char buffer temp1 :lisp-syntax
                                 (not (or :space :prefix :char-quote)))
                      (test-char (next-character buffer temp1) :lisp-syntax
                                 :constituent))
           (return-from lisp-indentation-check-for-local-def nil))
         (move-mark temp2 temp1)
         (scan-char buffer temp2 :lisp-syntax (not :constituent))
         (let ((fname (nstring-upcase (region-to-string buffer (region temp1 temp2)))))
           (with-buffer-globals (buffer)
             (cond ((not (member fname (value lisp-indentation-local-definers)
                                 :test #'string=))
                    nil)
                   (arg-list
                    (1+ (.mark-column. start)))
                   (t
                    (+ (.mark-column. start) 3))))))))

;;; LISP-GENERIC-INDENTATION -- Internal.
;;;
;;; LISP-INDENTATION calls this when mark is in a invalid spot, or quoted
;;; context.  If we are inside a string, we return the column one greater
;;; than the opening double quote.  Otherwise, we just use the indentation
;;; of the first preceding non-blank line.
;;;
(defun lisp-generic-indentation (buffer mark)
  (with-mark ((m mark))
    (form-offset buffer m -1)
    (with-buffer-globals (buffer)
      (cond ((eq (character-attribute :lisp-syntax (next-character buffer m))
                 :string-quote)
             (1+ (.mark-column. m)))
            (t
             (let* ((line (mark-line mark))
                    (prev (do ((line (line-previous line) (line-previous line)))
                              ((not (and line (blank-line-p buffer line))) line))))
               (cond (prev
                      (line-start mark prev)
                      (find-attribute buffer mark :space #'zerop)
                      (.mark-column. mark))
                     (t 0))))))))

;;; Skip-Valid-Space  --  Internal
;;;
;;;    Skip over any space on the line Mark is on, stopping at the first valid
;;; non-space character.  If there is none on the line, return nil.
;;;
(defun skip-valid-space (buffer mark)
  (loop
    (scan-char buffer mark :lisp-syntax (not :space))
    (let ((val (character-attribute :lisp-syntax
                                    (next-character buffer mark))))
      (cond ((eq val :newline) (return nil))
            ((valid-spot buffer mark t) (return mark))))
    (mark-after buffer mark)))

;; (declaim (optimize (speed 0))); byte compile again


;;;; Indentation commands and hook functions.

(defcommand "Defindent" (window p)
    "Define the Lisp indentation for the current function.
  The indentation is a non-negative integer which is the number
  of special arguments for the form.  Examples: 2 for Do, 1 for Dolist.
  If a prefix argument is supplied, then delete the indentation information."
    "Do a defindent, man!"
  (let ((buffer (window-buffer window)))
    (with-mark ((m (buffer-point buffer)))
      (pre-command-parse-check buffer m)
      (unless (backward-up-list buffer m) (editor-error))
      (mark-after buffer m)
      (with-mark ((n m))
        (scan-char buffer n :lisp-syntax (not :constituent))
        (let ((s (region-to-string buffer (region m n))))
          (declare (simple-string s))
          (when (zerop (length s)) (editor-error))
          (if p
              (defindent s nil)
              (let ((i (prompt-for-integer
                        window
                        :prompt (format nil "Indentation for ~A: " s)
                        :help "Number of special arguments.")))
                (when (minusp i)
                  (editor-error "Indentation must be non-negative."))
                (defindent s i))))))
    (indent-command window nil)))

(defcommand "Indent Form" (window p)
  "Indent Lisp code in the next form."
  "Indent Lisp code in the next form."
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (pre-command-parse-check buffer point)
    (with-mark ((m point))
      (unless (form-offset buffer m 1) (editor-error))
      (lisp-indent-region window buffer (region point m) "Indent Form"))))

;;; LISP-INDENT-REGION -- Internal.
;;;
;;; This indents a region of Lisp code without doing excessive redundant
;;; computation.  We parse the entire region once, then scan through doing
;;; indentation on each line.  We forcibly reparse each line that we indent so
;;; that the list operations done to determine indentation of subsequent lines
;;; will work.  This is done undoably with save1, save2, buf-region, and
;;; undo-region.
;;;
(defun lisp-indent-region (current-window buffer region &optional (undo-text "Lisp region indenting"))
  (check-region-query-size current-window region)
  (let ((start (region-start region))
        (end (region-end region)))
    (with-buffer-globals (buffer)
      (with-mark ((m1 start)
                  (m2 end))
        (funcall (value parse-start-function) m1)
        (funcall (value parse-end-function) m2)
        (parse-over-block buffer (mark-line m1) (mark-line m2)))
    (let* ((first-line (mark-line start))
           (last-line (mark-line end))
           (prev (line-previous first-line))
           (prev-line-info
            (and prev (getf (line-plist prev) 'lisp-info)))
           (save1 (line-start (copy-mark start :right-inserting)))
           (save2 (line-end buffer (copy-mark end :left-inserting)))
           (buf-region (region save1 save2))
           (undo-region (copy-region buffer buf-region)))
      (with-mark ((bol start :left-inserting))
        (do ((line first-line (line-next line)))
            (nil)
          (line-start bol line)
          (.insert-lisp-indentation. bol)
          (let ((line-info (getf (line-plist line) 'lisp-info)))
            (parse-lisp-line-info buffer bol line-info prev-line-info)
            (setq prev-line-info line-info))
          (when (eq line last-line) (return nil))))
      (make-region-undo :twiddle undo-text buf-region undo-region)))))

;;; INDENT-FOR-LISP -- Internal.
;;;
;;; This is the value of "Indent Function" for "Lisp" mode.
;;;
(defun indent-for-lisp (buffer mark)
  (line-start mark)
  (pre-command-parse-check buffer mark)
  (insert-lisp-indentation buffer mark))





;;;; Most "Lisp" mode commands.

(defcommand "Beginning of Defun" (window p)
  "Move the point to the beginning of a top-level form.
  with an argument, skips the previous p top-level forms."
    "Move the point to the beginning of a top-level form."

  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (count (or p 1)))
    (pre-command-parse-check buffer point)
      (if (minusp count)
          (end-of-defun-command window (- count))
          (unless (top-level-offset buffer point (- count))
            (editor-error))))
  (update-window-image window))
    

;;; "End of Defun", with a positive p (the normal case), does something weird.
;;; Get a mark at the beginning of the defun, and then offset it forward one
;;; less top level form than we want.  This sets us up to use FORM-OFFSET which
;;; allows us to leave the point immediately after the defun.  If we used
;;; TOP-LEVEL-OFFSET one less than p on the mark at the end of the current
;;; defun, point would be left at the beginning of the p+1'st form instead of
;;; at the end of the p'th form.
;;;
(defcommand "End of Defun" (window p)
  "Move the point to the end of a top-level form.
   With an argument, skips the next p top-level forms."
  "Move the point to the end of a top-level form."
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
        (count (or p 1)))
    (pre-command-parse-check buffer point)
    (if (minusp count)
        (beginning-of-defun-command window (- count))
        (with-mark ((m point)
                    (dummy point))
          (cond ((not (mark-top-level-form buffer m dummy))
                 (editor-error "No current or next top level form."))
                (t
                 (unless (top-level-offset buffer m (1- count))
                   (editor-error "Not enough top level forms."))
                 ;; We might be one unparsed for away.
                 (pre-command-parse-check buffer m)
                 (unless (form-offset buffer m 1)
                   (editor-error "Not enough top level forms."))
                 (when (blank-after-p buffer m) (line-offset m 1 0))
                 (move-mark point m)))))
    (update-window-image window)))

(defcommand "Forward List" (window p)
    "Skip over the next Lisp list.
  With argument, skips the next p lists."
    "Skip over the next Lisp list."
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (count (or p 1)))
    (pre-command-parse-check buffer point)
    (unless (list-offset buffer point count) (editor-error))
    (update-window-image window)))

(defcommand "Backward List" (window p)
    "Skip over the previous Lisp list.
  With argument, skips the previous p lists."
    "Skip over the previous Lisp list."
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (count (- (or p 1))))
    (pre-command-parse-check buffer point)
    (unless (list-offset buffer point count) (editor-error))
    (update-window-image window)))

(defcommand "Forward Form" (window p)
  "Skip over the next Form.
  With argument, skips the next p Forms."
  "Skip over the next Form."
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
        (count (or p 1)))
    (pre-command-parse-check buffer point)
    (unless (form-offset buffer point count) (editor-error))
    (update-window-image window)))
  
(defcommand "Backward Form" (window p)
    "Skip over the previous Form.
  With argument, skips the previous p Forms."
    "Skip over the previous Form."
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (count (- (or p 1))))
    (pre-command-parse-check buffer point)
    (unless (form-offset buffer point count) (editor-error))
    (update-window-image window)))

(defcommand "Mark Form" (window p)
    "Set the mark at the end of the next Form.
   With a positive argument, set the mark after the following p
   Forms. With a negative argument, set the mark before
   the preceding -p Forms."
    "Set the mark at the end of the next Form."
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (with-mark ((m point))
      (pre-command-parse-check buffer m)
      (let ((count (or p 1))
            (mark (push-buffer-mark (copy-mark m) t)))
        (if (form-offset buffer m count)
            (move-mark mark m)
            (editor-error))))
    (update-window-image window)))

(defcommand "Mark Defun" (window p)
    "Puts the region around the next or containing top-level form.
   The point is left before the form and the mark is placed immediately
   after it."
    "Puts the region around the next or containing top-level form."
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (pre-command-parse-check buffer point)
    (with-mark ((start point)
                (end point))
      (cond ((not (mark-top-level-form buffer start end))
             (editor-error "No current or next top level form."))
            (t
             (move-mark point start)
             (move-mark (push-buffer-mark (copy-mark point) t) end))))
    (update-window-image window)))

(defcommand "Forward Kill Form" (window p)
    "Kill the next Form.
   With a positive argument, kills the next p Forms.
   Kills backward with a negative argument."
    "Kill the next Form."
  (let ((buffer (window-buffer window)))
    (when buffer
      (let ((point (buffer-point buffer)))
        (with-mark ((m1 point)
                    (m2 point))
          (pre-command-parse-check buffer m1)
          (let ((count (or p 1)))
            (unless (form-offset buffer m1 count) (editor-error))
            (if (minusp count)
                (kill-region buffer (region m1 m2) :kill-backward)
                (kill-region buffer (region m2 m1) :kill-forward))))))
    (krma.gui:redraw-buffer window)))

(defcommand "Backward Kill Form" (window p)
  "Kill the previous Form.
  With a positive argument, kills the previous p Forms.
  Kills forward with a negative argument."
  "Kill the previous Form."
  (forward-kill-form-command window (- (or p 1))))

(defcommand "Extract Form" (window p)
  "Replace the current containing list with the next form.  The entire affected
   area is pushed onto the kill ring.  If an argument is supplied, that many
   upward levels of list nesting is replaced by the next form."
  "Replace the current containing list with the next form.  The entire affected
   area is pushed onto the kill ring.  If an argument is supplied, that many
   upward levels of list nesting is replaced by the next form."
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (pre-command-parse-check buffer point)

    (with-mark ((form-start point :right-inserting)
                (form-end point))

      (unless (form-offset buffer form-end 1) (editor-error))
      (form-offset buffer (move-mark form-start form-end) -1)

      (with-mark ((containing-start form-start :left-inserting)
                  (containing-end form-end :left-inserting))
        (dotimes (i (or p 1))
          (unless (and (forward-up-list buffer containing-end)
                       (backward-up-list buffer containing-start))
            (editor-error)))
        (let ((r (copy-region buffer (region form-start form-end))))
          (ring-push (delete-and-save-region
                      buffer
                      (region containing-start containing-end))
                     *kill-ring*)
          (ninsert-region buffer point r)
          (move-mark point form-start))))
    (krma.gui:redraw-buffer window)))

(defcommand "Extract List" (window p)
    "Extract the current list.
  The current list replaces the surrounding list.  The entire affected
  area is pushed on the kill-ring.  With prefix argument, remove that
  many surrounding lists."
    "Replace the P containing lists with the current one."
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (pre-command-parse-check buffer point)
    (with-mark ((lstart point :right-inserting)
                (lend point))
      (if (eq (character-attribute :lisp-syntax (next-character buffer lstart))
              :open-paren)
          (mark-after buffer lend)
          (unless (backward-up-list buffer lstart) (editor-error)))
      (unless (forward-up-list buffer lend) (editor-error))
      (with-mark ((rstart lstart)
                  (rend lend))
        (dotimes (i (or p 1))
          (unless (and (forward-up-list buffer rend) (backward-up-list buffer rstart))
            (editor-error)))
        (let ((r (copy-region buffer (region lstart lend))))
          (ring-push (delete-and-save-region buffer (region rstart rend))
                     *kill-ring*)
          (ninsert-region buffer point r)
          (move-mark point lstart))))
    (krma.gui:redraw-buffer window)))

(defcommand "Transpose Forms" (window p)
    "Transpose Forms immediately preceding and following the point.
  With a zero argument, tranposes the Forms at the point and the mark.
  With a positive argument, transposes the Form preceding the point
  with the p-th one following it.  With a negative argument, transposes the
  Form following the point with the p-th one preceding it."
    "Transpose Forms immediately preceding and following the point."
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (count (or p 1)))
    (pre-command-parse-check buffer point)
    (if (zerop count)
        (let ((mark (current-mark buffer)))
          (with-mark ((s1 mark :left-inserting)
                      (s2 point :left-inserting))
            (scan-char buffer s1 :whitespace nil)
            (scan-char buffer s2 :whitespace nil)
            (with-mark ((e1 s1 :right-inserting)
                        (e2 s2 :right-inserting))
              (unless (form-offset buffer e1 1) (editor-error))
              (unless (form-offset buffer e2 1) (editor-error))
              (ninsert-region buffer s1 (delete-and-save-region buffer (region s2 e2)))
              (ninsert-region buffer s2 (delete-and-save-region buffer (region s1 e1))))))
        (let ((fcount (if (plusp count) count 1))
              (bcount (if (plusp count) 1 count)))
          (with-mark ((s1 point :left-inserting)
                      (e2 point :right-inserting))
            (dotimes (i bcount)
              (unless (form-offset buffer s1 -1) (editor-error)))
            (dotimes (i fcount)
              (unless (form-offset buffer e2 1) (editor-error)))
            (with-mark ((e1 s1 :right-inserting)
                        (s2 e2 :left-inserting))
              (unless (form-offset buffer e1 1) (editor-error))
              (unless (form-offset buffer s2 -1) (editor-error))
              (ninsert-region buffer s1 (delete-and-save-region buffer (region s2 e2)))
              (ninsert-region buffer s2 (delete-and-save-region buffer (region s1 e1)))
              (move-mark point s2)))))))


(defcommand "Insert ()" (window count)
    "Insert a pair of parentheses ().  With positive argument, puts
   parentheses around the next COUNT Forms, or previous COUNT forms, if
   COUNT is negative.  The point is positioned after the open parenthesis."
    "Insert a pair of parentheses ()."
  ;; TODO Form navigation is broken, so this is broken too -- it is
  ;; possible to put parens around more forms than there are in current
  ;; expression.  It works by moving past as many forms as there is, and
  ;; then each delimiting paren also counts as a form.
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (pre-command-parse-check buffer point)
    (cond (count
           (when (minusp count)
             (form-offset buffer point count)
             (setq count (- count)))
           (insert-character buffer point #\()
           (with-mark ((m point))
             (unless (form-offset buffer m count)
               (editor-error "Could not find that many forms."))
             (insert-character buffer m #\))))
          ;; The simple case with no prefix argument
          (t
           (insert-character buffer point #\()
           (insert-character buffer point #\))
           (mark-before buffer point)))))


(defcommand "Move Over )" (window p)
    "Move past the next close parenthesis, and start a new line.  Any
   indentation preceding the preceding the parenthesis is deleted, and the
   new line is indented.  If there is only whitespace preceding the close
   paren, the paren is moved to the end of the previous line. With prefix
   argument, this command moves past next closing paren and inserts space."
    "Move past the next close parenthesis, and start a new line."
  ;; TODO This is still not complete, because SCAN-CHAR finds the next
  ;; close-paren, but we need to find the next paren that closes current
  ;; expression.  This will have to be updated when form navigation is
  ;; fixed.
  (when window
    (let ((buffer (window-buffer window)))
      (when buffer
        (let ((point (buffer-point buffer)))
          (pre-command-parse-check buffer point)
          (with-mark ((m point :right-inserting))
            (cond ((scan-char buffer m :lisp-syntax :close-paren)
                   (with-buffer-globals (buffer)
                     (cond ((same-line-p point m)
                            (delete-horizontal-space buffer m))
                           (t
                            (move-mark point m)
                            (reverse-find-attribute point :whitespace #'zerop)
                            (delete-region buffer (region point m))))
                     (cond ((not p)
                            ;; Move to the previous line if current is empty
                            (when (zerop (mark-charpos m))
                              (delete-characters buffer m -1))
                            (mark-after buffer m)
                            (move-mark point m)
                            (indent-new-line-command window 1))
                           (t
                            (mark-after buffer m)
                            (move-mark point m)
                            (insert-character buffer m #\space)))))
                  (t
                   (editor-error "Could not find closing paren.")))))))))


(defcommand "Forward Up List" (window p)
    "Move forward past a one containing )."
    "Move forward past a one containing )."
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (count (or p 1)))
    (pre-command-parse-check buffer point)
    (if (minusp count)
        (backward-up-list-command window (- count))
        (with-mark ((m point))
          (dotimes (i count (move-mark point m))
            (unless (forward-up-list buffer m) (editor-error)))))))


(defcommand "Backward Up List" (window p)
    "Move backward past a one containing (."
    "Move backward past a one containing (."
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (count (or p 1)))
    (pre-command-parse-check buffer point)
    (if (minusp count)
        (forward-up-list-command window (- count))
        (with-mark ((m point))
          (dotimes (i count (move-mark point m))
            (unless (backward-up-list buffer m) (editor-error)))))))


(defcommand "Down List" (window p)
    "Move down a level in list structure.  With positive argument, moves down
   p levels.  With negative argument, moves down backward, but only one
   level."
    "Move down a level in list structure."
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (count (or p 1)))
    (pre-command-parse-check buffer point)
    (with-mark ((m point))
      (cond ((plusp count)
             (loop repeat count
                   do (unless (and (scan-char buffer m :lisp-syntax :open-paren)
                                   (mark-after buffer m))
                        (editor-error))))
            (t
             (unless (and (rev-scan-char m :lisp-syntax :close-paren)
                          (mark-before buffer m))
               (editor-error))))
      (move-mark point m))))



;;;; Filling Lisp comments, strings, and indented text.



(defcommand "Fill Lisp Comment Paragraph" (window p)
  "This fills a flushleft or indented Lisp comment.
   This also fills Lisp string literals using the proper indentation as a
   filling prefix.  When invoked outside of a comment or string, this tries
   to fill all contiguous lines beginning with the same initial, non-empty
   blankspace.  When filling a comment, the current line is used to determine a
   fill prefix by taking all the initial whitespace on the line, the semicolons,
   and any whitespace following the semicolons."
  "Fills a flushleft or indented Lisp comment."
  (declare (ignore p))
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer)))
    (pre-command-parse-check buffer point)
    (with-mark ((start point)
                (end point)
                (m point))
      (let ((commentp (fill-lisp-comment-paragraph-prefix buffer start end)))
        (cond (commentp
               (fill-lisp-comment-or-indented-text buffer start end))
              ((and (not (valid-spot buffer m nil))
                    (form-offset buffer m -1)
                    (eq (character-attribute :lisp-syntax (next-character buffer m))
                        :string-quote))
               (fill-lisp-string buffer m))
              ((or (not (value fill-lisp-comment-paragraph-confirm))
                   (prompt-for-y-or-n
                    window
                    :prompt '("Not in a comment or string.  Fill contiguous ~
                               lines with the same initial whitespace? ")))
               (fill-lisp-comment-or-indented-text buffer start end)))))))

;;; FILL-LISP-STRING -- Internal.
;;;
;;; This fills the Lisp string containing mark as if it had been entered using
;;; Hemlock's Lisp string indentation, "Indent Function" for "Lisp" mode.  This
;;; assumes the area around mark has already been PRE-COMMAND-PARSE-CHECK'ed,
;;; and it ensures the string ends before doing any filling.  This function
;;; is undo'able.
;;;
(defun fill-lisp-string (buffer mark)
  (with-mark ((end mark))
    (unless (form-offset buffer end 1)
      (editor-error "Attempted to fill Lisp string, but it doesn't end?"))
    (let* ((mark (copy-mark mark :left-inserting))
           (end (copy-mark end :left-inserting))
           (string-region (region mark end))
           (undo-region (copy-region buffer string-region))
           (hack (make-empty-region)))
      ;; Generate prefix.
      (indent-to-column buffer (region-end hack) (1+ (mark-column buffer mark)))
      ;; Skip opening double quote and fill string starting on its own line.
      (mark-after buffer mark)
      (insert-character buffer mark #\newline)
      (line-start mark)
      (setf (mark-kind mark) :right-inserting)
      (fill-region string-region (region-to-string buffer hack))
      ;; Clean up inserted prefix on first line, delete inserted newline, and
      ;; move before the double quote for undo.
      (with-mark ((text mark :left-inserting))
        (find-attribute text :whitespace #'zerop)
        (delete-region buffer (region mark text)))
      (delete-characters buffer mark -1)
      (mark-before buffer mark)
      ;; Save undo.
      (make-region-undo :twiddle "Fill Lisp Comment Paragraph"
                        string-region undo-region))))

;;; FILL-LISP-COMMENT-OR-INDENTED-TEXT -- Internal.
;;;
;;; This fills all contiguous lines around start and end containing fill prefix
;;; designated by the region between start and end.  These marks can only be
;;; equal when there is no comment and no initial whitespace.  This is a bad
;;; situation since this function in that situation would fill the entire
;;; buffer into one paragraph.  This function is undo'able.
;;;
(defun fill-lisp-comment-or-indented-text (buffer start end)
  (when (mark= start end)
    (editor-error "This command only fills Lisp comments, strings, or ~
                   indented text, but this line is flushleft."))
  ;;
  ;; Find comment block.
  (let* ((prefix (region-to-string buffer (region start end)))
         (length (length prefix)))
    (declare (simple-string prefix))
    (flet ((frob (mark direction)
             (loop
               (let* ((line (line-string buffer (mark-line mark)))
                      (line-len (length line)))
                 (declare (simple-string line))
                 (unless (string= line prefix :end1 (min line-len length))
                   (when (= direction -1)
                     (unless (same-line-p mark end) (line-offset mark 1 0)))
                   (return)))
               (unless (line-offset mark direction 0)
                 (when (= direction 1) (line-end buffer mark))
                 (return)))))
      (frob start -1)
      (frob end 1))
    ;;
    ;; Do it undoable.
    (let* ((start1 (copy-mark start :right-inserting))
           (end2 (copy-mark end :left-inserting))
           (region (region start1 end2))
           (undo-region (copy-region buffer region)))
      (fill-region region prefix)
      (make-region-undo :twiddle "Fill Lisp Comment Paragraph"
                        region undo-region))))

;;; FILL-LISP-COMMENT-PARAGRAPH-PREFIX -- Internal.
;;;
;;; This sets start and end around the prefix to be used for filling.  We
;;; assume we are dealing with a comment.  If there is no ";", then we try to
;;; find some initial whitespace.  If there is a ";", we make sure the line is
;;; blank before it to eliminate ";"'s in the middle of a line of text.
;;; Finally, if we really have a comment instead of some indented text, we skip
;;; the ";"'s and any immediately following whitespace.  We allow initial
;;; whitespace, so we can fill strings with the same command.
;;;
(defun fill-lisp-comment-paragraph-prefix (buffer start end)
  (line-start start)
  (let ((commentp t)) ; Assumes there's a comment.
    (unless (to-line-comment (line-start end) ";")
      (find-attribute buffer end :whitespace #'zerop)
      #|(when (start-line-p end)
        (editor-error "No comment on line, and no initial whitespace."))|#
      (setf commentp nil))
    (when commentp
      (unless (blank-before-p buffer end)
        (find-attribute buffer (line-start end) :whitespace #'zerop)
        #|(when (start-line-p end)
          (editor-error "Semicolon preceded by unindented text."))|#
        (setf commentp nil)))
    (when commentp
      (find-attribute end :lisp-syntax #'(lambda (x) (not (eq x :comment))))
      (find-attribute end :whitespace #'zerop))
    commentp))



;;;; "Lisp" mode.

(defcommand "LISP Mode" (window p)
  "Put current buffer in LISP mode."
  "Put current buffer in LISP mode."
  (declare (ignore p))
  (let ((buffer (window-buffer window)))
    (setf (buffer-major-mode buffer window) "LISP")))




(defun setup-lisp-mode (buffer)
  (declare (ignorable buffer))
  ;;
  )



;;;; Matching parenthesis display.



(defcommand "Lisp Insert )" (window p)
    "Inserts a \")\" and briefly positions the cursor at the matching \"(\"."
    "Inserts a \")\" and briefly positions the cursor at the matching \"(\"."
  (declare (ignore p))
  (let ((win window))
    (let* ((buffer (window-buffer win))
           (point (buffer-point buffer)))
      (insert-character buffer point #\))
      (pre-command-parse-check buffer point)
      (when (valid-spot buffer point nil)
        (with-mark ((m point))
          (if (list-offset buffer m -1)
              (let ((pause (value paren-pause-period)))
                (if pause
                    (unless (show-mark m win pause)
                      (clear-echo-area)
                      (message "~A" (line-string buffer (mark-line m))))
                    (unless (displayed-p m win)
                      (clear-echo-area)
                      (message "~A" (line-string buffer (mark-line m))))))
              (editor-error)))))))

;;; Since we use paren highlighting in Lisp mode, we do not want paren
;;; flashing too.
;;;

;;;


(defun lisp-open-paren-finder-function (buffer mark)
  (when (eq (character-attribute :lisp-syntax (previous-character buffer mark))
            :close-paren)
    (with-mark ((mark mark))
      (pre-command-parse-check buffer mark)
      (if (not (and (valid-spot buffer mark nil) (list-offset buffer mark -1)))
          (values nil nil)
          (values mark (mark-after buffer (copy-mark mark)))))))




