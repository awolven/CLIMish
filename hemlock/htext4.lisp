;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; More Hemlock Text-Manipulation functions.
;;; Written by Skef Wholey and Rob MacLachlan.
;;; Modified by Bill Chiles.
;;;
;;; The code in this file implements the delete and copy functions in the
;;; "Doing Stuff and Going Places" chapter of the Hemlock Design document.
;;;

(in-package :hemlock-internals)


;;;; DELETE-CHARACTERS.

;;(defvar *internal-temp-region* (make-empty-region)) ;; need to put this in buffer
;;(defvar *internal-temp-mark* (internal-make-mark nil nil :temporary)) ;; ditto


;;;; DELETE-REGION.

(defmacro .delete-region. (region)
  "Deletes the Region."
  (let ((region-sym (gensym "DELETE-REGION_REGION")))
    `(let* ((,region-sym ,region)
            (start (region-start ,region-sym))
            (end (region-end ,region-sym))
            (first-line (mark-line start))
            (last-line (mark-line end))
            (first-charpos (mark-charpos start))
            (last-charpos (mark-charpos end))
            (buffer (line-%buffer first-line)))
       (unless (and (eq first-line last-line)
                    (= first-charpos last-charpos))
         (modifying-buffer buffer
           (cond ((eq first-line last-line)
                  ;; Simple case -- just skip over the characters:
                  (.modifying-line. first-line start)
                  (let ((num (- last-charpos first-charpos)))
                    (setq right-open-pos (+ right-open-pos num))
                    ;; and fix up any marks in there:
                    (move-some-marks (charpos first-line)
                      (if (> charpos first-charpos)
                          (if (<= charpos last-charpos)
                              first-charpos
                              (- charpos num))
                          charpos))))
                 (t
                  ;; hairy case -- squish lines together:
                  (.close-line.)
                  (let* ((first-chars (line-chars first-line))
                         (last-chars (line-chars last-line))
                         (last-length (length last-chars)))
                    (declare (simple-string last-chars first-chars))
                    ;; Cons new chars for the first line.
                    (let* ((length (+ first-charpos (- last-length last-charpos)))
                           (new-chars (make-string length)))
                      (%sp-byte-blt first-chars 0 new-chars 0 first-charpos)
                      (%sp-byte-blt last-chars last-charpos new-chars first-charpos
                                    length)
                      (setf (line-chars first-line) new-chars))
                    ;; fix up the first line's marks:
                    (move-some-marks (charpos first-line)
                      (if (> charpos first-charpos)
                          first-charpos
                          charpos))
                    ;; fix up the marks of the lines in the middle and mash
                    ;;line-%buffer:
                    (do* ((line (line-next first-line) (line-next line))
                          #+NIL
                          (count (incf *disembodied-buffer-counter*)))
                         ((eq line last-line)
                          (setf (line-%buffer last-line) .buffer.

                                #+NIL(make-instance 'disembodied-buffer
                                                    :id count)))
                      (setf (line-%buffer line) .buffer.

                            #+NIL(make-instance 'disembodied-buffer
                                                :id count))
                      (move-some-marks (ignore line first-line)
                        (declare (ignore ignore))
                        first-charpos))
                    ;; and fix up the last line's marks:
                    (move-some-marks (charpos last-line first-line)
                      (if (<= charpos last-charpos)
                          first-charpos
                          (+ (- charpos last-charpos)
                             first-charpos)))
                    ;; And splice the losers out:
                    (let ((next (line-next last-line)))
                      (setf (line-next first-line) next)
                      (when next (setf (line-previous next) first-line)))))))))))

(defmethod delete-region-1 (buffer region)
  (with-buffer-globals (buffer)
    (.delete-region. region)))

(defmethod delete-region (region)
  (let ((start-buffer (line-%buffer (mark-line (region-start region))))
        (end-buffer (line-%buffer (mark-line (region-end region)))))
    (when (neq start-buffer end-buffer)
      (warn "buffers do not agree"))
    (delete-region-1 start-buffer region)))


(defmacro .delete-characters. (mark &optional (n 1))
  "Deletes N characters after the mark (or -N before if N is negative)."
  (let ((mark-sym (gensym "DELETE-CHARACTERS_MARK"))
        (n-sym (gensym "DELETE-CHARACTERS_N")))
    `(let* ((,mark-sym ,mark)
            (,n-sym ,n)
            (line (mark-line ,mark-sym))
            (charpos (mark-charpos ,mark-sym))
            (length (line-length* line))
            (buffer (line-%buffer line)))
       (cond ((zerop ,n-sym) t)
             ;; Deleting chars on one line, just bump the pointers.
             ((<= 0 (+ charpos ,n-sym) length)
              (modifying-buffer buffer
                (.modifying-line. line ,mark-sym)
                (cond
                  ((minusp ,n-sym)
                   (setq left-open-pos (+ left-open-pos ,n-sym))
                   (move-some-marks (pos line)
                     (if (> pos left-open-pos)
                         (if (<= pos charpos) left-open-pos (+ pos ,n-sym))
                         pos)))

                  (t
                   (setq right-open-pos (+ right-open-pos ,n-sym))
                   (let ((bound (+ charpos ,n-sym)))
                     (move-some-marks (pos line)
                       (if (> pos charpos)
                           (if (<= pos bound) left-open-pos (- pos ,n-sym))
                           pos)))))
                t))

             ;; Deleting some newlines, punt out to delete-region.
             (t
              (setf (mark-line *internal-temp-mark*) line
                    (mark-charpos *internal-temp-mark*) charpos)
              (let ((other-mark (character-offset *internal-temp-mark* ,n-sym)))
                (cond
                  (other-mark
                   (if (< ,n-sym 0)
                       (setf (region-start *internal-temp-region*) other-mark
                             (region-end *internal-temp-region*) ,mark-sym)
                       (setf (region-start *internal-temp-region*) ,mark-sym
                             (region-end *internal-temp-region*) other-mark))
                   (delete-region-1 .buffer. *internal-temp-region*)
                   t)
                  (t nil))))))))

(defmethod delete-characters-1 (buffer mark &optional (n 1))
  (with-buffer-globals (buffer)
    (.delete-characters. mark n)))

(defmethod delete-characters (mark &optional (n 1))
  (delete-characters-1 (line-%buffer (mark-line mark)) mark n))


;;;; DELETE-AND-SAVE-REGION.

(defmacro .delete-and-save-region. (region)
  "Deletes Region and returns a region containing the deleted characters."
  (let ((region-sym (gensym "DELETE-AND-SAVE-REGION_REGION")))
    `(let* ((,region-sym ,region)
            (start (region-start ,region-sym))
            (end (region-end ,region-sym))
            (first-line (mark-line start))
            (last-line (mark-line end))
            (first-charpos (mark-charpos start))
            (last-charpos (mark-charpos end))
            (buffer (line-%buffer first-line)))
       (cond
         ((and (eq first-line last-line)
               (= first-charpos last-charpos))
          (make-empty-region))
         (t
          (modifying-buffer buffer
            (cond ((eq first-line last-line)
                   ;; simple case -- just skip over the characters:
                   (.modifying-line. first-line start)
                   (let* ((num (- last-charpos first-charpos))
                          (new-right (+ right-open-pos num))
                          (new-chars (make-string num))
                          (new-line (make-line
                                     :chars new-chars  :number 0
                                     :%buffer (make-instance 'disembodied-buffer
                                                         :id (incf *disembodied-buffer-counter*)))))
                     (declare (simple-string new-chars))
                     (%sp-byte-blt open-chars right-open-pos new-chars 0 num)
                     (setq right-open-pos new-right)
                     ;; and fix up any marks in there:
                     (move-some-marks (charpos first-line)
                       (if (> charpos first-charpos)
                           (if (<= charpos last-charpos)
                               first-charpos
                               (- charpos num))
                           charpos))
                     ;; And return the region with the nuked characters:
                     (internal-make-region (mark new-line 0 :right-inserting)
                                           (mark new-line num :left-inserting))))
                  (t
                   ;; hairy case -- squish lines together:
                   (.close-line.)
                   (let* ((first-chars (line-chars first-line))
                          (last-chars (line-chars last-line))
                          (first-length (length first-chars))
                          (last-length (length last-chars))
                          (saved-first-length (- first-length first-charpos))
                          (saved-first-chars (make-string saved-first-length))
                          (saved-last-chars (make-string last-charpos))
                          (count (incf *disembodied-buffer-counter*))
                          (saved-line (make-line :chars saved-first-chars
                                                 :%buffer (make-instance 'disembodied-buffer
                                                                     :id count))))
                     (declare (simple-string first-chars last-chars
                                             saved-first-chars saved-last-chars))
                     ;; Cons new chars for victim line.
                     (let* ((length (+ first-charpos (- last-length last-charpos)))
                            (new-chars (make-string length)))
                       (%sp-byte-blt first-chars 0 new-chars 0 first-charpos)
                       (%sp-byte-blt last-chars last-charpos new-chars first-charpos
                                     length)
                       (setf (line-chars first-line) new-chars))
                     ;; Make a region with all the lost stuff:
                     (%sp-byte-blt first-chars first-charpos saved-first-chars 0
                                   saved-first-length)
                     (%sp-byte-blt last-chars 0 saved-last-chars 0 last-charpos)
                     ;; Mash the chars and buff of the last line.
                     (setf (line-chars last-line) saved-last-chars
                           (line-%buffer last-line) (make-instance 'disembodied-buffer
                                                                   :id count))
                     ;; fix up the marks of the lines in the middle and mash
                     ;;line-%buffer:
                     (do ((line (line-next first-line) (line-next line)))
                         ((eq line last-line)
                          (setf (line-%buffer last-line) (make-instance 'disembodied-buffer
                                                                        :id count)))
                       (setf (line-%buffer line) (make-instance 'disembodied-buffer
                                                                :id count))
                       (move-some-marks (ignore line first-line)
                         (declare (ignore ignore))
                         first-charpos))
                     ;; And splice the losers out:
                     (let ((next (line-next first-line))
                           (after (line-next last-line)))
                       (setf (line-next saved-line) next
                             (line-previous next) saved-line
                             (line-next first-line) after)
                       (when after
                         (setf (line-previous after) first-line
                               (line-next last-line) nil)))

                     ;; fix up the first line's marks:
                     (move-some-marks (charpos first-line)
                       (if (> charpos first-charpos)
                           first-charpos
                           charpos))
                     ;; and fix up the last line's marks:
                     (move-some-marks (charpos last-line first-line)
                       (if (<= charpos last-charpos)
                           first-charpos
                           (+ (- charpos last-charpos)
                              first-charpos)))
                     ;; And return the region with the nuked characters:
                     (renumber-region
                      (internal-make-region
                       (mark saved-line 0 :right-inserting)
                       (mark last-line last-charpos :left-inserting))))))))))))


(defmethod delete-and-save-region-1 (buffer region)
  (with-buffer-globals (buffer)
    (.delete-and-save-region. region)))

(defmethod delete-and-save-region (region)
  (let ((start-buffer (line-%buffer (mark-line (region-start region))))
        (end-buffer (line-%buffer (mark-line (region-end region)))))
    (when (neq start-buffer end-buffer)
      (warn "buffers do not agree"))
    (delete-and-save-region-1 start-buffer region)))


;;;; COPY-REGION.

(defmacro .copy-region. (region)
  "Returns a region containing a copy of the text within Region."
  (let ((region-sym (gensym "COPY-REGION_REGION")))
    `(let* ((,region-sym ,region))
       (let* ((start (region-start ,region-sym))
              (end (region-end ,region-sym))
              (first-line (mark-line start))
              (last-line (mark-line end))
              (first-charpos (mark-charpos start))
              (last-charpos (mark-charpos end))
              (count (incf *disembodied-buffer-counter*)))
         (cond
           ((eq first-line last-line)
            (when (eq first-line open-line) (.close-line.))
            (let* ((length (- last-charpos first-charpos))
                   (chars (make-string length))
                   (line (make-line :chars chars
                                    :%buffer (make-instance 'disembodied-buffer
                                                            :id count)
                                    :number 0)))
              (%sp-byte-blt (line-chars first-line) first-charpos chars 0 length)
              (internal-make-region (mark line 0 :right-inserting)
                                    (mark line length :left-inserting))))
           (t
            (.close-line.)
            (let* ((first-chars (line-chars first-line))
                   (length (- (length first-chars) first-charpos))
                   (chars (make-string length))
                   (first-copied-line (make-line :chars chars
                                                 :%buffer (make-instance 'disembodied-buffer
                                                                         :id count)
                                                 :number 0)))
              (declare (simple-string first-chars))
              (%sp-byte-blt first-chars first-charpos chars 0 length)
              (do ((line (line-next first-line) (line-next line))
                   (previous first-copied-line)
                   (number line-increment (+ number line-increment)))
                  ((eq line last-line)
                   (let* ((chars (make-string last-charpos))
                          (last-copied-line (make-line :chars chars
                                                       :number number
                                                       :%buffer (make-instance 'disembodied-buffer
                                                                               :id count)
                                                       :previous previous)))
                     (%sp-byte-blt (line-chars last-line) 0 chars 0 last-charpos)
                     (setf (line-next previous) last-copied-line)
                     (internal-make-region
                      (mark first-copied-line 0 :right-inserting)
                      (mark last-copied-line last-charpos :left-inserting))))
                (let* ((new-line (%copy-line line :%buffer (make-instance 'disembodied-buffer
                                                                          :id count)
                                                  :number number
                                                  :previous previous)))
                  (setf (line-next previous) new-line)
                  (setq previous new-line))))))))))

(defmethod copy-region-1 (buffer region)
  (with-buffer-globals (buffer)
    (.copy-region. region)))

(defmethod copy-region (region)
  (let ((start-buffer (line-%buffer (mark-line (region-start region))))
        (end-buffer (line-%buffer (mark-line (region-end region)))))
    (when (neq start-buffer end-buffer)
      (warn "buffers do not agree"))
    (copy-region-1 start-buffer region)))



;;;; FILTER-REGION.

;;(eval-when (:compile-toplevel :execute)
(defmacro fcs (fun str)
  `(let ((rs (funcall ,fun ,str)))
     (if (simple-string-p rs) rs
         (coerce rs 'simple-string))))
;;); eval-when

;;; FILTER-REGION  --  Public
;;;
;;;    After we deal with the nasty boundry conditions of the first and
;;; last lines, we just scan through lines in the region replacing their
;;; chars with the result of applying the function to the chars.
;;;
(defmacro .filter-region. (function region)
  "This function filters the text in a region though a Lisp function.  The
   argument function must map from a string to a string.  It is passed each
   line string from region in order, and each resulting string replaces the
   original.  The function must neither destructively modify its argument nor
   modify the result string after it is returned.  The argument will always be
   a simple-string.  It is an error for any string returned to contain
   newlines."
  (let ((function-sym (gensym "FILTER-REGION_FUNCTION"))
        (region-sym (gensym "FILTER-REGION_FUNCTION")))
    `(let* ((,function-sym ,function)
            (,region-sym ,region)
            (start (region-start ,region-sym))
            (start-line (mark-line start))
            (first (mark-charpos start))
            (end (region-end ,region-sym))
            (end-line (mark-line end))
            (last (mark-charpos end))
            (marks ())
            (buffer (line-%buffer start-line)))
       (modifying-buffer buffer
         (.modifying-line. end-line end)
         (cond ((eq start-line end-line)
                (let* ((res (fcs ,function-sym (subseq open-chars first last)))
                       (rlen (length res))
                       (new-left (+ first rlen))
                       (delta (- new-left left-open-pos)))
                  (declare (simple-string res))
                  (when (> new-left right-open-pos)
                    (.grow-open-chars. (+ new-left line-cache-length)))
                  (%sp-byte-blt res 0 open-chars first left-open-pos)
                  ;;
                  ;; Move marks to start or end of region, depending on kind.
                  (dolist (m (line-marks start-line))
                    (let ((charpos (mark-charpos m)))
                      (when (>= charpos first)
                        (setf (mark-charpos m)
                              (if (<= charpos last)
                                  (if (eq (mark-%kind m) :left-inserting)
                                      new-left first)
                                  (+ charpos delta))))))
                  (setq left-open-pos new-left)))
               (t
                ;;
                ;; Do the chars for the first line.
                (let* ((first-chars (line-chars start-line))
                       (first-len (length first-chars))
                       (res (fcs ,function-sym (subseq first-chars first first-len)))
                       (rlen (length res))
                       (nlen (+ first rlen))
                       (new (make-string nlen)))
                  (declare (simple-string res first-chars new))
                  (%sp-byte-blt first-chars 0 new 0 first)
                  (%sp-byte-blt res 0 new first nlen)
                  (setf (line-chars start-line) new))
                ;;
                ;; Fix up marks on the first line, saving any within the region
                ;; to be dealt with later.
                (let ((outside ()))
                  (dolist (m (line-marks start-line))
                    (if (<= (mark-charpos m) first)
                        (push m outside) (push m marks)))
                  (setf (line-marks start-line) outside))
                ;;
                ;; Do chars of intermediate lines in the region, saving their
                ;; marks.
                (do ((line (line-next start-line) (line-next line)))
                    ((eq line end-line))
                  (when (line-marks line)
                    (setq marks (nconc (line-marks line) marks))
                    (setf (line-marks line) nil))
                  (setf (line-chars line) (fcs ,function-sym (line-chars line))))
                ;;
                ;; Do the last line, which is cached.
                (let* ((res (fcs ,function-sym (subseq (the simple-string open-chars)
                                                       0 last)))
                       (rlen (length res))
                       (delta (- rlen last)))
                  (declare (simple-string res))
                  (when (> rlen right-open-pos)
                    (.grow-open-chars. (+ rlen line-cache-length)))
                  (%sp-byte-blt res 0 open-chars 0 rlen)
                  (setq left-open-pos rlen)
                  ;;
                  ;; Adjust marks after the end of the region and save ones in it.
                  (let ((outside ()))
                    (dolist (m (line-marks end-line))
                      (let ((charpos (mark-charpos m)))
                        (cond ((> charpos last)
                               (setf (mark-charpos m) (+ charpos delta))
                               (push m outside))
                              (t
                               (push m marks)))))
                    (setf (line-marks end-line) outside))
                  ;;
                  ;; Scan over saved marks, moving them to the correct end of the
                  ;; region.
                  (dolist (m marks)
                    (cond ((eq (mark-%kind m) :left-inserting)
                           (setf (mark-charpos m) rlen)
                           (setf (mark-line m) end-line)
                           (push m (line-marks end-line)))
                          (t
                           (setf (mark-charpos m) first)
                           (setf (mark-line m) start-line)
                           (push m (line-marks start-line)))))))))
       ,region-sym)))

(defmethod filter-region-1 (buffer function (region region))
  (with-buffer-globals (buffer)
    (.filter-region. function region)))

(defmethod filter-region (function region)
  (let ((start-buffer (line-%buffer (mark-line (region-start region))))
        (end-buffer (line-%buffer (mark-line (region-start region)))))
    (when (neq start-buffer end-buffer)
      (warn "buffers do not agree in filter region"))
    (filter-region-1 start-buffer function region)))

(in-package :hemlock)

(declaim (special *delete-char-region*))

(defvar hemlock::*kill-ring* nil "The Hemlock kill ring.")
(defvar *delete-char-region* nil)
(defvar *delete-char-count* 0)

;;;; KILL-REGION and KILL-CHARACTERS primitives.

(defmacro .kill-char-region. (region current-type)
  (let ((region-sym (gensym "REGION"))
        (current-type-sym (gensym "CURRENT-TYPE")))
    `(let ((,region-sym ,region)
           (,current-type-sym ,current-type))
       (let ((deleted-region (hi::.delete-and-save-region. ,region-sym)))
         (cond ((< *delete-char-count* (value hemlock::character-deletion-threshold))
                (hi::.ninsert-region. (if (eq ,current-type-sym :char-kill-forward)
                                      (region-end *delete-char-region*)
                                      (region-start *delete-char-region*))
                                  deleted-region)
                (setf (last-command-type) ,current-type-sym))
               (t
                (when *delete-char-region*
                  (ring-push *delete-char-region* hemlock::*kill-ring*)
                  (setf *delete-char-region* nil))
                (let ((string (ring-ref hemlock::*kill-ring* 0)))
                  (hi::.insert-string. (region-start deleted-region) string))
                (setf (last-command-type)
                      (if (eq ,current-type-sym :char-kill-forward)
                          :kill-forward
                          :kill-backward))))))))

;;; KILL-CHARACTERS makes sure there are count characters with CHARACTER-OFFSET.
;;; If the last command type was a region kill, we just use the top region
;;; in hemlock::*kill-ring* by making KILL-CHAR-REGION believe *delete-char-count* is
;;; over the threshold.  We don't call KILL-REGION in this case to save making
;;; undo's -- no good reason.  If we were just called, then increment our
;;; global counter.  Otherwise, make an empty region to keep KILL-CHAR-REGION
;;; happy and increment the global counter.
;;;
(defmacro .kill-characters. (mark count)
  "Kills count characters after mark if positive, before mark if negative.
   If called multiple times contiguously such that the sum of the count values
   equals \"Character Deletion Threshold\", then the characters are saved on
   hemlock::*kill-ring*.  This relies on setting LAST-COMMAND-TYPE, and it interacts
   with KILL-REGION.  If there are not count characters in the appropriate
   direction, no characters are deleted, and nil is returned; otherwise, mark
   is returned."
  (let ((mark-sym (gensym "KILL-CHARACTERS_MARK"))
        (count-sym (gensym "KILL-CHARACTERS_COUNT")))
    `(let ((,mark-sym ,mark)
           (,count-sym ,count))
       (if (zerop ,count-sym)
           ,mark-sym
           (with-mark ((temp ,mark-sym :left-inserting))
             (if (character-offset temp ,count-sym)
                 (let ((current-type (if (plusp ,count-sym)
                                         :char-kill-forward
                                         :char-kill-backward))
                       (last-type (last-command-type))
                       (del-region (if (mark< temp ,mark-sym)
                                       (region temp ,mark-sym)
                                       (region ,mark-sym temp))))
                   (cond ((or (eq last-type :kill-forward)
                              (eq last-type :kill-backward))
                          (setf *delete-char-count*
                                (value hemlock::character-deletion-threshold))
                          (setf *delete-char-region* nil))
                         ((or (eq last-type :char-kill-backward)
                              (eq last-type :char-kill-forward))
                          (incf *delete-char-count* (abs ,count-sym)))
                         (t
                          (setf *delete-char-region* (make-empty-region))
                          (setf *delete-char-count* (abs ,count-sym))))
                   (.kill-char-region. del-region current-type)
                   ,mark-sym)
                 nil))))))

(defmacro .kill-region-top-of-ring. (region current-type)
  (let ((region-sym (gensym "KILL-REGION-TOP-OF-RING_REGION"))
        (current-type-sym (gensym "KILL-REGION-TOP-OF-RING_CURRENT-TYPE")))
    `(let ((,region-sym ,region)
           (,current-type-sym ,current-type))
       (let ((string (ring-ref hemlock::*kill-ring* 0))
             (region (hi::.delete-and-save-region. ,region-sym)))
         (hi::.insert-string. (region-start region) string)
         region))))



;;; KILL-REGION first checks for any characters that may need to be added to
;;; the region.  If there are some, we possibly push a region onto hemlock::*kill-ring*,
;;; and we use the top of hemlock::*kill-ring*.  If there are no characters to deal
;;; with, then we make sure the ring isn't empty; if it is, just push our
;;; region.  If there is some region in hemlock::*kill-ring*, then see if the last
;;; command type was a region kill.  Otherwise, just push the region.
;;;
(defmacro .kill-region. (region current-type)
  "Kills the region saving it in hemlock::*kill-ring*.  Current-type is either
   :kill-forward or :kill-backward.  When LAST-COMMAND-TYPE is one of these,
   region is appended or prepended, respectively, to the top of hemlock::*kill-ring*.
   The killing of the region is undo-able with \"Undo\".  LAST-COMMAND-TYPE
   is set to current-type.  This interacts with KILL-CHARACTERS."
  (let ((region-sym (gensym "KILL-REGION_REGION"))
        (current-type-sym (gensym "KILL-REGION_CURRENT-TYPE"))
        (string-sym (gensym "KILL-REGION_STRING")))
    `(let* ((,region-sym ,region)
            (,string-sym)
            (,current-type-sym ,current-type)
            (last-type (last-command-type))
            (insert-mark (copy-mark (region-start ,region-sym) :left-inserting)))
       (cond ((or (eq last-type :char-kill-forward)
                  (eq last-type :char-kill-backward))
              (when *delete-char-region*
                (ring-push *delete-char-region* hemlock::*kill-ring*)
                (setf *delete-char-region* nil))
              (setf ,region-sym (.kill-region-top-of-ring. ,region-sym ,current-type-sym)))
             ((zerop (ring-length hemlock::*kill-ring*))
              (setf ,region-sym (hi::.delete-and-save-region. ,region-sym))
              (ring-push ,region-sym hemlock::*kill-ring*))
             ((or (eq last-type :kill-forward) (eq last-type :kill-backward))
              (setf ,region-sym (.kill-region-top-of-ring. ,region-sym ,current-type-sym)))
             (t
              (setf ,region-sym (hi::.delete-and-save-region. ,region-sym))
              (ring-push ,region-sym hemlock::*kill-ring*)))
       (make-region-undo :insert "kill" (hi::.copy-region. ,region-sym) insert-mark)
       (setf (last-command-type) ,current-type-sym))))
