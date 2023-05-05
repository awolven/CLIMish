;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Killing and unkilling things.
;;;
;;; Written by Bill Chiles and Rob MacLachlan.
;;;

(in-package :hemlock)


;;;; Active Regions.




(defvar *ephemerally-active-command-types* (list :ephemerally-active)
  "This is a list of command types that permit the current region to be active
   for the immediately following command.")

(declaim (inline activate-region deactivate-region region-active-p))

(defun activate-region (buffer)
  "Make the current region active."
  (setf (buffer-active-region-p buffer) (buffer-signature buffer)))

(defun deactivate-region (buffer)
  "Make the current region not active."
  (setf (buffer-active-region-p buffer) nil))

(defun region-active-p (buffer)
  "Returns t or nil, depending on whether the current region is active."
  (or (eql (buffer-active-region-p buffer)
           (buffer-signature buffer))
      (member (last-command-type) *ephemerally-active-command-types*
              :test #'equal)))

(defun check-region-active (buffer)
  "Signals an error when active regions are enabled and the current region
   is not active."
  (when (and (value active-regions-enabled) (not (region-active-p buffer)))
      (editor-error "The current region is not active.")))

(defun current-region (buffer &optional (error-if-not-active t)
                                (deactivate-region t))
  "Returns a region formed by CURRENT-MARK and CURRENT-POINT, optionally
   signalling an editor error if the current region is not active.  A new
   region is cons'ed on each call.  This optionally deactivates the region."
  (when error-if-not-active (check-region-active buffer))
  (when deactivate-region (deactivate-region buffer))
  (let ((point (buffer-point buffer))
        (mark (current-mark buffer)))
    (if (mark< mark point) (region mark point) (region point mark))))


(defcommand "Activate Region" (p)
  "Make the current region active.  ^G deactivates the region."
  "Make the current region active."
  (declare (ignore p))
  (activate-region (window-buffer (current-window))))


;;; The following are hook functions for keeping things righteous.
;;;

(defun control-g-deactivate-region ()
  (deactivate-region (window-buffer (current-window))))
;;;




;;;; Buffer-Mark primitives and commands.

;;; See Command.Lisp for #'hcmd-make-buffer-hook-fun which makes the
;;; stack for each buffer.

(defun current-mark (buffer)
  "Returns the top of the current buffer's mark stack."
  (ring-ref (variable-value 'buffer-mark-ring :buffer buffer) 0))

(defun buffer-mark (buffer)
  "Returns the top of buffer's mark stack."
  (ring-ref (variable-value 'buffer-mark-ring :buffer buffer) 0))

(defun pop-buffer-mark (buffer)
  "Pops the current buffer's mark stack, returning the mark.  If the stack
   becomes empty, a mark is push on the stack pointing to the buffer's start.
   This always makes the current region not active."
  (let* ((ring (variable-value 'buffer-mark-ring :buffer buffer))
         (mark (ring-pop ring)))
    (deactivate-region buffer)
    (if (zerop (ring-length ring))
        (ring-push (copy-mark
                    (buffer-start-mark buffer) :right-inserting)
                   ring))
    mark))

(defun push-buffer-mark (buffer mark &optional (activate-region nil))
  "Pushes mark into buffer's mark ring, ensuring that the mark is in the right
   buffer and :right-inserting.  Optionally, the current region is made active.
   This never deactivates the current region.  Mark is returned."
  (cond ((eq (line-buffer (mark-line mark)) buffer)
         (setf (mark-kind mark) :right-inserting)
         (ring-push mark (variable-value 'buffer-mark-ring :buffer buffer)))
        (t (error "Mark not in the current buffer.")))
  (when activate-region (activate-region buffer))
  mark)

(defcommand "Set/Pop Mark" (p)
    "Set or Pop the mark ring.
   With no C-U's, pushes point as the mark, activating the current region.
   With one C-U's, pops the mark into point, de-activating the current region.
   With two C-U's, pops the mark and throws it away, de-activating the current
   region."
    "Set or Pop the mark ring."
  (let ((window (current-window)))
    (when window
      (let ((buffer (window-buffer window)))
        (cond ((not p)
               (push-buffer-mark buffer (copy-mark (buffer-point buffer)) t)
               (when (interactive)
                 (message "Mark pushed.")))
              ((= p (value universal-argument-default))
               (pop-and-goto-mark buffer nil))
              ((= p (expt (value universal-argument-default) 2))
               (delete-mark (pop-buffer-mark buffer)))
              (t (editor-error)))))))

(defun pop-and-goto-mark (buffer p)
  (declare (ignore p))
  (bt:with-recursive-lock-held ((buffer-lock buffer))
    (let ((mark (pop-buffer-mark buffer)))
      (move-mark (buffer-point buffer) mark)
      (delete-mark mark))))

(defcommand "Pop and Goto Mark" (p)
  "Pop mark into point, de-activating the current region."
  "Pop mark into point."
  (let ((buffer (window-buffer (current-window))))
    (pop-and-goto-mark buffer p)))

(defcommand "Pop Mark" (p)
  "Pop mark and throw it away, de-activating the current region."
  "Pop mark and throw it away."
  (declare (ignore p))
  (let ((buffer (window-buffer (current-window))))
    (bt:with-recursive-lock-held ((buffer-lock buffer))
      (delete-mark (pop-buffer-mark buffer)))))

(defcommand "Exchange Point and Mark" (p)
  "Swap the positions of the point and the mark."
  "Swap the positions of the point and the mark."
  (declare (ignore p))
  (let ((buffer (window-buffer (current-window))))
    (bt:with-recursive-lock-held ((buffer-lock buffer))
      (let ((point (buffer-point buffer))
            (mark (current-mark buffer)))
        (with-mark ((temp point))
          (move-mark point mark)
          (move-mark mark temp))))))

(defcommand "Mark Whole Buffer"  (p)
  "Set the region around the whole buffer, activating the region.
   Pushes the point on the mark ring first, so two pops get it back.
   With prefix argument, put mark at beginning and point at end."
  "Put point at beginning and part at end of current buffer.
  If P, do it the other way around."
  (let ((buffer (window-buffer (current-window))))
    (bt:with-recursive-lock-held ((buffer-lock buffer))
      (let* ((region (buffer-region buffer))
             (start (region-start region))
             (end (region-end region))
             (point (buffer-point buffer)))
        (push-buffer-mark buffer (copy-mark point))
        (cond (p (push-buffer-mark buffer (copy-mark start) t)
                 (move-mark point end))
              (t (push-buffer-mark buffer (copy-mark end) t)
                 (move-mark point start)))))))








;;;; Commands.

(defcommand "Kill Region" (p)
  "Kill the region, pushing on the kill ring.
   If the region is not active nor the last command a yank, signal an error."
  "Kill the region, pushing on the kill ring."
  (declare (ignore p))
  (with-buffer-globals ((window-buffer (current-window)))
    (.kill-region. (current-region .buffer.)
                   (if (mark< (current-mark .buffer.) .point.)
                       :kill-backward
                       :kill-forward))))

(defcommand "Save Region" (p)
  "Insert the region into the kill ring.
   If the region is not active nor the last command a yank, signal an error."
  "Insert the region into the kill ring."
  (declare (ignore p))
  (let ((buffer (window-buffer (current-window))))
    (with-buffer-globals (buffer)
      (ring-push (hi::.copy-region. (current-region buffer)) *kill-ring*))))

(defmacro .kill-next-word. (&optional p)
  `(let ((num (or ,p 1)))
     (with-mark ((mark .point. :temporary))
       (if (word-offset mark num)
           (if (minusp num)
               (.kill-region. (region mark .point.) :kill-backward)
               (.kill-region. (region .point. mark) :kill-forward))
           (editor-error)))))

(defcommand "Kill Next Word" (p)
  "Kill a word at the point.
  With prefix argument delete that many words.  The text killed is
  appended to the text currently at the top of the kill ring if it was
  next to the text being killed."
    "Kill p words at the point"
  (let ((buffer (window-buffer (current-window))))
    (with-buffer-globals (buffer)
      (.kill-next-word. p))))

(defcommand "Kill Previous Word" (p)
  "Kill a word before the point.
  With prefix argument kill that many words before the point.  The text
  being killed is appended to the text currently at the top of the kill
  ring if it was next to the text being killed."
    "Kill p words before the point"
  (let ((buffer (window-buffer (current-window))))
    (with-buffer-globals (buffer)
      (.kill-next-word. (- (or p 1))))))

(defmacro kill-line-macro (buffer p)
  (let ((p-sym (gensym "KILL-LINE_P"))
        (buffer-sym (gensym "KILL-LINE_BUFFER")))
    `(let* ((,p-sym ,p)
            (,buffer-sym ,buffer)
            (point (buffer-point ,buffer-sym))
            (line (mark-line point)))
       (with-mark ((mark point))
         (cond
           (,p-sym
            (when (and (/= (mark-charpos point) 0) (minusp ,p-sym))
              (incf ,p-sym))
            (unless (line-offset mark ,p-sym 0)
              (with-buffer-globals (,buffer-sym)
                (if (plusp ,p-sym)
                    (.kill-region. (region .point. (buffer-end mark .buffer.)) :kill-forward)
                    (.kill-region. (region (buffer-start mark .buffer.) .point.) :kill-backward)))
              (editor-error))
            (if (plusp ,p-sym)
                (with-buffer-globals (,buffer-sym)
                  (.kill-region. (region .point. mark) :kill-forward)
                  (.kill-region. (region mark .point.) :kill-backward))))
           (t
            (cond ((not (with-buffer-globals (,buffer-sym) (hi::.blank-after-p. mark)))
                   (line-end mark))
                  ((line-next line)
                   (line-start mark (line-next line)))
                  ((not (end-line-p mark))
                   (line-end mark))
                  (t
                   (editor-error)))
            (with-buffer-globals (,buffer-sym)
              (.kill-region. (region .point. mark) :kill-forward))))))))

(defun kill-line (buffer &optional (p 1))
  (with-buffer-globals (buffer)
    (kill-line-macro buffer p)))

(defcommand "Kill Line" (p)
  "Kills the characters to the end of the current line.
  If the line is empty then the line is deleted.  With prefix argument,
  deletes that many lines past the point (or before if the prefix is negative)."
    "Kills p lines after the point."
  (let ((buffer (window-buffer (current-window))))
    (kill-line buffer p)))

(defcommand "Backward Kill Line" (p)
    "Kill from the point to the beginning of the line.
  If at the beginning of the line, kill the newline and any trailing space
  on the previous line.  With prefix argument, call \"Kill Line\" with
  the argument negated."
    "Kills p lines before the point."
  (let ((buffer (window-buffer (current-window))))
    (if p
        (kill-line buffer (- p))
        (with-mark ((m (buffer-point buffer)))
          (cond ((zerop (mark-charpos m))
                 (mark-before m)
                 (unless (reverse-find-attribute m :space #'zerop)
                   (buffer-start m buffer)))
                (t
                 (line-start m)))
          (with-buffer-globals (buffer)
            (.kill-region. (region m .point.) :kill-backward))))))


(defcommand "Delete Blank Lines" (p)
  "On a blank line, deletes all surrounding blank lines, leaving just
  one. On an isolated blank line, deletes that one. On a non-blank line,
  deletes all blank following that one."
  "Kill blank lines around the point"
  (declare (ignore p))
  (let ((point (current-point)))
    (with-mark ((beg-mark point :left-inserting)
                (end-mark point :right-inserting))
      ;; handle case when the current line is blank
      (when (blank-line-p (mark-line point))
        ;; back up to last non-whitespace character
        (reverse-find-attribute beg-mark :whitespace #'zerop)
        (when (previous-character beg-mark)
          ;; that is, we didn't back up to the beginning of the buffer
          (unless (same-line-p beg-mark end-mark)
            (line-offset beg-mark 1 0)))
        ;; if isolated, zap the line else zap the blank ones above
        (cond ((same-line-p beg-mark end-mark)
               (line-offset end-mark 1 0))
              (t
               (line-start end-mark)))
        (delete-region (region beg-mark end-mark)))
      ;; always delete all blank lines after the current line
      (move-mark beg-mark point)
      (when (line-offset beg-mark 1 0)
        (move-mark end-mark beg-mark)
        (find-attribute end-mark :whitespace #'zerop)
        (when (next-character end-mark)
          ;; that is, we didn't go all the way to the end of the buffer
          (line-start end-mark))
        (delete-region (region beg-mark end-mark))))))


(defcommand "Un-Kill" (p)
    "Inserts the top item in the kill-ring at the point.
  The mark is left mark before the insertion and the point after.  With prefix
  argument inserts the prefix'th most recent item."
    "Inserts the item with index p in the kill ring at the point, leaving
  the mark before and the point after."
  (let ((buffer (window-buffer (current-window))))
    (with-buffer-globals (buffer)
      (let ((idx (1- (or p 1))))
        (cond ((> (ring-length *kill-ring*) idx -1)
               (let* ((region (ring-ref *kill-ring* idx))
                      (point (buffer-point buffer))
                      (mark (copy-mark point)))
                 (push-buffer-mark buffer mark)
                 (hi::.insert-region. point region)
                 (make-region-undo :delete "Un-Kill"
                                   (region (copy-mark mark) (copy-mark point))))
               (setf (last-command-type) :unkill))
              (t (editor-error)))))))
;;;
(push :unkill *ephemerally-active-command-types*)

(defcommand "Rotate Kill Ring" (p)
  "Replace un-killed text with previously killed text.
  Kills the current region, rotates the kill ring, and inserts the new top
  item.  With prefix argument rotates the kill ring that many times."
  "This function will not behave in any reasonable fashion when
  called as a lisp function."
  (let ((buffer (window-buffer (current-window))))
    (with-buffer-globals (buffer)
      (let ((point (buffer-point buffer))
            (mark (current-mark buffer)))
        (cond ((or (not (eq (last-command-type) :unkill))
                   (zerop (ring-length *kill-ring*)))
               (editor-error))
              (t (delete-region (region mark point))
                 (rotate-ring *kill-ring* (or p 1))
                 (hi::.insert-region. point (ring-ref *kill-ring* 0))
                 (make-region-undo :delete "Un-Kill"
                                   (region (copy-mark mark) (copy-mark point)))
                 (setf (last-command-type) :unkill)))))))
