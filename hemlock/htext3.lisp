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
;;; Written by Skef Wholey.
;;;
;;; The code in this file implements the insert functions in the
;;; "Doing Stuff and Going Places" chapter of the Hemlock Design document.
;;;

(in-package :hemlock-internals)


(defmacro .insert-character. (mark character)
  "Inserts the Character at the specified Mark."
  (let ((mark-sym (gensym "INSERT-CHARACTER_MARK"))
        (character-sym (gensym "INSERT-CHARACTER_CHARACTER")))
    `(let ((,mark-sym ,mark)
           (,character-sym ,character))
       (declare (type character ,character-sym))
       (let* ((line (mark-line ,mark-sym))
              (buffer (line-%buffer line)))
         (modifying-buffer buffer
           (.modifying-line. line ,mark-sym)
           (cond ((char= ,character-sym #\newline)
                  (let* ((next (line-next line))
                         (new-chars (subseq (the simple-string open-chars)
                                            0 left-open-pos))
                         (new-line (make-line :%buffer buffer
                                              :chars (decf *cache-modification-tick*)
                                              :previous line
                                              :next next)))
                    (maybe-move-some-marks (charpos line new-line) left-open-pos
                      (- charpos left-open-pos))
                    (setf (line-chars line) new-chars)
                    (setf (line-next line) new-line)
                    (if next (setf (line-previous next) new-line))
                    (number-line new-line)
                    (setq open-line new-line  left-open-pos 0)))
                 (t
                  (if (= right-open-pos left-open-pos)
                      (.grow-open-chars.))

                  (maybe-move-some-marks (charpos line) left-open-pos
                    (1+ charpos))

                  (cond
                    ((eq (mark-%kind ,mark-sym) :right-inserting)
                     (decf right-open-pos)
                     (setf (char (the simple-string open-chars) right-open-pos)
                           ,character-sym))
                    (t
                     (setf (char (the simple-string open-chars) left-open-pos)
                           ,character-sym)
                     (incf left-open-pos))))))))))

(defun insert-character-1 (buffer mark character)
  (with-buffer-globals (buffer)
    (.insert-character. mark character)))

(defmethod insert-character (mark character)
  (let ((mark-line-buffer (line-%buffer (mark-line mark))))
    (insert-character-1 mark-line-buffer mark character)))


(defmacro .insert-string. (mark string &optional (start 0) (end nil))
  "Inserts the String at the Mark.  Do not use Start and End unless you
  know what you're doing!"
  (let ((mark-sym (gensym "INSERT-STRING_MARK"))
        (string-sym (gensym "INSERT-STRING_STRING"))
        (start-sym (gensym "INSERT-STRING-START"))
        (end-sym (gensym "INSERT-STRING_END")))
    `(let* ((,mark-sym ,mark)
            (,string-sym (coerce ,string 'simple-string))
            (,start-sym ,start)
            (,end-sym (or ,end (length ,string-sym)))
            (line (mark-line ,mark-sym)))
       (declare (simple-string ,string-sym))
       (unless (zerop (- ,end-sym ,start-sym))
         (modifying-buffer .buffer.
           (.modifying-line. line ,mark-sym)
           (if (position #\newline ,string-sym :start ,start-sym :end ,end-sym)
               (with-mark ((mark ,mark-sym :left-inserting))
                 (do ((left-index ,start-sym (1+ right-index))
                      (right-index
                       (position #\newline ,string-sym :start ,start-sym :end ,end-sym)
                       (position #\newline ,string-sym :start (1+ right-index) :end ,end-sym)))
                     ((null right-index)
                      (if (/= left-index ,end-sym)
                          ;; can't have recursive macros
                          (insert-string-1 .buffer. mark ,string-sym left-index ,end-sym)))
                   ;; i wonder how new-undo is supposed to work
                   ;; does it only record top-level inserts and such or recursive ones too
                   ;; this may need to be insert-string-1 an internal function or something
                   ;; so it doesn't trigger the around method
                   (insert-string-1 .buffer. mark ,string-sym left-index right-index)
                   (insert-character-1 .buffer. mark #\newline)))
               (let ((length (- ,end-sym ,start-sym)))
                 (if (<= right-open-pos (+ left-open-pos ,end-sym))
                     (.grow-open-chars. (* (+ line-cache-length ,end-sym) 2)))

                 (maybe-move-some-marks (charpos line) left-open-pos
                   (+ charpos length))
                 (cond
                   ((eq (mark-%kind ,mark-sym) :right-inserting)
                    (let ((new (- right-open-pos length)))
                      (%sp-byte-blt ,string-sym ,start-sym open-chars new right-open-pos)
                      (setq right-open-pos new)))
                   (t
                    (let ((new (+ left-open-pos length)))
                      (%sp-byte-blt ,string-sym ,start-sym open-chars left-open-pos new)
                      (setq left-open-pos new)))))))))))

(defun insert-string-1 (buffer mark string &optional (start 0) (end (length string)))
  (with-buffer-globals (buffer)
    (.insert-string. mark string start end)))

(defmethod insert-string (mark string &optional (start 0) (end (length string)))
  ;; sanity checks
  (let ((mark-line-buffer (line-%buffer (mark-line mark))))
    (insert-string-1 mark-line-buffer mark string start end)))

(defconstant line-number-interval-guess 8
  "Our first guess at how we should number an inserted region's lines.")

(defmacro .insert-region. (mark region)
  "Inserts the given Region at the Mark."
  (let ((mark-sym (gensym "MARK"))
        (region-sym (gensym "REGION")))
    `(let* ((,region-sym ,region)
            (,mark-sym ,mark)
            (start (region-start ,region-sym))
            (end (region-end ,region-sym))
            (first-line (mark-line start))
            (last-line (mark-line end))
            (first-charpos (mark-charpos start))
            (last-charpos (mark-charpos end)))
       (cond
         ((eq first-line last-line)
          ;; simple case -- just BLT the characters in with insert-string
          (if (eq first-line open-line) (.close-line.))
          (insert-string-1 .buffer. ,mark-sym (line-chars first-line) first-charpos last-charpos))
         (t
          (.close-line.)
          (let* ((line (mark-line ,mark-sym))
                 (next (line-next line))
                 (charpos (mark-charpos ,mark-sym))
                 (buffer (line-%buffer line))
                 (old-chars (line-chars line)))
            (declare (simple-string old-chars))
            (modifying-buffer buffer
              ;; we're "modifying buffer" but buffer globals get changed in .buffer. not buffer
              ;;hack marked line's chars
              (let* ((first-chars (line-chars first-line))
                     (first-length (length first-chars))
                     (new-length (+ charpos (- first-length first-charpos)))
                     (new-chars (make-string new-length)))
                (declare (simple-string first-chars new-chars))
                (%sp-byte-blt old-chars 0 new-chars 0 charpos)
                (%sp-byte-blt first-chars first-charpos new-chars charpos new-length)
                (setf (line-chars line) new-chars))

              ;; Copy intervening lines.  We don't link the lines in until we are
              ;; done in case the mark is within the region we are inserting.
              (do* ((this-line (line-next first-line) (line-next this-line))
                    (number (+ (line-number line) line-number-interval-guess)
                            (+ number line-number-interval-guess))
                    (first (%copy-line this-line :previous line
                                                 :%buffer buffer ;; buffer not .buffer.
                                                 :number number))
                    (previous first)
                    (new-line first (%copy-line this-line :previous previous
                                                          :%buffer buffer
                                                          :number number)))
                   ((eq this-line last-line)
                    ;;make last line
                    (let* ((last-chars (line-chars new-line))
                           (old-length (length old-chars))
                           (new-length (+ last-charpos (- old-length charpos)))
                           (new-chars (make-string new-length)))
                      (%sp-byte-blt last-chars 0 new-chars 0 last-charpos)
                      (%sp-byte-blt old-chars charpos new-chars last-charpos
                                    new-length)
                      (setf (line-next line) first)
                      (setf (line-chars new-line) new-chars)
                      (setf (line-next previous) new-line)
                      (setf (line-next new-line) next)
                      (when next
                        (setf (line-previous next) new-line)
                        (if (<= (line-number next) number)
                            (renumber-region-containing new-line)))
                      ;;fix up the marks
                      (maybe-move-some-marks (this-charpos line new-line) charpos
                        (+ last-charpos (- this-charpos charpos)))))
                (setf (line-next previous) new-line  previous new-line)))))))))

(defun insert-region-1 (buffer mark region)
  (with-buffer-globals (buffer)
    (.insert-region. mark region)))

(defmethod insert-region (mark region)
  (let ((mark-line-buffer (line-%buffer (mark-line mark)))
        (region-start-mark-line-buffer (line-%buffer (region-start region)))
        (region-end-mark-line-buffer (line-%buffer (region-end region))))
    (if (and (eq mark-line-buffer region-start-mark-line-buffer)
             (eq mark-line-buffer region-end-mark-line-buffer))
        (insert-region-1 mark-line-buffer mark region)
        (progn
          (warn "buffers do not match.")
          (insert-region-1 mark-line-buffer mark region)))))

(defmacro .ninsert-region. (mark region)
  "Inserts the given Region at the Mark, possibly destroying the Region.
  Region may not be a part of any buffer's region."
  (let ((mark-sym (gensym "NINSERT-REGION_MARK"))
        (region-sym (gensym "NINSERT-REGION_REGION")))
    `(let* ((,mark-sym ,mark)
            (,region-sym ,region)
            (start (region-start ,region-sym))
            (end (region-end ,region-sym))
            (first-line (mark-line start))
            (last-line (mark-line end))
            (first-charpos (mark-charpos start))
            (last-charpos (mark-charpos end)))
       (cond
         ((eq first-line last-line)
          ;; Simple case -- just BLT the characters in with insert-string.
          (if (eq first-line open-line) (.close-line.))
          (insert-string-1 .buffer. ,mark-sym (line-chars first-line) first-charpos last-charpos))
         (t
          (when (bufferp (line-%buffer first-line))
            (error "Region is linked into Buffer ~S." (line-%buffer first-line)))
          (.close-line.)
          (let* ((line (mark-line ,mark-sym))
                 (second-line (line-next first-line))
                 (next (line-next line))
                 (charpos (mark-charpos ,mark-sym))
                 (buffer (line-%buffer line))
                 (old-chars (line-chars line)))
            (declare (simple-string old-chars))
            (modifying-buffer buffer ;; now we're modifying a potentially different buffer
              ;; also we could be locking two different buffers at once
              ;; Make new chars for first and last lines.
              (let* ((first-chars (line-chars first-line))
                     (first-length (length first-chars))
                     (new-length (+ charpos (- first-length first-charpos)))
                     (new-chars (make-string new-length)))
                (declare (simple-string first-chars new-chars))
                (%sp-byte-blt old-chars 0 new-chars 0 charpos)
                (%sp-byte-blt first-chars first-charpos new-chars charpos
                              new-length)
                (setf (line-chars line) new-chars))
              (let* ((last-chars (line-chars last-line))
                     (old-length (length old-chars))
                     (new-length (+ last-charpos (- old-length charpos)))
                     (new-chars (make-string new-length)))
                (%sp-byte-blt last-chars 0 new-chars 0 last-charpos)
                (%sp-byte-blt old-chars charpos new-chars last-charpos new-length)
                (setf (line-chars last-line) new-chars))

              ;; Link stuff together.
              (setf (line-next last-line) next)
              (setf (line-next line) second-line)
              (setf (line-previous second-line) line)

              ;;Number the inserted stuff and mash any marks.
              (do ((line second-line (line-next line))
                   (number (+ (line-number line) line-number-interval-guess)
                           (+ number line-number-interval-guess)))
                  ((eq line next)
                   (when next
                     (setf (line-previous next) last-line)
                     (if (<= (line-number next) number)
                         ;; this renumber stuff could be what is slowing down scrolling
                         (renumber-region-containing last-line))))
                (when (line-marks line)
                  (dolist (m (line-marks line))
                    (setf (mark-line m) nil))
                  (setf (line-marks line) nil))

                ;; when we setf line-%buffer do the buffer-globals
                ;; from the old line-%buffer need to be migrated to the new buffer?
                (setf (line-number line) number  (line-%buffer line) buffer)))

            ;; Fix up the marks in the line inserted into.
            (maybe-move-some-marks (this-charpos line last-line) charpos
              (+ last-charpos (- this-charpos charpos)))))))))

(defun ninsert-region-1 (buffer mark region)
  (with-buffer-globals (buffer)
    (.ninsert-region. mark region)))

(defmethod ninsert-region (mark region)
  (let ((mark-line-buffer (line-%buffer (mark-line mark)))
        (region-start-mark-line-buffer (line-%buffer (region-start region)))
        (region-end-mark-line-buffer (line-%buffer (region-end region))))
    (if (and (eq mark-line-buffer region-start-mark-line-buffer)
             (eq mark-line-buffer region-end-mark-line-buffer))
        (ninsert-region-1 mark-line-buffer mark region)
        (progn
          (warn "buffers do not match.")
          (ninsert-region-1 mark-line-buffer mark region)))))
