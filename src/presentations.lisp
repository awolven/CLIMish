(in-package :climish)

(defparameter *quotation-character* #\")

(defun invoke-with-input-context (frame type override body-continuation context-continuation)
  (declare (dynamic-extent body-continuation context-continuation)
	   (special *input-wait-handler*
		    *input-wait-test*
		    *pointer-button-press-handler*))
  ;; At one time, this used to canonicalize the presentation type by
  ;; calling WITH-PRESENTATION-TYPE-DECODED and then consing just the
  ;; type name and parameters.  That turns out not to be necessary any
  ;; more since everyplace else in CLIM is careful to decode the type.
  ;; Furthermore, it is necessary to include the type options in the
  ;; input context in case the options are needed to correctly present
  ;; an object using the context type as the presentation type.
  (with-stack-list (this-tag '#:context-tag type)        ;guaranteed to be unique
    (with-stack-list (this-context type this-tag)
      (let (#+NOTYET(pwindow nil)
            #+NOTYET(px 0)
            #+NOTYET(py 0)
            #+NOTYET(old-state 0))
        (flet ((pointer-motion-pending (stream)
		 (declare (ignorable stream))
                 (let ((moved-p nil))
		   #+NOTYET
                   (multiple-value-setq (moved-p pwindow px py)
                     (pointer-state-changed (stream-primary-pointer stream)
                                            pwindow px py))
		   #+NIL
                   (or 
                     (and moved-p
                          pwindow
                          (window-cohorts-p stream pwindow))
                     ;; This is more like the pointer moving than anything else.
                     (/= old-state
                         (setf old-state (window-modifier-state stream)))))))
          #-allegro (declare (dynamic-extent #'pointer-motion-pending))
          (let (#+NOTYET(*input-wait-handler* #'highlight-presentation-of-context-type)
                #+NOTYET(*input-wait-test* #'pointer-motion-pending)
                #+NOTYET(*pointer-button-press-handler* #'input-context-button-press-handler))
            ;; Push the new context.  The most specific context is at the
            ;; beginning of *INPUT-CONTEXT, the least specific is at the end.
	    (let ((saved (input-context frame)))
	      (if override
		  (setf (input-context frame) (list this-context))
		  (setf (input-context frame) (list* this-context (input-context frame))))
	      (unwind-protect
		   (block with-input-context
                     (multiple-value-bind (object ptype event options)
			 (catch this-tag
			   (return-from invoke-with-input-context
			     (funcall body-continuation)))
                       (funcall context-continuation object ptype event options)))
		(if override
		    (setf (input-context frame) saved)
		    (pop (input-context frame)))))))))))

(defun write-token (token stream &key acceptably)
  (cond ((and acceptably (some #'delimiter-gesture-p token))
         (write-char *quotation-character* stream)
         (write-string token stream)
         (write-char *quotation-character* stream))
        (t
         (write-string token stream))))
