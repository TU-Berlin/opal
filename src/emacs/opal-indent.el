;;; opal-indent.el --- Indentation module for Opal Mode

;; Installation:
;; 
;; To bind TAB to the indentation command for all Opal buffers, add
;; this to .emacs:
;;
;;    (add-hook 'opal-mode-hook 'turn-on-opal-indent)
;;
;; Otherwise, call `turn-on-opal-indent'.
;;
;;
;; Customisation:
;;
;; None supported.


(defun opal-indent ()
  ""
  (interactive)
  ())


(defvar opal-indent-old)


;; The main functions.
(defun turn-on-opal-indent ()
  "Set `indent-line-function' to the Opal indentation function.
TAB will now move the cursor to the next indent point in the previous
nonblank line.  An indent point is a non-whitespace character following
whitespace.

Runs `opal-indent-hook'."
  (set (make-local-variable 'opal-indent-old) indent-line-function)
  (set (make-local-variable 'indent-line-function) 'opal-indent)
  (run-hooks 'opal-indent-hook))

(defun turn-off-opal-indent ()
  "Return `indent-line-function' to original value.
I.e. the value before `turn-on-opal-indent' was called."
  (when (local-variable-p 'opal-indent-old)
    (setq indent-line-function opal-indent-old)
    (kill-local-variable 'opal-indent-old)))


(provide 'haskell-simple-indent)
