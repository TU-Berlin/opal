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

(require 'syntax nil t)			; Emacs 21 add-on

(defgroup opal-indentation nil
  "Opal indentation."
  :group 'opal
  :prefix "opal-indentation-")

(defcustom opal-indentation-layout-offset 2
  "Extra indentation to add before expressions in a opal layout list."
  :type 'integer
  :group 'opal-indentation)

(defcustom opal-indentation-starter-offset 1
  "Extra indentation after an opening keyword (e.g. let)."
  :type 'integer
  :group 'opal-indentation)

(defcustom opal-indentation-left-offset 2
  "Extra indentation after an indentation to the left (e.g. after do)."
  :type 'integer
  :group 'opal-indentation)

(defcustom  opal-indentation-ifte-offset 2
  "Extra indentation after the keywords `IF' `THEN' or `ELSE'."
  :type 'integer
  :group 'opal-indentation)

(defun opal-indent-if ()
  (forward-line -1)
  (beginning-of-line)
  (if (looking-at "^[ \t]*IF")
      ()
    ()
)
)

(defun opal-current-column ()
  "Compute current column according to haskell syntax rules,
  correctly ignoring composition."
  (save-excursion
    (let ((start (point))
          (cc 0))
      (beginning-of-line)
      (while (< (point) start)
        (if (= (char-after) ?\t)
            (setq cc (* 8 (+ 1 (/ cc 8))))
          (incf cc))
        (forward-char))
      cc)))

(defun opal-indent-start ()
  (or (bobp) (looking-at "^[ \t]*\\(IMPLEMENTATION\\|SIGNATURE\\|DEF\\|FUN\\|DATA\\|SORT\\|TYPE\\|IMPORT\\)")))

(defun opal-indent-line ()
  "Indent current line as Opal code"
  (interactive)
  (beginning-of-line)
  (let (cur-line line-number-at-pos)
  (if (opal-indent-start) ; Check for block start
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*IF") ; Check for IF
	  ()
	()
      )))))

(defun turn-on-opal-indentation ()
  (set (make-local-variable 'indent-line-function) 'opal-indent-line)
)

(provide 'opal-indentation)
;;; opal-indentation.el ends here
