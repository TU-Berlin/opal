;; $_Header$
;;; calling the interpreter from within emacs

(provide 'opal-oasys)
(require 'oasys-mode)

;; 0) keymap and menu setup
(defun opal-mode-oasys-keymap ()
  "set the opal-mode oasys keymap"

;  (if opal-running-xemacs 
;      (opal-oasys-menu-xemacs)
;      (opal-oasys-menu-fsfemacs)
;  )
  (define-key opal-mode-map "\C-c\C-a\C-a" 'opal-oasys-call)
  (define-key opal-mode-map "\C-c\C-a\C-c" 'oasys-close)
  (define-key opal-mode-map "\M-e" 'opal-oasys-structure)
  (define-key opal-mode-map "\C-c\C-a\C-s" 'opal-oasys-structure)
  (define-key opal-mode-map "\C-c\C-a\C-v" 'opal-oasys-view)
  (define-key opal-mode-map "\C-c\C-ar"    'opal-oasys-call-raw)
  (define-key opal-mode-map "\C-c\C-ae"    'opal-oasys-eval-raw)
)

(defun opal-oasys-menu-xemacs ()
  "set up opal-mode oasys menu for XEmacs"

  (interactive)
  (setq opal-oasys-menu
	(list "OASYS"
	      ["Eval" opal-oasys-structure  t]
	       ["Close" oasys-close t]
	       "---"
	       ["Interpreter" opal-oasys-call 
		:active t :included (not opal-novice)]
	       ["Raw interpreter" opal-oasys-call-raw 
	       :active t :included (not opal-novice)]
	      ["View interpreter" opal-oasys-view t]
	      )
   )
)

(defun opal-oasys-menu-fsfemacs ()
  "set up opal-mode oasys menu for FSF Emacs"
  
  (interactive)
  (if opal-novice
      () ; see opal-mode-set-menu'opal-mode.el for opal novices
    (define-key opal-mode-map [menu-bar opal oasys]
      (cons "OASYS" (make-sparse-keymap "OASYS")))

    (define-key opal-mode-map [menu-bar opal oasys opal-oasys-view]
      '("View interpreter" . opal-oasys-call-view))
    (define-key opal-mode-map [menu-bar opal oasys opal-oasys-call-raw]
      '("Raw interpreter" . opal-oasys-call-raw))
    (define-key opal-mode-map [menu-bar opal oasys opal-oasys-call]
      '("Interpreter" . opal-oasys-call))
    (define-key opal-mode-map [menu-bar opal oasys t1]
      '("" . nil))
    (define-key opal-mode-map [menu-bar opal oasys opal-oasys-close]
      '("Close" . oasys-close))
    (define-key opal-mode-map [menu-bar opal oasys opal-oasys-call]
      '("Eval" . opal-oasys-structure))
    )
)


;; 1) calling the interpreter

(defun opal-oasys-call ()
  "call the OPAL interpreter in a buffer *oasys* if not already existent"
 
  (interactive)
  (oasys-set-current (current-buffer))
  (oasys)
)

(defun opal-oasys-structure ()
  "call oasys with current structure"
  (interactive)
  
  (delete-other-windows)
  (opal-ask-save-opal-buffers)
  (oasys-set-current (current-buffer))
  (oasys-prepare-current-opal)
;  (display-buffer oasys-check-diagnostics-buffer)
  (if oasys-check-errors
      (progn
	(message "errors checking %s!" 
	       (oasys-unit-of (buffer-file-name oasys-current-opal-buffer)))
;	(display-buffer oasys-buffer-name)
      )
    (if oasys-eval-allowed
	(oasys-eval (oasys-unit-of 
		     (buffer-file-name oasys-current-opal-buffer)))
    )
  )
)
  

(defun opal-oasys-view ()
  "show *oasys* but do not select that buffer"
  (interactive)
  (oasys t)
)

(defun opal-oasys-raw ()
  "call oasys and switch to that buffer, but inhibit initialising"
  (interactive)
  (let ((oasys-initstring ""))
    (oasys)
  )
)

(defun opal-oasys-eval-raw ()
  "just call oasys-eval"
  (interactive)
  (oasys-eval)
)

