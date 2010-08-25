;; $_Header$
;;; calling the interpreter from within emacs

(provide 'opal-oasys)
(require 'oasys-mode)

;; 0) keymap and menu setup

(defun opal-mode-oasys-keymap ()
  "set the opal-mode oasys keymap"
  (define-key opal-mode-map "\C-c\C-a\C-e" 'opal-oasys-eval)
  (define-key opal-mode-map "\C-c\C-a\C-f" 'opal-oasys-focus)
  (define-key opal-mode-map "\M-e" 'opal-oasys-eval)
  (define-key opal-mode-map "\C-c\C-a\C-c" 'opal-oasys-check)
  (define-key opal-mode-map "\C-c\C-a\C-v" 'opal-oasys-view)
  (define-key opal-mode-map "\C-c\C-a\C-h" 'opal-oasys-hide)
  (define-key opal-mode-map "\C-c\C-a\C-r" 'opal-oasys-raw)

  (if opal-running-xemacs
      (opal-oasys-menu-xemacs)
      (opal-oasys-menu-fsfemacs)
      )

)

(defun opal-oasys-menu-fsfemacs ()
  "Set opal-mode oasys menu for FSF Emacs"
  (interactive)

  (if (not opal-novice)
      (progn
	(define-key opal-mode-map [menu-bar opal oasys]
	  (cons "Oasys" (make-sparse-keymap "Oasys")))
	
	(define-key opal-mode-map [menu-bar opal oasys opal-oasys-eval]
	  '("Eval ..." . opal-oasys-eval))
	(define-key opal-mode-map [menu-bar opal oasys opal-oasys-check]
	  '("Check" . opal-oasys-check))
	(define-key opal-mode-map [menu-bar opal oasys opal-oasys-focus]
	  '("Focus" . opal-oasys-focus))
	(define-key opal-mode-map [menu-bar opal oasys opal-oasys-view]
	  '("View" . opal-oasys-view))
	(define-key opal-mode-map [menu-bar opal oasys opal-oasys-hide]
	  '("Hide" . opal-oasys-hide))
      )
  )
)


(defun opal-oasys-menu-xemacs ()
  "set up opal-mode oasys menu for XEmacs"

  (interactive)
  (setq opal-oasys-menu
	(list "Oasys"
	      ["Eval ..." opal-oasys-eval t]
	      ["Check" opal-oasys-check t]
	      "---"
	      ["View" opal-oasys-view t]
	      ["Hide" opal-oasys-hide t]
	      ))
  )


(defun opal-oasys-eval (expr)
  "set focus to current unit and ask for expression to be evaluated"
  (interactive "sExpression:")
  (let (b)
    (setq b (buffer-file-name))
    (oasys-cmd (concat "a " (file-name-nondirectory b)))
    (oasys-focus (file-name-nondirectory b))
    (oasys-eval expr)
    )
  )

(defun opal-oasys-focus ()
  "set focus to current unit and ask for expression to be evaluated"
  (interactive)
  (let (b)
    (setq b (buffer-file-name))
     (oasys-cmd (concat "a " (file-name-nondirectory b)))
     (oasys-focus (file-name-nondirectory b))
     )
  
  )

(defun opal-oasys-check ()
  "set focus to current unit and check"
  (interactive)
   (let (b)
    (setq b (buffer-file-name))
    (oasys-cmd (concat "a " (file-name-nondirectory b)))
    (oasys-focus (file-name-nondirectory b))
    )
  (oasys-check)
)

(defun opal-oasys-view ()
  "show oasys window"
  (interactive)
  (oasys-start)
  (pop-to-buffer oasys-output)
)

(defun opal-oasys-raw ()
  "show oasys window"
  (interactive)
  (oasys-start)
  (pop-to-buffer oasys-raw-output)
)

(defun opal-oasys-hide ()
  "hide oasys (and other) windows"
  (interactive)
  (delete-other-windows)
)
