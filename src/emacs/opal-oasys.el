;; $_Header$
;;; calling the interpreter from within emacs

(provide 'opal-oasys)
(require 'oasys-mode)

;; 0) keymap and menu setup
(defun opal-mode-oasys-keymap ()
  "set the opal-mode oasys keymap"

  (opal-oasys-menu-xemacs)

  (define-key opal-mode-map "\C-c\C-a\C-e" 'opal-oasys-eval)
  (define-key opal-mode-map "\M-e" 'opal-oasys-eval)
  (define-key opal-mode-map "\M-o" 'opal-oasys-view)
  (define-key opal-mode-map "\C-c\C-a\C-c" 'opal-oasys-check)
  (define-key opal-mode-map "\C-c\C-a\C-v" 'opal-oasys-view)
  (define-key opal-mode-map "\C-c\C-a\C-h" 'opal-oasys-hide)
  (define-key opal-mode-map "\C-c\C-a\C-r" 'opal-oasys-raw)
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
  (oasys-add-path (file-name-directory (buffer-file-name)))
  (oasys-focus (file-name-nondirectory (buffer-file-name)))
  (oasys-eval expr)
)

(defun opal-oasys-check ()
  "set focus to current unit and check"
  (interactive)
  (oasys-add-path (file-name-directory (buffer-file-name)))
  (oasys-focus (file-name-nondirectory (buffer-file-name)))
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
