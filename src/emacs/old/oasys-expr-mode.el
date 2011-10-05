;; Small mode for oasys expression buffer

(provide 'oasys-expr-mode)

(defvar oasys-expr-buffer "*oasys-expr*" 
  "Buffer used for temporary storing of input expression")

(defvar oasys-expr-mode-map nil "map for oasys expr mode")

(defun oasys-expr-mode-map ()
  "define keymap for oasys expr mode"
  (setq oasys-expr-mode-map (make-sparse-keymap))
  (define-key oasys-expr-mode-map "\M-n" 'opal-diag-next-main-error)
  (define-key oasys-expr-mode-map "\M-p" 'opal-diag-prev-main-error)
  (define-key oasys-expr-mode-map "\C-cc" 'oasys-expr-eval)
)

(defun oasys-expr-mode ()
  "major mode for oasys output buffer"

  (interactive)
  (setq major-mode 'oasys-expr-mode)
  (setq mode-name "oasys expression (v2)")
  (oasys-expr-mode-map)
  (use-local-map oasys-expr-mode-map)
  (oasys-mode-menu)
  (opal-toolbar-install)
)

(defun oasys-expr-eval ()
  "(re)evaluate epxression in oasys-expr-buffer"

  (interactive)
  (oasys-eval (buffer-substring nil nil (get-buffer oasys-expr-buffer)))
)