;; include the following lines into your .emacs file
;; (where you may need to change /usr/ocs to point to
;; the actual installation place)

(setq opal-path "/usr/ocs")

(setq load-path (cons (concat opal-path "/lib/emacs") load-path))

(defun my-opal-hook()
  ;; (setq opal-alist-file nil) ; uncomment to not save import maps
  ;; (setq opal-diag-extended-flag nil) ; uncomment to not show extended help
  ;; (setq opal-use-frames nil) ; uncomment to inhibit usage of frames
  ;; (setq indent-tabs-mode nil) ; uncomment to not expand TABs 
  ;; (setq auto-fill-mode nil) ; uncomment to not autobreak lines
  ;; (opal-misc-indent-on) ; uncomment to switch on opal-indentation

  (opal-font-lock opal-font-lock-keywords-simple) ; choose one of these
  ;; (opal-font-lock opal-font-lock-keywords-extended) 
					; NOTE that the extended keywords may
					; be rather slow

)

(add-hook 'opal-mode-hook 'my-opal-hook)

(require 'opal-mode)
