; --- opaldefs-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (add-hook 'find-file-not-found-hooks 'opal-defs-init-sysdefs)
(require 'opal-switch)
(defvar opal-defs-mode-map nil)   ; Create a mode-specific keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opal-defs-mode ()
   "major mode for editing ProjectDefs and SysDefs files"
   (interactive)

   (kill-all-local-variables)
   (setq mode-name "OpalDefs")
   (setq major-mode 'opal-defs-mode)
   (setq opal-defs-mode-map (make-sparse-keymap))
   (use-local-map opal-defs-mode-map)         ; This provides the local keymap
   (put 'opal-defs-mode 'font-lock-defaults 
       '(opal-defs-font-lock-keywords nil nil nil 'beginning-of-buffer))
   (if opal-running-xemacs
       ; XEmacs-related
       (progn
	 (set-buffer-menubar (copy-sequence current-menubar))
	 (add-submenu nil
		      (opal-defs-mode-menu-xemacs t)
		      )
	 (define-key opal-defs-mode-map [(alt button1)] 
	    'opal-defs-mode-menu-xemacs)
	 (define-key opal-defs-mode-map [(button3)] 
	    'opal-defs-mode-menu-xemacs)
	 )
     ; FSF Emacs related
     )
   (run-hooks 'opal-defs-mode-hook)
)

(defun opal-defs-mode-menu-xemacs (&optional nopopup)
  
  (interactive)
  (if nopopup
      (list "OpalDefs" :filter 'opal-defs-menu-filter )
    (popup-menu (opal-defs-mode-menu-xemacs t))
    )
)

(defun opal-defs-menu-filter (items)
  "add buffer menus to menu items"
  (nconc 
   (list (concat "Current: "(buffer-file-name)))
;   items
    (if (string-match "ProjectDefs" (buffer-name))
	(list ["set OCSPROJECT to this file" 'opal-defs-make-current t])
      nil
      )
   (list (opal-switch-opaldefs-buffers-menu))
   (list (opal-switch-opal-buffers-menu))
   ;; Submenu mit Opal-Dateien in akt. Verzeichnis
   (list (list "Load Opal file" :filter 'opal-switch-opal-files-menu))
   (list 
    ["Save all Opal files" opal-ask-save-opal-buffers t]
    )
  )
)

(defvar opal-defs-font-lock-keywords

  (list
   '("#.*$" (0 'font-lock-comment-face t t))
   '("\\([A-Za-z0-9_.]+\\)[ \t]*.?=" (1 'font-lock-function-name-face nil t))
   '("$([A-Z]+)" (0 'font-lock-keyword-face nil t))
   '("\\\\$" (0 'font-lock-keyword-face nil t))
   )
)




(defun opal-defs-init-sysdefs ()
  "initialize SysDefs file"

  (cond ((string-match "SysDefs" (buffer-name))
         (let ((dd default-directory))
         (set-variable 'default-directory (concat opal-path "/lib/om/tmpls/"))
         (call-interactively 'opal-defs-insert-sysdefs)
         (set-variable 'default-directory dd)
         )
        )
  )
)

(defun opal-defs-insert-sysdefs (file)
  "ask for Sysdefs template file to be inserted"
  (interactive  "fTemplate to insert:\n")
  (insert-file-contents file)
)

(defun opal-defs-make-current ()
  "make current buffer ProjectDefs buffer"
  (interactive)
  (setq opal-compile-projectdefsfile (buffer-file-name))
)

(provide 'opal-defs-mode)
