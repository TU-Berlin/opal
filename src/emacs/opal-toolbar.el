;;; Define a Toolbar for Opal buffers
;;; Copyright 1989 - 1999 by the Opal Group, TU Berlin. All rights reserved 
;;; See OCSHOME/doc/LICENSE or
;;; http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
;;; $Id$


(provide 'opal-toolbar)

(defvar opal-toolbar-position 'left
  "*position in which the Opal toolbar is displayed (one of 'left or 'right)")

;; empty string for normal size
(defvar opal-toolbar-icon-set "half-"
  "*prefix to mark a specific icon set. Note that opal-toolbar-icon-width must be changed accordingly." )

;; 68 for normal size
(defvar opal-toolbar-icon-width 40
  "*width of icons in currently selected icon set.")

(defvar opal-toolbar-unsaved nil
  "t iff some opal buffers have been modified")

(if opal-toolbar-position
    (progn
      
;;; first search for the images
      
      (defvar opal-toolbar-dir nil)
      
      (let ((p load-path))
	(while (and p (not opal-toolbar-dir))
	  (if (file-readable-p (concat (car p) "/opal-toolbar-opal.gif"))
	      (setq opal-toolbar-dir (car p))
	    )
	  (setq p (cdr p))
	  )
	)
      
      (if (not opal-toolbar-dir)
	  (error "could not read images for toolbar")
	
	(defvar opal-toolbar-specifier nil)
	

;;; $Button Definitions$
	(if opal-running-xemacs
        (progn	    
	(defun opal-toolbar-make-file (fn)
	  (concat opal-toolbar-dir "/opal-toolbar-" opal-toolbar-icon-set fn)
	  )

	(defvar opal-toolbar-opal-icon
	  (toolbar-make-button-list (opal-toolbar-make-file "opal.gif")))
	(defvar opal-toolbar-opal-button
	  [opal-toolbar-opal-icon opal-toolbar-opal nil "Opal mode version 4"])
	
	(defvar opal-toolbar-sign-impl-icon 
	  (toolbar-make-button-list (opal-toolbar-make-file "sign.impl.gif")))
	(defvar opal-toolbar-sign-impl-button
	  [opal-toolbar-sign-impl-icon opal-toolbar-sign-impl 
	   (equal major-mode 'opal-mode)
            "switch from SIGNATURE to IMPLEMENTATION or vice versa"])

	(defvar opal-toolbar-save-icon
	  (toolbar-make-button-list (opal-toolbar-make-file "save.gif")))
	(defvar opal-toolbar-save-button
	  [opal-toolbar-save-icon opal-toolbar-save opal-toolbar-unsaved
           "save all changed Opal sources"])

	(defvar opal-toolbar-load-icon
	  (toolbar-make-button-list (opal-toolbar-make-file "load.gif")))
	(defvar opal-toolbar-load-button
	  [opal-toolbar-load-icon opal-toolbar-load t
           "load Opal source"])

	(defvar opal-toolbar-compile-icon
	  (toolbar-make-button-list (opal-toolbar-make-file "compile.gif")))
	(defvar opal-toolbar-compile-button
	  [opal-toolbar-compile-icon opal-compile-call 
           (not (equal major-mode 'oasys-mode)) 
           "compile current Opal project"])

	(defvar opal-toolbar-eval-icon
	  (toolbar-make-button-list (opal-toolbar-make-file "eval.gif")))
	(defvar opal-toolbar-eval-button
	  [opal-toolbar-eval-icon opal-toolbar-eval
	  (or (equal major-mode 'opal-mode) (equal major-mode 'oasys-mode))
	  "evaluate expression in current unit"])

	(defvar opal-toolbar-update-icon
	  (toolbar-make-button-list (opal-toolbar-make-file "upddiag.gif")))
	(defvar opal-toolbar-update-button
	  [opal-toolbar-update-icon opal-diag-update (buffer-file-name)
           "read error messages and other diagnostics"])

	(defvar opal-toolbar-hide-icon
	  (toolbar-make-button-list (opal-toolbar-make-file "hidediag.gif")))
	(defvar opal-toolbar-hide-button
	  [opal-toolbar-hide-icon opal-diag-hide-diag-buffer
	   (and (opal-diag-my-diag-p t) (not (equal major-mode 'oasys-mode)))
           "hide diagnostics"])

	(defvar opal-toolbar-next-diag-icon
	  (toolbar-make-button-list (opal-toolbar-make-file "next.gif")))
	(defvar opal-toolbar-next-diag-button
	  [opal-toolbar-next-diag-icon opal-diag-next-main-error
           (and opal-diag-errors (opal-diag-my-diag-p t))
	   "show next main diagnostic"])

	(defvar opal-toolbar-prev-diag-icon
	  (toolbar-make-button-list (opal-toolbar-make-file "prev.gif")))
	(defvar opal-toolbar-prev-diag-button
	  [opal-toolbar-prev-diag-icon opal-diag-prev-main-error
           (and opal-diag-errors (opal-diag-my-diag-p t))
	   "show previous main diagnostic"]) 

	(defvar opal-toolbar-info-icon
	  (toolbar-make-button-list (opal-toolbar-make-file "info.gif")))
	(defvar opal-toolbar-info-button
	  [opal-toolbar-info-icon opal-diag-toggle-extended-flag
           (and opal-diag-curr-error (opal-diag-my-diag-p t))
           "show or hide additional information on current diagnostic"])

	(defvar opal-toolbar-certify-icon
	  (toolbar-make-button-list (opal-toolbar-make-file "certify.gif")))
	(defvar opal-toolbar-certify-button
	  [opal-toolbar-certify-icon opal-toolbar-certify 
          (equal major-mode 'opal-mode)
          "certify current proposition"])

	(defvar opal-toolbar-check-icon
	  (toolbar-make-button-list (opal-toolbar-make-file "check.gif")))
	(defvar opal-toolbar-check-button
	  [opal-toolbar-check-icon opal-toolbar-check
          (equal major-mode 'opal-mode)
          "check signature of current proposition"])
	

	(defvar opal-toolbar-sequent-q-icon
	  (toolbar-make-button-list (opal-toolbar-make-file "sequent-q.gif")))
	(defvar opal-toolbar-sequent-q-button
	  [opal-toolbar-sequent-q-icon opal-toolbar-proofcheck
          (equal major-mode 'opal-mode)
	  "call proofchecker for current unit"])

	(defvar opal-toolbar-sequent-icon
	  (toolbar-make-button-list (opal-toolbar-make-file "sequent.gif")))
	(defvar opal-toolbar-sequent-button
	  [opal-toolbar-sequent-icon opal-pcheck-trace
          (equal major-mode 'opal-mode)
	  "see trace"])


	(defvar opal-toolbar-novice
	  (list opal-toolbar-opal-button
		opal-toolbar-sign-impl-button
		opal-toolbar-save-button
		opal-toolbar-load-button
		opal-toolbar-update-button
		; opal-toolbar-hide-button
		opal-toolbar-next-diag-button
		opal-toolbar-prev-diag-button
		opal-toolbar-info-button
		)
	  "toolbar for Opal novices")
	
	(defvar opal-toolbar-standard
	  (list opal-toolbar-opal-button
		opal-toolbar-sign-impl-button
		opal-toolbar-save-button
		opal-toolbar-load-button
		opal-toolbar-compile-button
;		opal-toolbar-eval-button
		opal-toolbar-update-button
		; opal-toolbar-hide-button
		opal-toolbar-next-diag-button
		opal-toolbar-prev-diag-button
		opal-toolbar-info-button
		)	
	  "standard Opal toolbar"
	  )
	
	(defvar opal-toolbar-pcheck
	  (list opal-toolbar-opal-button
		opal-toolbar-sign-impl-button
		opal-toolbar-save-button
		opal-toolbar-load-button
		opal-toolbar-compile-button
;		opal-toolbar-eval-button
		opal-toolbar-update-button
		; opal-toolbar-hide-button
		opal-toolbar-next-diag-button
		opal-toolbar-prev-diag-button
		opal-toolbar-info-button
		nil
		opal-toolbar-certify-button
		opal-toolbar-check-button
		opal-toolbar-sequent-q-button
		opal-toolbar-sequent-button
		)
	  )			       
	  ); progn 
	  ); running-xemacs
;;; $Installing the Toolbar$
	(defun opal-toolbar-install ()
	  "install Opal toolbar in current frame"
	  
	  (interactive)
	  
	  (let ((width opal-toolbar-icon-width)
		(frame (selected-frame))
		(buffer (current-buffer))
		(tag-set '(win))
		(toolbar (cond (opal-novice opal-toolbar-novice)
			       (opal-pchecker opal-toolbar-pcheck)
			       (t opal-toolbar-standard)))
		)
	    (cond ((equal opal-toolbar-position 'left)
		   (set-specifier left-toolbar toolbar buffer)
		   (set-specifier left-toolbar-width width frame tag-set)
		   (set-specifier left-toolbar-visible-p t frame tag-set))
		  ((equal opal-toolbar-position 'right)
		   (set-specifier right-toolbar toolbar buffer)
		   (set-specifier right-toolbar-width width frame tag-set)
		   (set-specifier right-toolbar-visible-p t frame tag-set))
		  )
	    )
	  )

;;; $Button Functions$
	(defun opal-toolbar-opal ()
	  "print message in echo area"
	  
	  (interactive)
	  (message "OCS system 2.4a")
	  )
	
	(defun opal-toolbar-sign-impl ()
	  "switch from SIGNATURE to IMPLEMENTATION and vice versa"
	  (interactive)
	  
	  (cond ((opal-in-sign) (opal-switch-to-impl))
		((opal-in-impl) (opal-switch-to-sign))
		(t (message "No Opal source file"))
		)
	  )

	(defun opal-toolbar-save ()
	  (interactive)
	  (opal-ask-save-opal-buffers)
	  (setq opal-toolbar-unsaved nil)
	  (opal-toolbar-install)
	  )

	(defun opal-toolbar-load ()
	  "popup menu with known Opal files"

	  (interactive)
	  (popup-menu (opal-switch-opal-files-menu nil))
	  )

	(defun opal-toolbar-eval ()
	  "evaluate expression"
	  
	  (interactive)
	  (cond ((equal major-mode 'opal-mode)
		 (call-interactively 'opal-oasys-eval))
		((equal major-mode 'oasys-mode)
		 (call-interactively 'oasys-eval))
		)
	  )

	(defun opal-toolbar-proofcheck ()
	  "pcheck current menu"

	  (interactive)
	  (oasys-add-path (file-name-directory (buffer-file-name)))
	  (oasys-focus (file-name-nondirectory (buffer-file-name)))
	  (oasys-cmd "pc")
	  )
	
	(defun opal-toolbar-certify ()
	  "certify current proposition"
	  (interactive)
	  (opal-certify-sign-proof)
	  )

	(defun opal-toolbar-check ()
	  "check signature of current proof"
	  (interactive)
	  (opal-certify-check-proof)
	  )


	;; end of proper functions

	) ; not opal-toolbar-dir
      )
  ; else toolbar-position
  (defun opal-toolbar-install ()
    "dummy, does nothing"
    (interactive)
    )
  ) ; opal-toolbar-position

(if (not opal-running-xemacs)
    (progn
	; FSF Emacs
	(defun opal-toolbar-ersatz-menu ()
	  "ersatz menu for toolbar in XEmacs"
	  (interactive)
	  (setq opal-ersatz-keymap (make-sparse-keymap "Opal Mode"))
	  (define-key opal-ersatz-keymap [opal-ersatz-info]
	    '("toggle extended help" . opal-diag-toggle-extended-flag))
	  (define-key opal-ersatz-keymap [opal-ersatz-prev-diag]
	    '("previous error" . opal-diag-prev-main-error))
	  (define-key opal-ersatz-keymap [opal-ersatz-next-diag]
	    '("next error" . opal-diag-next-main-error))
	  (define-key opal-ersatz-keymap [opal-ersatz-update]
	    '("update diagnostics" . opal-diag-update))
	  (define-key opal-ersatz-keymap [opal-ersatz-save]
	    '("save changed Opal sources" . opal-toolbar-save))
	  (fset 'opal-toolbar-ersatz-menu opal-ersatz-keymap)
	  )

	(defun opal-ersatz-enabled-p () (interactive)
	  (and opal-diag-curr-error (opal-diag-my-diag-p t)))

	(defun opal-toolbar-install ()
           "install toolbar ersatz - click mouse1 on toolbar"
	   (interactive)
;	   (opal-toolbar-ersatz-menu)
;	   (define-key opal-mode-map [mode-line mouse-1]
;	     'opal-toolbar-ersatz-menu)
	   )
	(put 'opal-diag-toggle-extended-flag 'menu-enable '(opal-ersatz-enabled-p))
	(put 'opal-diag-prev-main-error 'menu-enable '(opal-ersatz-enabled-p))
	(put 'opal-diag-next-main-error 'menu-enable '(opal-ersatz-enabled-p))

)); not running-xemacs

(defun opal-toolbar-save-necessary ()
  " return t, iff some Opal buffers contain unsaved changes"
  (interactive)

  (let ((bl (buffer-list))
	(unsaved nil)
	(old opal-toolbar-unsaved)
	b blv
	)
    (while (and bl (not unsaved))
      (setq b (car bl))
      (setq bl (cdr bl))
      (setq blv  (buffer-local-variables b))
      (setq unsaved (and
		     (or
		      (equal (assoc 'major-mode blv) 
			     '(major-mode . opal-mode))
		      (equal (assoc 'major-mode blv) 
			     '(major-mode . opal-defs-mode))
		      )
		     (buffer-modified-p b)
		     (buffer-file-name b)
		     )
	    )
      )
    (setq opal-toolbar-unsaved unsaved)
    (if (or (and (not old) unsaved) (and old (not unsaved)))
	(progn 
;	  (popup-dialog-box (list "Some Opal files changed status" ["ok" 'b t]))
	  (opal-toolbar-install)
	  )
      )
    )
  )
	    
(defun opal-toolbar-mark-change ()
  "set unsaved variable to t and make save button pop up"
  
  (let ((blv (buffer-local-variables (current-buffer))))
    (if (or
	 (equal (assoc 'major-mode blv) '(major-mode . opal-mode))
	 (equal (assoc 'major-mode blv) '(major-mode . opal-defs-mode))
	 )
	(progn
	  (setq opal-toolbar-unsaved t)
	  (opal-toolbar-install)
	  )
      )
    )
  )