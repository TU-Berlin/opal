;;; Define a Toolbar for Opal buffers
;;; Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
;;; See OCSHOME/etc/LICENSE or 
;;; http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
;;; $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/emacs/opal-toolbar.el,v 1.1 1998-09-23 12:14:35 kd Exp $

;;; This file is written for XEmacs and may not work with other Emacsen.

(provide 'opal-toolbar)

(defvar opal-toolbar-position 'left
  "*position in which the Opal toolbar is displayed (one of 'left or 'right)")

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
	(defun opal-toolbar-make-file (fn)
	  (concat opal-toolbar-dir "/opal-toolbar-" fn)
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
	  [opal-toolbar-save-icon opal-toolbar-save t
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
		opal-toolbar-eval-button
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
		opal-toolbar-eval-button
		opal-toolbar-update-button
		; opal-toolbar-hide-button
		opal-toolbar-next-diag-button
		opal-toolbar-prev-diag-button
		opal-toolbar-info-button
		nil
		opal-toolbar-sequent-q-button
		opal-toolbar-sequent-button
		)
	  )			       
	
;;; $Installing the Toolbar$
	(defun opal-toolbar-install ()
	  "install Opal toolbar in current frame"
	  
	  (interactive)
	  
	  (let ((width 68)
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
	  (message "OCS system 2.3b.beta")
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


	;; end of proper functions

	) ; not opal-toolbar-dir
      )
  ; else toolbar-position
  (defun opal-toolbar-install ()
    "dummy, does nothing"
    (interactive)
    )
  ) ; opal-toolbar-position

