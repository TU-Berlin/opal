;;; emacs mode for processing Opal diagnostics

;;; Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
;;; See OCSHOME/etc/LICENSE or 
;;; http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
;;; $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/emacs/opal-diag-mode.el,v 1.7 1998-11-19 10:45:46 kd Exp $


(provide 'opal-diag-mode)

;;; $User Variables$

(defvar opal-diag-extended-flag t  "*if t then show extended help")

;;; $Error List$
(defvar opal-diag-errors nil
  "Vector of diags (type src-overlay diag-overlay).")

(defun opal-diag-make-diag (type src err) (list type src err))
(defun opal-diag-type-of (diag)  (car diag))
(defun opal-diag-src-of  (diag)  (cadr diag))
(defun opal-diag-err-of  (diag)  (car (cddr diag)))

(defvar opal-diag-curr-error nil
  "Current position in opal-diag-errors.")

(defun opal-diag-get-current-diag ()
  "return current diagnostic or nil, if none is current"
  (if opal-diag-curr-error
      (aref opal-diag-errors opal-diag-curr-error)
    nil
    )
  )

(defun opal-diag-next-diag (&optional suberror)
  "increment pointer and get next diagnostic or nil, honors show-what"

  (let ((error-number (length opal-diag-errors)))
    (if opal-diag-curr-error
	(setq opal-diag-curr-error (+ opal-diag-curr-error 1))
      (setq opal-diag-curr-error 0)
      )
    (while (and (< opal-diag-curr-error error-number)
		(not (opal-diag-err-enabled-p
		      (opal-diag-type-of (opal-diag-get-current-diag))
		      suberror)))
      (setq opal-diag-curr-error (+ opal-diag-curr-error 1))
      )
    (if (>= opal-diag-curr-error error-number)
	(setq opal-diag-curr-error nil)
      )
    )
  (opal-diag-get-current-diag)
  )

(defun opal-diag-prev-diag (&optional suberror)
  "decrement pointer and get previous diagnostic or nil, honors show-what"

  (if opal-diag-curr-error
      (setq opal-diag-curr-error (- opal-diag-curr-error 1))
    (setq opal-diag-curr-error (- (length opal-diag-errors) 1))
    )
  (while (and (<= 0 opal-diag-curr-error) 
	      (not (opal-diag-err-enabled-p
		    (opal-diag-type-of (opal-diag-get-current-diag))
		    suberror)))
    (setq opal-diag-curr-error (- opal-diag-curr-error 1))
    )
  (if (< opal-diag-curr-error 0)
      (setq opal-diag-curr-error nil)
    )
  (opal-diag-get-current-diag)
  )

(defun opal-diag-clear-errors ()
  "clear all information in current error list"

  (if (> (length opal-diag-errors) 0)
      (progn 
	(setq opal-diag-curr-error (- (length opal-diag-errors) 1))
	(while (>= opal-diag-curr-error 0)
	  (delete-overlay (opal-diag-src-of (opal-diag-get-current-diag)))
	  (delete-overlay (opal-diag-err-of (opal-diag-get-current-diag)))
	  (setq opal-diag-curr-error (- opal-diag-curr-error 1))
	  )
	)
    )
  (setq opal-diag-errors nil)
  (setq opal-diag-curr-error nil)
)

;;; $Selective Display of Errors$
(defvar opal-diag-show-what 'all "what errors to show (error, warning, all)")

(defun opal-diag-err-enabled-p (type &optional suberror)
  "t, if type is to be shown according to show-what; if suberror is t, show these"
  (or (and suberror (equal 'suberror type))
      (and (not (equal 'suberror type))
	   (or (equal 'all opal-diag-show-what)
	       (and (equal 'warning opal-diag-show-what)
		    (or (equal 'warning type)
			(equal 'error type)
			)
		    )
	       (and (equal 'error opal-diag-show-what)
		    (equal 'error type)
		    )
	       )
	   )
      )
  )

(defun opal-diag-show-all ()
  "Show all diagnostics."
  (interactive)
;  (let ((x (opal-diag-find-diag t)))
  (setq opal-diag-show-what 'all)
;  (setq opal-diag-number-errors nil)
;  (if x (opal-diag-show-error))
;)
)

(defun opal-diag-show-errors-and-warns ()
  "Show only errors and warnings."
  (interactive)
;  (let ((x (opal-diag-find-diag t)))
  (setq opal-diag-show-what 'warning)
;  (setq opal-diag-number-errors nil)
;  (if x (opal-diag-show-error))
;)
)

(defun opal-diag-show-errors ()
  "Show only errors."
  (interactive)
;  (let ((x (opal-diag-find-diag t)))
  (setq opal-diag-show-what 'error)
;  (setq opal-diag-number-errors nil)
;  (if x (opal-diag-show-error))
;)
)

;;; $Menus and Keybindings$
;; global key-bindings
(defun opal-diag-fsfemacs-menu ()
  "Set the opal-diag-mode menu for FSF Emacs"

  (interactive)

  (define-key opal-mode-map [menu-bar diag]
    (cons "Diag" (make-sparse-keymap "Diag")))
    
  (define-key opal-mode-map [menu-bar diag opal-diag-next-main-error]
    '("Show next main error" . opal-diag-next-main-error))
  (define-key opal-mode-map [menu-bar diag opal-diag-prev-main-error]
    '("Show previous main error" . opal-diag-prev-main-error))
  (define-key opal-mode-map [menu-bar diag t4]
    '("" . nil))
  (define-key opal-mode-map [menu-bar diag opal-diag-clear-diags]
    '("Clear diagnostics" . opal-diag-clear-diags))
  (define-key opal-mode-map [menu-bar diag opal-diag-update-silent]
    '("Silently update diagnostics buffer" . opal-diag-update-silent))
  (define-key opal-mode-map [menu-bar diag opal-diag-update]
    '("Update diagnostics buffer" . opal-diag-update))
  (define-key opal-mode-map [menu-bar diag opal-diag-toggle-extended-flag]
    '("Toggle extended help" . opal-diag-toggle-extended-flag))  
  (define-key opal-mode-map [menu-bar diag t3]
    '("" . nil))
  (if (not opal-novice)
      (progn
	(define-key opal-mode-map 
	  [menu-bar diag opal-diag-insert-missing-item]
	  '("Import item from diagnostic" . opal-diag-insert-missing-item))
	(define-key opal-mode-map [menu-bar diag t2]
	  '(""  . nil))
      )
  )
  (define-key opal-mode-map [menu-bar diag opal-diag-show]
    '("Show current error" . opal-diag-show))
  (if (not opal-novice)
      (progn
	(define-key opal-mode-map [menu-bar diag opal-diag-next-error]
	  '("Show next (sub)error" . opal-diag-next-error))
	(define-key opal-mode-map [menu-bar diag opal-diag-prev-error]
	  '("Show previous (sub)error" . opal-diag-prev-error))
      )
  )
  (define-key opal-mode-map [menu-bar diag t1]
    '("" . nil))
  (define-key opal-mode-map [menu-bar diag opal-diag-show-errors]
    '("Show only errors" . opal-diag-show-errors))
  (define-key opal-mode-map [menu-bar diag opal-diag-show-errors-and-warns]
    '("Show only errors and warnings" . opal-diag-show-errors-and-warns))
  (define-key opal-mode-map [menu-bar diag opal-diag-show-all]
    '("Show all diagnostics" . opal-diag-show-all)) 
   
  (put 'opal-diag-show-errors 'menu-enable
       '(not (equal 'error opal-diag-show-what)))
  (put 'opal-diag-show-errors-and-warns 'menu-enable 
       '(not (equal 'warning opal-diag-show-what)))
  (put 'opal-diag-show-all 'menu-enable
       '(not (equal 'all opal-diag-show-what)))
  )



(defun opal-mode-diag-keymap ()
  "Set the opal-mode diag keymap."
;; changed by ralfi
;; shortkeys
  (define-key opal-mode-map "\M-n" 'opal-diag-next-main-error)
  (define-key opal-mode-map "\M-p" 'opal-diag-prev-main-error)
  (define-key opal-mode-map "\M-u" 'opal-diag-update)
  (define-key opal-mode-map "\M-v" 'opal-diag-update-silent)
  (define-key opal-mode-map "\M-h" 'opal-diag-toggle-extended-flag)
  (define-key opal-mode-map "\M-m" 'opal-diag-insert-missing-item)
  (define-key opal-mode-map "\M-0" 'opal-diag-clear-diags)
;;longkeys
  (define-key opal-mode-map "\C-c\C-d\C-n" 'opal-diag-next-main-error)
  (define-key opal-mode-map "\C-c\C-d\C-p" 'opal-diag-prev-main-error)
  (define-key opal-mode-map "\C-c\C-d\C-u" 'opal-diag-update)
  (define-key opal-mode-map "\C-c\C-d\C-v" 'opal-diag-update-silent)
  (define-key opal-mode-map "\C-c\C-d\C-c" 'opal-diag-show-current)
  (define-key opal-mode-map "\C-c\C-d\C-f" 'opal-diag-next-error)
  (define-key opal-mode-map "\C-c\C-d\C-b" 'opal-diag-prev-error)
  (define-key opal-mode-map "\C-c\C-d\C-e" 'opal-diag-show-errors)
  (define-key opal-mode-map "\C-c\C-d\C-w" 'opal-diag-show-errors-and-warns)
  (define-key opal-mode-map "\C-c\C-d\C-a" 'opal-diag-show-all)
  (define-key opal-mode-map "\C-c\C-d\C-h" 'opal-toggle-extended-flag)
  (define-key opal-mode-map "\C-c\C-d\C-m" 'opal-diag-insert-missing-item)
;  (define-key opal-mode-map "\C-c\C-d\C-0" 'opal-diag-clear-diags)
  (define-key opal-mode-map [mouse-1] 'opal-diag-mouse-select-error)
  (define-key opal-mode-map [mouse-2] 'opal-diag-mouse-hide-diags)
  (define-key opal-mode-map [mouse-3] 'opal-diag-mouse-help)

  (opal-diag-fsfemacs-menu)
  )

(defun opal-diag-mouse-select-error (event)
  "set point to position where event occurred. if event occurred 
over an overlay which has err-no set, select that error"
  (interactive "e")
  (let (ovl this errno)
    (mouse-set-point event)
    (setq ovl (overlays-at (point)))
    (while ovl
      (setq this (car ovl))
      (setq ovl (cdr ovl))
      (setq errno (overlay-get this 'err-no))
      (if errno
	  (progn
	    (if (>= errno 0)
		(opal-diag-show-this-error errno)
	      (opal-diag-not-found)
	      )
	    (setq ovl nil)
	    )))))

(defun opal-diag-mouse-hide-diags (event)
  "set point to position where event occurred. if event occurred 
over an overlay which has err-no set, hide-diagnostics"
  (interactive "e")
  (let (ovl this errno)
    (mouse-set-point event)
    (setq ovl (overlays-at (point)))
    (while ovl
      (setq this (car ovl))
      (setq ovl (cdr ovl))
      (setq errno (overlay-get this 'err-no))
      (if errno
	  (progn
	    (if (>= errno 0)
		(opal-diag-hide-diag-buffer)
	      (opal-diag-not-found)
	      )
	    (setq ovl nil)
	    )))))

(defun opal-diag-mouse-help (event)
  "set point to position where event occurred. if event occurred 
over an overlay which has err-no set, toggle extended help"
  (interactive "e")
  (let (ovl this errno)
    (mouse-set-point event)
    (setq ovl (overlays-at (point)))
    (while ovl
      (setq this (car ovl))
      (setq ovl (cdr ovl))
      (setq errno (overlay-get this 'err-no))
      (if errno
	  (progn
	    (if (>= errno 0)
		(progn
		  (opal-diag-show-this-error errno)
		  (opal-diag-toggle-extended-flag)
		  )
	      (opal-diag-not-found)
	      )
	    (setq ovl nil)
	    )))))

;;; $Diag Mode$
;; colors for diag-mode

(defun opal-diag-hilit19 ()
  "setup regexps for opal-diag-mode for hilit19"
(hilit-set-mode-patterns
    '(opal-diag-mode)
    '(
      ("^.*HINT:" nil define)
      ("^.*WARNING:" nil warning)
      ("^.*ERROR:" nil error)
     )
   )
 )

(defconst opal-diag-font-lock-keywords
  (list
   '("\\(ERROR\\|WARNING\\|HINT\\)" (0 'font-lock-function-name-face t t))
   )
)

(put 'opal-diag-mode 'font-lock-defaults 
       '(opal-diag-font-lock-keywords nil nil nil 'beginning-of-buffer)
  )
;; opal-diag-mode entry

(defun opal-diag-mode () 

"Major mode for visiting OC diagnostic files together with the
corresponding OPAL sources.  Several functions are bounded GLOBALLY to
keys the first time this mode is activated (see below). Each of this
functions trys to examine the pair of diag and source file they could
be targeted to.  This try succeeds if either the current buffer holds
a .diag file or if the current buffer holds a .sign or .impl file and
a corresponding .diag file is loaded in some other buffer. In either
case, both buffers are made visible (more or less as you expect), and
the source buffer becomes selected.

\\[opal-diag-show] = 'opal-diag-show
          shows the current error (necessary after loading a diag file)
\\[opal-diag-show-all] = 'opal-diag-show-all
          prepares to visit all diagnostics
\\[opal-diag-show-errors-and-warns] = 'opal-diag-show-errors-and-warns
          prepares to visit only errors and warnings
\\[opal-diag-show-errors] = 'opal-diag-show-errors
          prepares to visit only errors
\\[opal-diag-next-main-error] = 'opal-diag-next-main-error
          steps to the next main error 
\\[opal-diag-prev-main-error] = 'opal-diag-prev-main-error
          steps to the previous main error
\\[opal-diag-next-error] = 'opal-diag-next-error
          steps to the next error
\\[opal-diag-prev-error] = 'opal-diag-prev-error
          steps to the previous error
\\[opal-diag-update] = 'opal-diag-update
          updates the diag buffer (useful after recompiling the 
          source visited)

"

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'opal-diag-mode)
  (setq mode-name "Opal diagnostics")
  (use-local-map opal-mode-map)         ; This provides the local keymap

  (setq buffer-read-only t)
;  (opal-toolbar-install)  ; no toolbar for FSF Emacs :-(

  (run-hooks 'opal-diag-mode-hook))

(defvar opal-diag-buffer nil "buffer in which current diagnostics are found")
(defvar opal-diag-source nil "buffer in which source is located by default")
(defvar opal-diag-hide nil "buffer which is shown when hiding - if nil, opal-diag-source is used")
(defvar opal-diag-buffer-may-kill t "may kill diag buffer")

;;; $Displaying Errors$

(defun opal-diag-my-diag-p (&optional noerror)
  "check whether current error list might concern this buffer.
Signal error, if not. Don't signal error if noerror is t"
  (interactive)
  (let (ok)
    (setq ok (or (equal major-mode 'opal-diag-mode)
		 (not opal-diag-source)
		 (and (equal major-mode 'opal-mode)
		      (equal opal-diag-source (current-buffer)))))
    (if (and (not ok) (not noerror))
	(error "current error list belongs to buffer %s"
	       (buffer-name opal-diag-source)))
    ok
)
)

(defun opal-diag-hide-diag-buffer ()
  "hide diag buffer, but do not clear the error list"
  (interactive)
  (if opal-diag-buffer
      (progn
	(set-buffer opal-diag-buffer)
	(delete-other-windows)
	(cond (opal-diag-hide (switch-to-buffer opal-diag-hide))
	      (opal-diag-source (switch-to-buffer opal-diag-source))
	      (opal-diag-errors (switch-to-buffer
				 (overlay-buffer (opal-diag-src-of
						  (aref opal-diag-errors 0)))))
	      (t nil)
	      )
	)
    )
  )

(defun opal-diag-clear-diags ()
  (interactive)
  (opal-diag-hide-diag-buffer)
  (opal-diag-clear-errors)
  (if (and opal-diag-buffer opal-diag-buffer-may-kill)
      (kill-buffer opal-diag-buffer)
    )
  (setq opal-diag-buffer nil)
)

(defun opal-diag-next-main-error ()
  "Visit next compilation error and corresponding source code, skipping
sub errors."
  (interactive)
  (opal-diag-my-diag-p)
  (opal-diag-next-diag)
  (if opal-diag-curr-error
      (opal-diag-show-error)
    (opal-diag-hide-diag-buffer)
    (message "No more diagnostics")
    )
  )

(defun opal-diag-prev-main-error ()
  "Visit previous compilation error and corresponding source code,
skipping sub errors."
  (interactive)
  (opal-diag-my-diag-p)
  (opal-diag-prev-diag)
  (if opal-diag-curr-error
      (opal-diag-show-error)
    (opal-diag-hide-diag-buffer)
    (message "No more diagnostics")
    )
  )

(defun opal-diag-next-error ()
  "Visit next compilation error and corresponding source code."
  (interactive)
  (opal-diag-my-diag-p)
  (opal-diag-next-diag t)
  (if opal-diag-curr-error
      (opal-diag-show-error)
    (opal-diag-hide-diag-buffer)
    (message "No more diagnostics")
    )
  )

(defun opal-diag-prev-error ()
  "Visit previous compilation error and corresponding source code."
  (interactive)
  (opal-diag-my-diag-p)
  (opal-diag-prev-diag t)
  (if opal-diag-curr-error
      (opal-diag-show-error)
    (opal-diag-hide-diag-buffer)
    (message "No more diagnostics")
    )
  )

(defun opal-diag-show-current ()
  "show current diagnostic"
  (interactive)
  (opal-diag-my-diag-p)
  (if opal-diag-curr-error
      (opal-diag-show-error)
    (error "No current diagnostic")
    )
  )

(defun opal-diag-show-this-error (n)
  "show error with specified number"

  (setq opal-diag-curr-error n)
  (opal-diag-show-error)
)

(defun opal-diag-show-error ()
  "Make current error and corresponding source visible."

  (if (not opal-diag-curr-error)
      (if (= (length opal-diag-errors) 0)
	  (error "No diagnostics")
	(error "No current diagnostic")
	)
    (let (src-ext err-ext pop-up-windows err-window)
      (setq pop-up-windows nil)
      (setq src-ext (opal-diag-src-of (opal-diag-get-current-diag)))
      (setq err-ext (opal-diag-err-of (opal-diag-get-current-diag)))
      (opal-diag-set-windows (overlay-buffer src-ext) (overlay-buffer err-ext) t)
      ;; show extended help, if flag set
      (if opal-diag-extended-flag (opal-diag-extended-show err-ext))
      ;; show error in source code
      (switch-to-buffer (overlay-buffer src-ext))
      (if (overlay-start src-ext)
	  (goto-char (overlay-start src-ext))
	(message "this diagnostic is no longer present in source")
	)
      ;; show error message
      (setq err-window (display-buffer (overlay-buffer err-ext)))
      (set-window-point err-window (overlay-start err-ext))
      (set-window-start err-window (overlay-start err-ext))
      ;; print message for certain error
      (if (buffer-file-name opal-diag-source)
	  (opal-diag-show-std-info err-ext)
	)
      (switch-to-buffer (overlay-buffer src-ext))
      )
    )
  )

(defun opal-diag-set-windows (src-buf err-buf &optional new)
  "create windows / buffers as necessary for displaying this error"
  (if (and (or (not opal-diag-extended-flag)
	       (and (get-buffer opal-diag-info-buffer)
		    (get-buffer-window opal-diag-info-buffer)))
	   (get-buffer-window err-buf)
	   (get-buffer-window src-buf)
	   (not new))
      () ; do nothing if diag-info-buffer exists and all buffers are displayed
    (let (dw nw)
      (switch-to-buffer src-buf)
      (delete-other-windows)
      (setq dw (split-window))
      (other-window 1)
      (set-window-buffer dw err-buf)
      (if opal-diag-extended-flag
	  (progn
	    (select-window dw)
	    (setq nw (split-window))
	    (set-window-buffer nw (get-buffer-create opal-diag-info-buffer))
	    (other-window 2)
	    )
	(other-window 1)
	)
      )
    )
)

;;; $File Handling$

(defun opal-diag-update ()
  "Update diagnostics buffer."
  (interactive)
  (opal-diag-find-diag)
  (opal-diag-set-windows opal-diag-source opal-diag-buffer t)
  (setq opal-diag-hide nil)
  (setq opal-diag-buffer-may-kill t)
  (opal-diag-parse)
  (if opal-diag-errors
      (progn 
	(setq opal-diag-curr-error nil)
	(opal-diag-next-error)
	(opal-diag-show-error)
	)
    (setq opal-diag-curr-error nil)
    (switch-to-buffer opal-diag-source)
    (error "No diagnostics")
    )
  )

(defun opal-diag-update-silent ()
  "Update diagnostics buffer without immediately showing diagnostics buffer."
  (interactive)
  (setq opal-diag-hide nil)
  (opal-diag-find-diag)
  (setq opal-diag-buffer-may-kill t)
  (opal-diag-parse)
  (switch-to-buffer opal-diag-source)
  (delete-other-windows)
  (goto-char (point-min))
  )

(defun opal-diag-from-current-buffer ()
  "parse current buffer as diagnostics buffer"

  (interactive)
  (setq opal-diag-buffer (current-buffer))
  (setq opal-diag-buffer-may-kill t)
  (setq opal-diag-source nil)
  (opal-diag-parse)
  (setq opal-diag-curr-error 0)
  (opal-diag-show-error)
  )

      
(defun opal-diag-find-diag (&optional noerror)
  "From information about the current buffer, find a corresponding
diag buffer and select it, make it opal-diag-buffer, and update opal-diag-source "
  ; called from either opal-diag-mode or opal-mode
  (cond ((eq major-mode 'opal-diag-mode)
	 (setq opal-diag-buffer (current-buffer))
	 (let (sn fn)
	   (setq sn (buffer-file-name))
	   (string-match "\\(.*\\)/OCS\\(.*\\)\\.diag" sn)
	   (setq fn (concat (substring sn (match-beginning 1) (match-end 1))
			    (substring sn (match-beginning 2) (match-end 2))))
	   (setq opal-diag-source (find-file-noselect fn))
	   ))
	((eq major-mode 'opal-mode)
	 (setq opal-diag-source (current-buffer))
	 (let (sn fn buf revert-without-query)
	   (setq revert-without-query (list ".*"))
	   (setq sn (buffer-file-name))
	   (setq fn (concat (file-name-directory sn) "OCS/"
			    (file-name-nondirectory sn) ".diag"))
	   (if opal-diag-buffer
	       (if (not (string= fn (buffer-file-name
				     (get-buffer opal-diag-buffer))))
		   (opal-diag-clear-diags)
		 ))
	   (setq buf (find-file-other-window fn))
	   (setq opal-diag-buffer buf)
	   ))
	)
  )

(defun opal-diag-find-source (unit)
  "return buffer which contains this unit"
  (let (fn)
    (setq fn (opal-find-structure unit))
    (if fn
	(find-file-noselect fn)
      nil ; (error "cannot find Opal unit %s" unit)
      )
    )
  )
	  


(defun opal-diag-select-source (&optional diag)
  "Find and select source corresponding to diag buffer."
  (if opal-diag-source 
      (set-buffer opal-diag-source)
    (error "No source for diag buffer set")
    )
  )

;;; $Parsing$

(defconst opal-diag-parse-ocs-regexp 
  "<\\([0-9]+\\),\\([0-9]+\\)>\\(ERROR\\|WARNING\\|HINT\\)"
  "regexp to match ocs warnings")

(defconst opal-diag-parse-oasys-regexp
  "\\(ERROR\\|WARNING\\|HINT\\) \\[\\(.*\\) at \\([0-9]+\\)\\.\\([0-9]+\\)"
  "regexp to match oasys warnings")

(defconst opal-diag-parse-oasys-unknown-regexp
  "\\(ERROR\\|WARNING\\|HINT\\) \\[\\(.*\\) at unknown location"
  "regexp to match oasys diagnostics at unknown locations")

(defconst opal-diag-parse-oasys-eval-regexp
  "\\(ERROR\\|WARNING\\|HINT\\) \\[at \\([0-9]+\\)\\.\\([0-9]+\\)"
  "regexp to match oasys diagnostics for evaluated expressions")

(defconst opal-diag-parse-suberror-regexp
  " *[0-9]+\\. *<\\([0-9]+\\),\\([0-9]+\\)>\\( \\)"
  "regexp to match suberrors")

(defconst opal-diag-error-regexp 
  "\\([0-9]+\\(\\.\\|,\\)[0-9]+[]>]\\)\\|unknown location"
  "lowest common denominator to match errors or suberrors")
  

(defun opal-diag-match (no)
  "return no-th match in buffer"
    (buffer-substring (match-beginning no) (match-end no))
)

(defun opal-diag-parse (&optional silent eval-buf)
  "Parse current diagnostic buffer und setup diagnostic variables. "
  (if (not silent) (message "Parsing diagnostics ..."))
  (opal-diag-clear-errors)
  (let (end-reached err-start err-end line col type curr-src-buf src 
		    true-error src-start src-end new-src-ext new-err-ext
		    signal-error-on-buffer-boundary error-list
		    num-hint num-warn num-error i ext-keymap unknown-src)
    (setq signal-error-on-buffer-boundary nil)
    (save-excursion
      (set-buffer opal-diag-buffer)
      (goto-char (point-min))
      (setq end-reached nil)
      (setq curr-src-buf opal-diag-source)
      (setq num-hint 0) (setq num-warn 0) (setq num-error 0)
      (setq i 0) (setq unknown-src nil)
      (while (not end-reached)
	(beginning-of-line)
	(setq err-start (point))
	(save-excursion (forward-line) (setq err-end (point)))
	(cond
	 ((looking-at opal-diag-parse-ocs-regexp) (opal-diag-handle-ocs))
	 ((looking-at opal-diag-parse-oasys-regexp) (opal-diag-handle-oasys))
	 ((looking-at opal-diag-parse-oasys-unknown-regexp)
	  (opal-diag-handle-oasys-unknown))
	 ((looking-at opal-diag-parse-oasys-eval-regexp)
	  (opal-diag-handle-oasys-eval))
	 ((looking-at opal-diag-parse-suberror-regexp) 
	  (opal-diag-handle-suberror))
	 ((looking-at "Checking Signature of \\(.*\\) \\.\\.\\.")
	  (opal-diag-handle-checking))
	 ((looking-at "\\(Compiling\\|Checking\\) Implementation of \\(.*\\) \\.\\.\\.")
	  (opal-diag-handle-compiling))
	 (t (setq true-error nil))
	 )
	(if true-error (opal-diag-parse-error-found) ) 
	(setq end-reached (> (forward-line) 0))
	); while
      ); save-excursion
    (if error-list
	(setq opal-diag-errors (vconcat (reverse error-list)))
      (setq opal-diag-errors nil)
      )
    (setq opal-diag-curr-error nil)
    (if (not silent)
	(message "%3d error(s), %3d warning(s), %3d hint(s) found"
		 num-error num-warn num-hint)
      )
; no popup-dialog-box in FSF-Emacs :-(
;    (if unknown-src
;	(popup-dialog-box (list (concat "* WARNING *\nThe following source files could not be found:\n" unknown-src)
;			      [ "OK" '(lambda () (interactive)) t]))
;      )
  ;; return: car is t, iff ERRORS are found, 
  ;;         cdr is t, iff user wants to see errors
    (cons (> num-error 0) 
	  (or (and (equal opal-diag-show-what 'all)
		   (> (+ num-error num-warn num-hint) 0))
	      (and (equal opal-diag-show-what 'warning)
		   (> (+ num-error num-warn) 0))
	      (and (equal opal-diag-show-what 'error)
		   (> num-error 0))))
    )
  )


(defun opal-diag-parse-type (string)
  "parse error type and return symbol"
  (cond ((string= "ERROR" string) 'error)
	((string= "WARNING" string) 'warning)
	((string= "HINT" string) 'hint)
	((string= "suberror" string) 'suberror)
	(t 'unknown)
	)
  )

(defun opal-diag-handle-ocs ()

  (setq src curr-src-buf)
  (setq line (string-to-int (opal-diag-match 1)))
  (setq col (string-to-int (opal-diag-match 2)))
  (setq type (opal-diag-match 3))
  (setq true-error t)
)

(defun opal-diag-handle-oasys ()
  (setq src (opal-diag-find-source (opal-diag-match 2)))
  (if (not src) 
      (opal-diag-handle-oasys-unknown-src-list (opal-diag-match 2))
    (setq curr-src-buf src)
    )
  (setq line (string-to-int (opal-diag-match 3)))
  (setq col (string-to-int (opal-diag-match 4)))
  (setq type (opal-diag-match 1))
  (setq true-error t)
)

(defun opal-diag-handle-oasys-unknown ()
  (setq src (opal-diag-find-source (opal-diag-match 2)))
  (if (not src) 
      (opal-diag-handle-oasys-unknown-src-list (opal-diag-match 2))
    (setq curr-src-buf src)
    )
  (setq line 0)
  (setq col 1)
  (setq type (opal-diag-match 1))
  (setq is-suberror nil)
  (setq true-error t)
)

(defun opal-diag-handle-oasys-unknown-src-list (unit)
  (if (string-match (concat "- " unit) (concat unknown-src " "))
      ()
    (setq unknown-src (concat unknown-src "\n- " unit))
    )
  )

(defun opal-diag-handle-oasys-eval ()
  (setq src eval-buf)
  (setq line (+ 1 (string-to-int (opal-diag-match 2))))
  (setq col (string-to-int (opal-diag-match 3)))
  (setq type (opal-diag-match 1))
  (setq is-suberror nil)
  (setq true-error t)
)

(defun opal-diag-handle-suberror ()
  (setq src curr-src-buf)
  (setq line (string-to-int (opal-diag-match 1)))
  (setq col (string-to-int (opal-diag-match 2)))
  (setq type "suberror")
  (setq true-error t)
)

(defun opal-diag-handle-checking ()
  (setq curr-src-buf (opal-diag-find-source
		      (concat (opal-diag-match 1) ".sign")))
  (if (not curr-src-buf) (setq unknown-src
			       (concat unknown-src "\n"
				       (concat (opal-diag-match 1) ".sign"))))
  (overlay-put (make-overlay err-start err-end (get-buffer opal-diag-buffer))
	       'face 'opal-diag-source-error-face)
  (setq true-error nil)
)

(defun opal-diag-handle-compiling ()
  (setq curr-src-buf (opal-diag-find-source
		      (concat (opal-diag-match 2) ".impl")))
  (if (not curr-src-buf) (setq unknown-src
			       (concat unknown-src "\'opal-diag-source-error-face)n"
				       (concat (opal-diag-match 1) ".impl"))))
  (overlay-put (make-overlay err-start err-end (get-buffer opal-diag-buffer))
	       'face 'opal-diag-source-error-face)
  (setq true-error nil)
  )

(defun opal-diag-parse-counter (silent)
  (cond ((equal 'hint (opal-diag-parse-type type))
	 (setq num-hint (+ num-hint 1)))
	((equal 'warning (opal-diag-parse-type type))
	 (setq num-warn (+ num-warn 1)))
	((equal 'error (opal-diag-parse-type type))
	 (setq num-error (+ num-error 1))))
  (if (not silent)
      (message "%3d error(s), %3d warning(s), %3d hint(s)"
	       num-error num-warn num-hint)
    )
  )

(defun opal-diag-parse-set-start-position ()
  (if src
      (save-excursion
	(set-buffer src)
	(goto-line line)
	(move-to-column col)
	(backward-char)
	(setq src-start (point))
	(forward-char 1)
	(setq src-end (point))
	(if (equal 'suberror (opal-diag-parse-type type))
	    (setq src-end (+ src-end 1)))
	)
    )
  )

(defun opal-diag-parse-ext-for-unknown ()
  (define-key ext-keymap [(button1)] 'opal-diag-not-found)
  (define-key ext-keymap [(button2)] 'opal-diag-not-found)
  (define-key ext-keymap [(button3)] 'opal-diag-not-found)
  (setq new-err-ext
	(make-overlay err-start err-end
		     (get-buffer opal-diag-buffer)))
;  (set-extent-keymap new-err-ext ext-keymap) 
  (overlay-put new-err-ext 'face 'opal-diag-source-error-face)
  (overlay-put new-err-ext 'mouse-face 'default)
)

(defun opal-diag-parse-ext-keymap ()
  (define-key ext-keymap [(button1)]
    `(lambda () (interactive) (opal-diag-show-this-error ,i)))
  (define-key ext-keymap [(button2)]
    'opal-diag-hide-diag-buffer)
  (define-key ext-keymap [(button3)]
    `(lambda () (interactive) (opal-diag-show-this-error ,i)
       (opal-diag-toggle-extended-flag)))
  )

(defun opal-diag-parse-src-ext ()
  (setq new-src-ext
	(make-overlay src-start src-end (get-buffer src)))
  (overlay-put new-src-ext 'mouse-face 'opal-diag-source-error-face)
;  (set-extent-keymap new-src-ext ext-keymap)
  (cond ((equal 'hint (opal-diag-parse-type type))
	 (overlay-put new-src-ext 'face 'opal-diag-mouse-hint-face))
	((equal 'warning (opal-diag-parse-type type))
	 (overlay-put new-src-ext 'face 'opal-diag-mouse-warning-face))
	(t
	 (overlay-put new-src-ext 'face 'opal-diag-mouse-error-face))
	)
  )

(defun opal-diag-parse-err-ext ()
  (setq new-err-ext (make-overlay err-start err-end
				  (get-buffer opal-diag-buffer)))
  (cond ((equal 'hint (opal-diag-parse-type type))
	 (overlay-put new-err-ext 'face 'opal-diag-error-hint-face)
	 (overlay-put new-err-ext 'mouse-face 'opal-diag-mouse-hint-face))
	 ((equal 'warning (opal-diag-parse-type type))
	  (overlay-put new-err-ext 'face 'opal-diag-error-warning-face)
	  (overlay-put new-err-ext 'mouse-face 'opal-diag-mouse-warning-face))
	 (t
	  (overlay-put new-err-ext 'face 'opal-diag-error-error-face)
	  (overlay-put new-err-ext 'mouse-face 'opal-diag-mouse-error-face))
	 )
;  (set-extent-keymap new-err-ext ext-keymap)
  )

(defun opal-diag-parse-error-found ()
  (opal-diag-parse-counter silent)
  (opal-diag-parse-set-start-position)
  (setq ext-keymap (make-sparse-keymap))
  (if (not src)
      (progn
	(opal-diag-parse-ext-for-unknown)
	(overlay-put new-src-ext 'err-no -1)
	(overlay-put new-err-ext 'err-no -1)
	)
    (opal-diag-parse-ext-keymap)
    (opal-diag-parse-src-ext)
    (opal-diag-parse-err-ext)
    (overlay-put new-src-ext 'err-no i)
    (overlay-put new-err-ext 'err-no i)
    (setq error-list 
	  (cons (opal-diag-make-diag (opal-diag-parse-type type)
		 new-src-ext new-err-ext) 
		error-list))
    (setq i (+ i 1))
    )
  )


(make-face 'opal-diag-source-error-face)
(set-face-foreground 'opal-diag-source-error-face "black")
(set-face-background 'opal-diag-source-error-face "gold")

(make-face 'opal-diag-mouse-error-face)
(set-face-foreground 'opal-diag-mouse-error-face "white")
(set-face-background 'opal-diag-mouse-error-face "red")
(make-face 'opal-diag-error-error-face)
(set-face-foreground 'opal-diag-error-error-face "red")

(make-face 'opal-diag-mouse-warning-face)
(set-face-foreground 'opal-diag-mouse-warning-face "white")
(set-face-background 'opal-diag-mouse-warning-face "magenta")
(make-face 'opal-diag-error-warning-face)
(set-face-foreground 'opal-diag-error-warning-face "magenta")

(make-face 'opal-diag-mouse-hint-face)
(set-face-foreground 'opal-diag-mouse-hint-face "white")
(set-face-background 'opal-diag-mouse-hint-face "SteelBlue")
(make-face 'opal-diag-error-hint-face)
(set-face-foreground 'opal-diag-error-hint-face "SteelBlue")

(defun opal-diag-not-found ()
  (interactive)
  (message "source file for this diagnostic could not be found.")
)


(defun opal-diag-read-int ()
  (let ((s nil))
    (while (looking-at "[0-9]")
      (setq s (concat s (char-to-string (following-char))))
      (forward-char 1))
    (string-to-int s)))

;;; $Support for extended help$

(defvar opal-diag-info-buffer "*opal-diag-information $Revision: 1.7 $*"
  "name of buffer to display extended information" )

(defun opal-diag-extended-show (err-ext)
  "show information for current diagnostic" 

;  (interactive)
  (switch-to-buffer (overlay-buffer err-ext))
  (goto-char (overlay-start err-ext))
  (setq acterror (opal-current-line))
  (set-buffer opal-diag-info-buffer)
  (erase-buffer)
  (insert acterror)
  (goto-char (point-min))
  (if (re-search-forward "^[^:]*:" nil t)
      (replace-match "")
  )
  (let ((el opal-diag-extended-doku-alist)
	(searching t)
       )
       (while (and el searching)
	 (let* ((pair (car el))
		(rest (cdr el))
		(mat  (car pair))
		(msg  (cdr pair))
	       )
           (cond ( (re-search-forward mat nil t)
                   (replace-match msg)
		   (goto-char (point-min))
		   (fill-region (point-min) (point-max) nil)
                   (setq searching nil)
                 )
           )
	   (setq el rest)
	 ); let*
       ); while
  ); let
)


(defun opal-diag-toggle-extended-flag ()
  "toggle current-value of extended-flag with appropriate action"

  (interactive)
  (cond (opal-diag-extended-flag
	 (setq opal-diag-extended-flag nil)
	 (cond ((get-buffer opal-diag-info-buffer)
		(delete-windows-on (get-buffer opal-diag-info-buffer))
	       )
	 )
        )
	(t 
	 (setq opal-diag-extended-flag t)
	 (opal-diag-show-current)
	)
  )
)

;;; $Standard reaction on some Opal errors$

(defun opal-diag-insert-missing-item-str-existp ()
  "t, if current diagnostic contains an item which may be imported from a missing structure; sets match 1 to this item and match to this structure"

  (interactive)
  (let ((cline (opal-current-line)))
    (or
     (string-match "import of .* needs import of \\([^']+\\)'\\([^']+\\)$" cline)
    )
   )
)

(defun opal-diag-insert-missing-item-multi-str-existp ()
  "t, if current diagnostic contains an item which may be imported from a missing structure; sets match 1 to this item and match to this structure"

  (interactive)
  (let ((cline (opal-current-line)))
    (or
     (string-match "import of .* needs import of \\(.*and [^']+'[^']+\\)$" cline)
    )
   )
)

(defun opal-diag-insert-missing-item-existp ()
  "t, if current diagnostic contains an item which may be imported; sets 
match 1 to this word"

  (interactive)
  (let ((cline (opal-current-line)))
    (or
     (string-match "no matching operation for \\(.*\\)$" cline)
     (string-match "no matching free constructor for \\(.*\\)$" cline)
     (string-match "undefined [Ii]dentification of \\(.*\\)$" cline)
     (string-match "import of .* needs import of \\(.*\\)$" cline)
    )
   )
)

(defun opal-diag-replace-underscorep ()
  "t, if current diagnostic contains an identifier which may be replaced by underscore; sets match 1 to this identifier"

  (interactive)
  (let ((cline (opal-current-line)))
    (or
     (string-match "unused pattern variable \\(.*\\)$" cline)
    )
   )
)

(defun opal-diag-expected-p ()
  "t, if current diagnostic contains a message about expected item"

  (interactive)
  (let ((cline (opal-current-line)))
    (or
     (string-match "Expected was `\\([^']*\\)'$" cline)
    )
   )
)
  

(defun opal-diag-insert-missing-item ()
  "insert last missing item as import"

  (interactive)
  (save-excursion
    (if (not opal-diag-buffer)
	(opal-diag-find-diag)
      )
    (set-buffer opal-diag-buffer)
					;    (goto-char (window-start))
    (cond ((opal-diag-insert-missing-item-str-existp)
	   (let ((it (substring (opal-current-line) 
				(match-beginning 1) (match-end 1)))
		 (str (substring (opal-current-line) 
				 (match-beginning 2) (match-end 2)))
		 )
	     (opal-diag-select-source)
	     (opal-import-str-it str it (opal-diag-ask-kind it))
	     )
	   )
	  ((opal-diag-insert-missing-item-multi-str-existp)
	   (let ((msg (substring (opal-current-line) 
				 (match-beginning 1) 
				 (match-end 1))))
	     (opal-diag-select-source)
	     (opal-diag-import-multi  msg 0)
	     )
	   )
          ((opal-diag-insert-missing-item-existp)
	   (let ((it (substring (opal-current-line) 
				(match-beginning 1) (match-end 1)))
		 )
	     (opal-diag-select-source)
	     (opal-import-item it)
	     )
	   )
	  ((opal-diag-replace-underscorep)
	   (let ((ide (substring (opal-current-line) 
				 (match-beginning 1) (match-end 1)))
		 )
	     (opal-diag-select-source)
	     (re-search-forward (regexp-quote ide))
	     (replace-match "_")
	     )
	   )
	  ((opal-diag-expected-p)
	   (let ((it (substring (opal-current-line) 
				(match-beginning 1) (match-end 1)))
		 )
	     (opal-diag-select-source)
	     (insert it)
	     ))
	  (t (message "%s" (opal-current-line)))
	  )
  )
    ;(pop-to-buffer (opal-diag-select-source))
;  (opal-diag-next-main-error)
  (opal-diag-show-error)
)

(defun opal-diag-ask-compare (&optional sort)
  "ask for sort and return kind of associated compare function"
  (interactive)

  (if (not sort)
      (setq sort (read-string "what sort:")))
  (if (string-equal sort "")
      nil
    (concat " : " sort " ** " sort " -> bool")
    )
)

(defun opal-diag-ask-kind (it)
  "return appropriate kind for item"
  (let ((kind
	 (cond ((string-match it "[a-zA-Z0-9]+")
		(setq kind ":SORT"))
	       ((string-match it "[<=]")
		(setq kind (opal-diag-ask-compare)))
	       (t
		(setq kind nil)))
	 ))
    kind
    )
)

(defun opal-diag-import-multi (string start)
  "expect a string ITEM'ORIGIN + separated by \" and \" and perform imports"
  (let ((bstring (concat ")" string)))
    (cond ((string-match ")\\([^' ]+\\)'\\([^' ]+\\)" bstring 0)
	   (let ((newstart (match-end 0))
		 (str (substring bstring (match-beginning 2) (match-end 2)))
		 (it (substring bstring (match-beginning 1) (match-end 1)))
		 kind
		 )
	     (setq kind (opal-diag-ask-kind it))
	     (opal-import-str-it str it kind)
	     (opal-diag-import-multi (substring bstring newstart) 0)
	     )
	   )
	  ((string-match ") *(" (concat bstring "(") 0) nil)
	  ((string-match ")\\(and +\\| +\\)" bstring 0)
	   (opal-diag-import-multi (substring bstring (match-end 0)) 0))
	  (t (message "<%d>%s" start bstring)))
    )
  )

   
(defun opal-diag-show-std-info (err-ext)
  "show information on standard reaction for error at given position"
  (save-excursion
    (switch-to-buffer (overlay-buffer err-ext))
    (goto-char (overlay-start err-ext))
    (let ((type-msg (substitute-command-keys 
		     "type \\[opal-diag-insert-missing-item]")))
      (cond 
       ((opal-diag-insert-missing-item-str-existp)
	(message "%s to import %s from %s" type-msg
		 (substring (opal-current-line) 
			    (match-beginning 1) (match-end 1))
		 (substring (opal-current-line) 
			    (match-beginning 2) (match-end 2))))
       ((opal-diag-insert-missing-item-multi-str-existp)
	(message "%s to import missing items" type-msg))
       ((opal-diag-insert-missing-item-existp)
	(message "%s to import %s" type-msg
		 (substring (opal-current-line) 
			    (match-beginning 1) (match-end 1))))
       ((opal-diag-replace-underscorep)
	(message "%s to replace %s by _" type-msg
		 (substring (opal-current-line) 
			    (match-beginning 1) (match-end 1))))
       ((opal-diag-expected-p)
	(message "%s to insert %s" type-msg
		 (substring (opal-current-line) 
			    (match-beginning 1) (match-end 1))))
       (t (message nil))
       )
      )
    )
)

    

(defun opal-current-line ()
  "returns contents of current line."

  (save-excursion
    (beginning-of-line)
    (let ((a (point)))
         (end-of-line)
	 (let ((b (point)))
	      (buffer-substring a b)
	 )
    )
  )
)

(defun opal-next-line ()
  "returns contents of next line."

  (save-excursion
    (forward-line)
    (let ((a (point)))
         (end-of-line)
	 (let ((b (point)))
	      (buffer-substring a b)
	 )
    )
  )
)

;; ----------------------------------------------------------------------
;; the data for the extended diagnostic information

(setq opal-diag-extended-doku-alist 
	'(
;;; HINTS ;;;
("assumed right associativity of \\(.*\\)\\('.*\\)" .
          "There is an expression containing several occurrences of \\1\\2 
as infixes. These infixes are parsed in a right associative way, i.e. 
`a \\1 b \\1 c' is parsed as `a \\1 (b \\1 c)'. 

This usually is what is wanted. Two of the few exceptions are the arithmetic 
operators `-' and `/'. If you do not want right associative parsing, you must 
provide parentheses.")

("local name \\(.*\\) is not used" .
	   "This hint may be caused by a typing error, better check the correct 
spelling of `\\1'. 

If you actually do not want to use the variable `\\1', better rename 
it to `_' and the compiler will not complain again.")

("unused operation \\(.*\\)" .
            "The operation \\1 was declared at the indicated place but never 
used or exported for further use. 

This can happen in a DATA declaration, if you did not export the corresponding 
free type. Otherwise you can remove the declaration and the definition of 
\\1 without affecting the usefulness of this structure.

Note that you cannot use this function in the oasys evaluator, because the optimizer will remove it. The simplest way to keep the optimizer from removing it is to export this function.")

("unused sort \\(.*\\)" .
             "The sort \\1 was declared at the indicated place but never used nor exported.")

("the following names are imported (Gottfried's rule): " .
             "The name in the import list is ambiguous. All of the following functions match this name and are imported into the current structure.

Normally, ambiguity causes an error, but in this case all matching functions are selected. This may cause ambiguities in expressions you were not aware of.

Gottfried Egger is a former member of the Opal Group who suggested importing all matching names instead reporting an error.")

("unused pattern variable \\(.*\\)" .
	   "The variable `\\1' on the left-side pattern of the definition is never referenced in the expression on the right side. If this is deliberate, you should change the pattern variable to `_' and the compiler will not generate this hint again. Otherwise, you most probably misspelled the name on the right hand side (and probably some error messages follow this hint).")

;;; WARNINGS ;;;
("ambiguous patterns at \\(.*\\) for \\(.*\\)" .
          "The equations at \\2 all apply to the pattern \\1. The compiler will chose one of them arbitrarily. This does not matter if the right hand side is equal in these cases. If you cannot figure out, why the pattern \\1 applies to the equations at \\2, check that all constructors are actually imported - if they are not, the constructor names will be variable names!")

("imported file '\\(.*\\)/\\(.*\\)\\.extp\\.inter' not found: only 'signature export' imported" .
          "Imports in property parts import from the corresponding property part. The abstract syntax of the external property part of structure \\2 was not found, therefore only items from the signature part are actually imported.

You most probably forgot to compile the property parts of \\2.")

("\\(.*\\)\\('.*\\)\\(:SORT\\) is not implemented" .
           "The indicated sort \\1 is declared but is not implemented. This will result in a runtime error if a function is called to construct an element of this sort."
)

("\\(.*[a-zA-Z0-9]\\)\\('.*\\):\\(.*\\) is not implemented" .
           "The indicated function \\1: \\3 is declared but is not implemented. This will result in a runtime error if the function is called."
)

("\\(.*\\)\\('.*\\):\\(.*\\) is not implemented" .
           "The indicated function \\1 : \\3 is declared but is not implemented. This will result in a runtime error if the function is called."
)

("unknown escape `.\\(.\\)' replaced by `\\(.\\)'" .
            "There is a fixed set of escape sequences to denote special characters. `\\\\\\1' is not one of those seqences.

Allowed sequences are:

\\\\a (alert), \\\\b (backspace), \\\\f (formfeed), \\\\n (newline), \\\\r (carriage return), \\\\t (tabulator), \\\\v (vertical tabulator), \\\\\\\\ (literal backslash), \\\\? (question mark), \\\\' (single quote), \\\\\" (double quote)
\\\\x followed by an arbitrary number of hexadecimal digits, denotes the character at that code position, \\\\ followed by one, two or three octal digits is an alternative to denote a certain character.

\\\\u followed by one, two, three or four hexadecimal digits (unicode escape), likewise denotes the character at that code position. Note that unicode escapes are processed outside of denotation constants, too, and that this expansion takes place before the expansion of the other characters. ")

("pattern variable \\(.*\\) hides constant \\(.*\\)'\\(.*\\):\\(.*\\)" .
            "The pattern variable \\1 has the same name and type as a global constant from structure \\3. Within the function definition, \\1 will designate the pattern variable and not the global constant.

Best practice is to rename the pattern variable in order not to confuse the use of \\1 as pattern variable and as global constant in this structure.")

;;; ERRORS ;;;
("ambiguous identification: cannot select from" .
           "The compiler cannot decide which of the functions or typings mentioned should be applied here. You must annotate the function or the variable. This message often comes several times, once for every subterm. In this case, you should annotate the innermost subterm and try again.")

("ambiguous identification: functionality of local name \\(.*\\):\\([^]]*\\)\\(\\[.*\\) ambiguous" .
           "The proper instantiation of the local variable \\1 cannot be deduced in this context. The 'var(xx)' entries in \\2\\3 show which parts cannot be deduced; usually functions must be supplied. 

Either import \\2 instantiated or supply the instantiation at the application.")

("ambiguous identification: functionality of local name \\(.*\\):\\(.*\\) ambiguous" .
           "The proper functionality of the local variable \\1 cannot be deduced in this context. The 'var(xx)' entries in \\2 show which parts cannot be deduced.")

("ambiguous Identification of \\(.*\\): instantiation missing" .
           "The instantiation of \\1 cannot be deduced from the context. Context consists of of imports and declarations only.")

("ambiguous Identification of instantiation" .
           "The instantiation cannot be deduced from the context. Context consists of of imports and declarations only. (?)")

("ambiguous Identification of \\(.*\\)" .
           "The instantiation of \\1 cannot be deduced from the context. Context consists of of imports and declarations only. (?)")

("ambiguous identification: uninstantiated global name \\(.*\\)'\\([^]]*\\)\\(\\[.*\\)\\(:.*\\)" .
           "The proper instantiation of imported name \\1 cannot be deduced in this context. The 'var(xx)' entries in \\2\\3 show which parts cannot be deduced; usually functions must be supplied. Either import \\2 instantiated or supply the instantiation at the application.")

("ambiguous infix application:\\(.*\\)" .
           "The infix application cannot be resolved, because there are several type correct possibilities. The error message contains a scheme of the offending expression, where dots stand for subexpressions and the functions whose infix application cannot be resolved are marked. Add parentheses and try again.")

("Application of _ not allowed here" .
           "The use of a section (?) is not allowed here. Check if all functions applied in a section are imported.")

("\\(`.*'\\) : codes above \\\\u00FF not supported.*" .
           "Though unicode escapes allow the notation of 65536 characters, the current OPAL scanner only supports the first 256.")

("compound object: not allowed for operations (and local names)" .
           "You may not declare a function or a local variable to be of a product type. 

If this error occurs in a function declaration, the easiest explanation is that you typed '*' instead of '**', but you may also have forgotten the codomain of the functionality, and check the correct instantiation of the sorts in the domain, too. 

If you really need to have a compound object here, use pair, triple or quadruple from the Bibliotheca Opalica.")

("direct cyclic initialization: \\(.*\\)" .
           "The constant \\1 is defined in terms of itself, which is not allowed. This error is detected in a very late stage of the compilation, and the position mey therefore be wrong.

All constants are evaluated once, when the program is started. Therefore this error occurs even if the constant is not applied. You may use the empty tuple () in the domain of the functionality to delay evaluation.")

("duplicate implementation: \\(.*\\)\\('.*\\)\\(:.*\\) already implemented at \\(.*\\)" .
           "The function \\1\\3 is implemented by a DATA item at \\4 and cannot be further implemented.")

("duplicate or overloaded local name \\(.*\\) (also added at \\(<[0-9]+,[0-9]+>\\))" .
           "Local names cannot be overloaded. The name \\1 is already declared as a local variable at \\2. Check for a typing error, if it is not an error, you have to rename one occurrence of \\1.")

("Expected was >> \\(.*\\) << as Structure Name" .
    "The name of a structure and the base name of the file where it is located must be the same.")

("Expected was \\(`.*'\\) instead of \\(`.*'\\)" .
           "The compiler expected \\1 and found instead \\2. Perhaps you forgot \\1, perhaps \\2 is at the wrong place.")

("Expected was `\\(.*\\)'" .
           "The compiler did not find a \\1 token before the next item. Perhaps you forgot it, but probably you need to look closer.")

("Found unexpected Ide\"\\(.*\\)\"".
           "The identifier `\\1' is not allowed in this place. 

Check the preceding text carefully for typing errors and missing keywords. Some expressions may not be used without brackets as infix arguments.")

("Found unexpected \\(.*\\)".
           "The token `\\1' is not allowed in this place. 

Check the preceding text carefully for typing errors and missing keywords. Some expressions may not be used without brackets as infix arguments.")

("illegal uninstantiated import of '\\(.*\\)'" .
            "The structure \\1 may not imported unparameterized in this structure part. You must supply the parameters in the IMPORT item.")

("import of \\(.*\\) needs import of \\(.* and .*\\)" .
           "The object \\1 somehow uses the the objects \\2, most often \\1 denotes a function, in the functionality of which the types \\2 are used. If you can't figure out why \\2 are needed, remember that all objects with the same name are imported, types and functions (constructors, selectors, discriminators) alike. 

If you want to restrict the import to a particular object, add the kind to the name (\\1:SORT or \\1:functionality).")

("import of \\(.*\\)'\\(.*\\):\\(.*\\) needs import of \\(.*\\)'\\(.*\\)" .
           "The object \\1 somehow uses the the object \\4, most often \\1 denotes a function, in the functionality of which the type \\4 is used. If you can't figure out why \\4 is needed, remember that all objects with the same name are imported, types and functions (constructors, selectors, discriminators) alike. 

If you want to restrict the import to a particular object, add the kind to the name (\\1:SORT or \\1:functionality). Otherwise add a line IMPORT \\5 ONLY \\4.")

("imported structure '\\(.*\\)' not in command line" .
           "This error can happen if you use the OPAL compiler's front end directly, or if you added an IMPORT \\1 item and saved the file after invoking ocs. In this case simply start ocs again.")

("improperly named function definition target or parameter \\(.*\\)" .
           "There is no declaration of a function \\1. 

Check for typing errors and commented declarations.")

("improperly instantiated import" .
           "The instantiation given is not correct. Check the parameters, if they all exist and are of the correct type.")

("\\(`\.*'\\) is a non existent character!" .
           "The available characters range from \\\\x0 - \\\\xFF (or \\\\0 - \\\\377 in octal notation). \\1 lies outside that range.

Note that the \\\\x escape takes _all_ following hexadecimal digits. Octal notation takes up one to three octal digits.")

("incompatible else" . 
        "The types of the `THEN' expressions (given as `guards:') and the `ELSE' expression (given as `else:') are different.")

("local name '\\(.*\\)' is used with conflicting types: '\\(.*\\)'&'\\(.*\\)'" .
           "The compiler can't decide which type the local name `\\1' should have. You probably forgot some conversion. If you cannot find the error, type the variable explicitly, either `\\1:\\2' or `\\1:\\3', so the compiler can point you to the error in the next pass.")

("Missing Infix Operator" .
           "This error results most often from a mixture of infix and postfix applications which cannot be resolved. Another reason is the omission of an infix operator. Check the expression, supply parentheses for postfix expressions.")

("Missing Operand" .
	   "Check for wrong usage of mixfix and postfix function symbols. You perhaps forgot a comma between two expressions.")

("Missing Operator" .
	   "Check for wrong usage of mixfix and postfix function symbols on the left-hand side of a definition equation. You perhaps forgot a comma between two expressions.")

("no matching free constructor for \\(.*\\)" .
           "You used \\1 in a pattern on the left-hand side of a definition. \\1 is not known to be a free constructor of the corresponding sort. Check whether you imported \\1.")

("no matching name for \\(.*\\)" .
           "The compiler knows nothing about a function (or a variable) `\\1'.

If it should be a function, you might have forgotten to import this function. If it should be a variable, you may have mistyped its name. If you did specify origin and/or instantiation check both for errors. [in instantiation ?]")

("no matching operation for \\(.*\\)" .
           "The compiler knows nothing about a function (or a variable) `\\1'.

If it should be a function, you might have forgotten to import this function. If it should be a variable, you may have mistyped its name. If you did specify origin and/or instantiation check both for errors.")

("no possible bracketing for infix found" .
           "You either forgot to import the function used as infix, or there is an error in one of the expressions involved. If you cannot figure out the error, you will have to supply brackets.\n\nYou may omit brackets in expressions like `a o b & c % d' if one of the following conditions holds: \n\n[1] The functions o & % are all the same (so the expression above looks like `a + b + c + d'). In this case, the compiler will assume right-associativity and interpret the expression as `a + (b + (c + d))'. \n\n[2] There is only one bracketing which respects the functionalities of the functions and the expressions.")

("`/\\*` not closed" .
            "The comment started at the indicated position was not closed by a corresponding `*/`. Note that '.*/' or '!*/' are valid identifiers in Opal and do not close a comment.

If you want to comment just one line, use '--' which marks everything up to the end of the line as comment.")

("`\"` not closed before end of line" .
            "Check the indicated line carefully for a forgotten \". You cannot enclose newline literally in OPAL denotations (use \\\\n for this purpose).")

("recursive let/where: direct recursive equation" .
            "The variable on the left hand side of the equation also appears on the right hand side. This produces an recursive equation which is not allowed.")

("recursive let/where: indirect recursive equation, involving equations at \\(.*\\)" .
            "The equations at \\1 form a cyclic definition of the variable(s) ob the left hand side of the current equation. This produces an indirect recursive equation which is not allowed.")

("undefined application of \\(.*\\) in only list" .
           "There is no item named \\1 in the interface. Check for typing errors, also for mistyped 'ONLY' and 'COMPLETELY' keywords. The error may also be caused by a mistyped explicit functionality.")

("undefined identification" .
	   "The function name is known to the compiler, but none of the possibilities is applicable here. This error message is followed by a (sometimes long) list of the possibilities the compiler considered, i.e. one line which contains a functionality, followed by a line which contains the actual functionalities of the arguments. Not evaluated sorts are shown as 'var(xx)', where xx is a natural number.

Check the functionalities, if the desired functionality is not among them, you may have forgotten to import the function you want to apply here.

If `delivered' is a prefix of `demanded', you may have forgotten some parameters of the type mentioned in `demanded'.")

("undefined Identification of \\(.*\\)" .
	   "If \\1 is a function, The function name `\\1' is known to the compiler, but none of the possibilities is applicable here. If \\1 is a sort, the compiler knows nothing about a sort \\1.

In case of a function, this error message is followed by a (probably long) list of the possibilities the compiler considered, i.e. one line which contains a functionality, followed by a line which contains the actual functionalities of the arguments. Not evaluated sorts are shown as 'var(xx)', where xx is a natural number.

Check the functionalities, if the desired functionality is not among them, you may have forgotten to import the function you want to apply here. If function and argument seem unifiable, check the (non-printed) origins!

In case of a sort, check whether you imported the correct instantiation.")
;; sorte nicht bekannt
("unexpected type of expression$
^    demanded:  \\(.*\\)$
^    delivered: \\(.*\\)$" .
           "In this context, only the type `\\1' is possible, but the expression turned out to have actually type `\\2'.")

("unexpected type of expression" .
           "In this context, only the type `demanded' is possible, but the expression turned out to have actually type `delivered'.")

("uninstantiated import of '\\(.*\\)'" .
           "Within SIGNATURE parts, uninstantiated imports are not allowed. You must explicitly give the instantiation of the import of structure \\1.")

("wrong number of parameters in instantiated import of '\\(.*\\)'" .
           "The structure \\1 has a different number of parameters than given here.")

("wrongly typed application" .
           "In a main error this means that there is exactly one function known with this name, but the arguments supplied have the wrong functionality. In a suberror this is one of the considered possibilities. You find below the functionality of the function and of its arguments. 

Check if you imported the desired function, keep in mind that there may be different functions with the same name and different origin. Another possibility is that you inadvertently named a function parameter like the desired function.")

("wrongly typed equation" .
           "The left and the right side of the equation have different types. You find below the types of the left- and the right-hand side of the equation.

If the left-hand side is explicitly typed, there is no possibility that the right-hand side is of that type. If the left-hand side is a tuple, the right-hand side is not of a tuple type or of a tuple type of different length.")

("wrongly typed implementation" .
           "The types of the left-hand side and the right-hand side of the equation do not match. You find below the types which the compiler computed for the left-hand side and the right-hand side of the equation. If you think that the expression is correctly typed, you should make sure that the function declaration matches your intention.")

(".*" . 
	   "Sorry, there is no help available for this diagnostic.")
	)
)

(put 'opal-diag-extended-doku-alist 'variable-documentation
     "alist of regexps matching diagnostics and associated information.")
