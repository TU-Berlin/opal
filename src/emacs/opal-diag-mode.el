;;; emacs mode for processing Opal diagnostics

;;; Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
;;; See OCSHOME/etc/LICENSE or 
;;; http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
;;; $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/emacs/opal-diag-mode.el,v 1.13 1999-05-11 13:32:28 kd Exp $

(provide 'opal-diag-mode)
(require 'opal-diag-messages)

;;; $User Variables$

(defvar opal-diag-extended-flag t  "*if t then show extended help")

(defvar opal-diag-background "lightyellow"
  "*colour to use for background of diagnostics")

;;; $Error List$
;; sigh .. again, differences between xemacs and fsf-emacs
;;; $$XEmacs Variant$
(if opal-running-xemacs 
(progn
(defvar opal-diag-errors nil
  "Vector of diags (type src-extent diag-extent).")

(defun opal-diag-make-diag (type src err) (list type src err))
(defun opal-diag-type-of (diag)  (car diag))
(defun opal-diag-src-of  (diag)  (cadr diag))
(defun opal-diag-err-of  (diag)  (caddr diag))

(defun opal-diag-delete-diag (diag)
 "delete extents of given diag"
 (delete-extent (opal-diag-src-of diag))
 (delete-extent (opal-diag-err-of diag))
)

(defun opal-diag-src-buffer-of (diag)
  "buffer of source for diag"
  (extent-object (opal-diag-src-of diag))
)

(defun opal-diag-err-buffer-of (diag)
  "buffer of diag file for diag"
  (extent-object (opal-diag-err-of diag))
)

(defun opal-diag-src-start (diag)
  "start of associated source"
  (extent-start-position (opal-diag-src-of diag))
)

(defun opal-diag-err-start (diag)
  "start of associated error"
  (extent-start-position (opal-diag-err-of diag))
)

(defun opal-diag-highlight (from to buffer highlight-face)
  "highlight text between from and to in buffer with highlight-face"
  (set-extent-face (make-extent from to buffer) highlight-face)
)

(defun opal-diag-def-diag (from to buffer keymap dface dmface)
  "create extent with these properties and return it"
  (let (r)
    (setq r (make-extent from to buffer))
    (set-extent-keymap r keymap)
    (set-extent-face r dface)
    (set-extent-mouse-face r dmface)
    r
    )
  )

(defun opal-diag-def-errno (ext i)
  "define associated err number (ignored in XEmacs [number is stored in keymap])"
)

)) ;; end of XEmacs only

;;; $$FSF-Emacs Variant$
(if (not opal-running-xemacs) 
(progn
(defvar opal-diag-errors nil
  "Vector of diags (type src-overlay diag-overlay).")

(defun opal-diag-make-diag (type src err) (list type src err))
(defun opal-diag-type-of (diag)  (car diag))
(defun opal-diag-src-of  (diag)  (cadr diag))
(defun opal-diag-err-of  (diag)  (car (cddr diag)))


(defun opal-diag-delete-diag (diag)
 "delete overlays of given diag"
 (delete-overlay (opal-diag-src-of diag))
 (delete-overlay (opal-diag-err-of diag))
)

(defun opal-diag-src-buffer-of (diag)
  "buffer of source for diag"
  (overlay-buffer (opal-diag-src-of diag))
)

(defun opal-diag-err-buffer-of (diag)
  "buffer of source for diag"
  (overlay-buffer (opal-diag-err-of diag))
)

(defun opal-diag-src-start (diag)
  "start of associated source"
  (overlay-start (opal-diag-src-of diag))
)

(defun opal-diag-err-start (diag)
  "start of associated error"
  (overlay-start (opal-diag-err-of diag))
)

(defun opal-diag-highlight (from to buffer highlight-face)
  "highlight text between from and to in buffer with highlight-face"
  (overlay-put (make-overlay from to buffer) 'face highlight-face)
)

(defun opal-diag-def-diag (from to buffer keymap dface dmface)
  "create overlay with these properties and return it (keymap is ignored)"
  (let (r)
    (setq r (make-overlay from to buffer))
    (overlay-put r 'face dface)
    (overlay-put r 'mouse-face dmface)
    r
    )
)

(defun opal-diag-def-errno (ovl i)
  "define associated err number"
  (overlay-put ovl 'err-no i)
)

)) ;; end of FSF Emacs only

;;; $Moving in the Error List$
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
	  (opal-diag-delete-diag (opal-diag-get-current-diag))
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
  (let ((x (opal-diag-find-diag t)))
  (setq opal-diag-show-what 'all)
  (setq opal-diag-number-errors nil)
  (if x (opal-diag-show-error))
))

(defun opal-diag-show-errors-and-warns ()
  "Show only errors and warnings."
  (interactive)
  (let ((x (opal-diag-find-diag t)))
  (setq opal-diag-show-what 'warning)
  (setq opal-diag-number-errors nil)
  (if x (opal-diag-show-error))
))

(defun opal-diag-show-errors ()
  "Show only errors."
  (interactive)
  (let ((x (opal-diag-find-diag t)))
  (setq opal-diag-show-what 'error)
  (setq opal-diag-number-errors nil)
  (if x (opal-diag-show-error))
))

;;; $Menus and Keybindings$
;; global key-bindings
(defun opal-diag-xemacs-menu ()
  "Set the opal-diag-mode menu fur XEmacs"
  (setq opal-diag-menu 
	(list "Diag"
	      ["Show all diagnostics" opal-diag-show-all 
	       :style radio :selected (equal 'all opal-diag-show-what)]
	      ["Show only errors and warnings" 
	       opal-diag-show-errors-and-warns :style radio
	       :selected (equal 'warning opal-diag-show-what)]
	      ["Show only errors"  opal-diag-show-errors 
	       :style radio :selected (equal 'error opal-diag-show-what)]
	      "---"
	      ["Show previous (sub)error"  opal-diag-prev-error 
	       :active t ;(opal-diag-prev-main-error-enable)
	       :included (not opal-novice)]
	      ["Show next (sub)error" opal-diag-next-error 
	       :active t ; (opal-diag-next-main-error-enable)
	       :included (not opal-novice) ]
	      ["Show current error" opal-diag-show-current
	       :active opal-diag-curr-error]
	      "---"
	      ["Import item from diagnostic" opal-diag-insert-missing-item
	       :active t 
	       :included (not opal-novice)]
	      "---"
	      ["Extended help" opal-diag-toggle-extended-flag 
	       :style toggle :selected opal-diag-extended-flag]
	      ["Update diagnostics buffer"  opal-diag-update t]
	      ["Silently update diagnostics buffer"  opal-diag-update-silent t]
	      ["Clear diagnostics" opal-diag-clear-diags t]
	      "---"
	      ["Show previous main error"  opal-diag-prev-main-error 
	       :active t ; (opal-diag-prev-main-error-enable) 
	       ]
	      ["Show next main error" opal-diag-next-main-error
	       :active t ; (opal-diag-next-main-error-enable) 
	       ]
	      ))
)

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


(if opal-running-xemacs

;; $$Keymap to be used in XEmacs$
(defun opal-mode-diag-keymap ()
  "Set the opal-mode diag keymap."
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
  (define-key opal-mode-map "\C-c\C-d0" 'opal-diag-clear-diags)

  (opal-diag-xemacs-menu)
  (opal-diag-define-faces)
  )

;; $$Keymap to be used in FSF Emacs
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
  (define-key opal-mode-map "\C-c\C-d0" 'opal-diag-clear-diags)
  (define-key opal-mode-map [mouse-1] 'opal-diag-mouse-select-error)
  (define-key opal-mode-map [mouse-2] 'opal-diag-mouse-hide-diags)
  (define-key opal-mode-map [mouse-3] 'opal-diag-mouse-help)

  (opal-diag-fsfemacs-menu)
  (opal-diag-define-faces)
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
		(opal-diag-show-this-error errno t)
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
		  (opal-diag-show-this-error errno t)
		  (opal-diag-toggle-extended-flag t)
		  )
	      (opal-diag-not-found)
	      )
	    (setq ovl nil)
	    )))))

); end of running-xemacs

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
  (opal-toolbar-install)

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
	(if (buffer-live-p opal-diag-buffer)
	    (set-buffer opal-diag-buffer)
	  )
	(delete-other-windows)
	(cond (opal-diag-hide (switch-to-buffer opal-diag-hide))
	      (opal-diag-source (switch-to-buffer opal-diag-source))
	      (opal-diag-errors (switch-to-buffer
				 (opal-diag-src-buffer-of 
				  (aref opal-diag-errors 0))))
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

(defun opal-diag-show-current (&optional nogoto)
  "show current diagnostic"
  (interactive)
  (opal-diag-my-diag-p)
  (if opal-diag-curr-error
      (opal-diag-show-error nil nogoto)
    (error "No current diagnostic")
    )
  )

(defun opal-diag-show-this-error (n &optional nogoto)
  "show error with specified number"

  (setq opal-diag-curr-error n)
  (opal-diag-show-error nil nogoto)
)

(defun opal-diag-show-error (&optional nomessage nogoto)
  "Make current error and corresponding source visible."

  (if (not opal-diag-curr-error)
      (if (= (length opal-diag-errors) 0)
	  (error "No diagnostics")
	(error "No current diagnostic")
	)
    (let (src-ext err-ext pop-up-windows err-window cDiag)
      (setq pop-up-windows nil)
      (setq cDiag (opal-diag-get-current-diag))
      (opal-diag-set-windows (opal-diag-src-buffer-of cDiag)
			     (opal-diag-err-buffer-of cDiag))
      ;; show extended help, if flag set
      (if opal-diag-extended-flag 
	  (opal-diag-extended-show cDiag))
      ;; show error in source code
      (switch-to-buffer (opal-diag-src-buffer-of cDiag))
      (if (opal-diag-src-start cDiag)
	  (if (not nogoto) (goto-char (opal-diag-src-start cDiag)))
	(message "this diagnostic is no longer present in source")
	)
      ;; show error message
      (setq err-window (display-buffer (opal-diag-err-buffer-of cDiag)))
      (set-window-point err-window (opal-diag-err-start cDiag))
      (set-window-start err-window (opal-diag-err-start cDiag))
      ;; print message for certain error
      (if (and (not nomessage) (buffer-file-name opal-diag-source))
	  (opal-diag-show-std-info cDiag)
	)
      (switch-to-buffer (opal-diag-src-buffer-of cDiag))
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
      (message "opal-diag-set-windows: nothing to do") ; do nothing if diag-info-buffer exists and all buffers are displayed
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
  (select-window (get-buffer-window src-buf))
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

(defconst opal-diag-parse-ocs-unknown-regexp 
  "<unknown>\\(ERROR\\|WARNING\\|HINT\\)"
  "regexp to match ocs warnings at unknown locations")

(defconst opal-diag-parse-ocs-line-region-regexp 
  "<\\([0-9]+\\),\\([0-9]+\\)-\\([0-9]+\\)>\\(ERROR\\|WARNING\\|HINT\\)"
  "regexp to match ocs line region warnings")

(defconst opal-diag-parse-ocs-region-regexp 
  "<\\([0-9]+\\),\\([0-9]+\\)-\\([0-9]+\\),\\([0-9]+\\)>\\(ERROR\\|WARNING\\|HINT\\)"
  "regexp to match ocs region warnings")

(defconst opal-diag-parse-oasys-regexp
  "\\(ERROR\\|WARNING\\|HINT\\) \\[\\(.*\\) at \\([0-9]+\\)\\.\\([0-9]+\\)"
  "regexp to match oasys warnings")

(defconst opal-diag-parse-oasys-region-regexp
  "\\(ERROR\\|WARNING\\|HINT\\) \\[\\(.*\\) at \\([0-9]+\\)\\.\\([0-9]+\\)-\\([0-9]+\\)\\.\\([0-9]+\\)"
  "regexp to match oasys region warnings")

(defconst opal-diag-parse-oasys-unknown-regexp
  "\\(ERROR\\|WARNING\\|HINT\\) \\[\\(.*\\) at unknown location"
  "regexp to match oasys diagnostics at unknown locations")

(defconst opal-diag-parse-oasys-eval-regexp
  "\\(ERROR\\|WARNING\\|HINT\\) \\[at \\([0-9]+\\)\\.\\([0-9]+\\)"
  "regexp to match oasys diagnostics for evaluated expressions")

(defconst opal-diag-parse-oasys-eval-region-regexp
  "\\(ERROR\\|WARNING\\|HINT\\) \\[at \\([0-9]+\\)\\.\\([0-9]+\\)-\\([0-9]+\\)\\.\\([0-9]+\\)"
  "regexp to match oasys region diagnostics for evaluated expressions")

(defconst opal-diag-parse-suberror-regexp
  " *[0-9]+\\. *<\\([0-9]+\\),\\([0-9]+\\)>\\( \\)"
  "regexp to match suberrors")

(defconst opal-diag-parse-suberror-line-region-regexp
  " *[0-9]+\\. *<\\([0-9]+\\),\\([0-9]+\\)-\\([0-9]+\\)>\\( \\)"
  "regexp to match suberrors")

(defconst opal-diag-parse-suberror-region-regexp
  " *[0-9]+\\. *<\\([0-9]+\\)-\\([0-9]+\\),\\([0-9]+\\)-\\([0-9]+\\)>\\( \\)"
  "regexp to match suberrors")

(defconst opal-diag-error-regexp 
  "\\([0-9]+[]>]\\)\\|unknown location\\|unknown>"
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
	(setq eLine nil)
	(save-excursion (forward-line) (setq err-end (point)))
	(cond
	 ((looking-at opal-diag-parse-ocs-regexp) (opal-diag-handle-ocs))
	 ((looking-at opal-diag-parse-ocs-unknown-regexp)
	  (opal-diag-handle-ocs-unknown))
	 ((looking-at opal-diag-parse-ocs-line-region-regexp)
	  (opal-diag-handle-ocs-line-region))
	 ((looking-at opal-diag-parse-ocs-region-regexp)
	  (opal-diag-handle-ocs-region))
	 ((looking-at opal-diag-parse-oasys-regexp) (opal-diag-handle-oasys))
	 ((looking-at opal-diag-parse-oasys-region-regexp) 
	  (opal-diag-handle-oasys-region))
	 ((looking-at opal-diag-parse-oasys-unknown-regexp)
	  (opal-diag-handle-oasys-unknown))
	 ((looking-at opal-diag-parse-oasys-eval-regexp)
	  (opal-diag-handle-oasys-eval))
	 ((looking-at opal-diag-parse-oasys-eval-region-regexp)
	  (opal-diag-handle-oasys-eval-region))
	 ((looking-at opal-diag-parse-suberror-regexp) 
	  (opal-diag-handle-suberror))
	 ((looking-at opal-diag-parse-suberror-line-region-regexp) 
	  (opal-diag-handle-suberror-line-region))
	 ((looking-at opal-diag-parse-suberror-region-regexp) 
	  (opal-diag-handle-suberror-region))
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
    (if (and unknown-src opal-running-xemacs) ; no popups in fsf emacs!
	(popup-dialog-box (list (concat "* WARNING *\nThe following source files could not be found:\n" unknown-src)
			      [ "OK" '(lambda () (interactive)) t]))
      )
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

(defun opal-diag-handle-ocs-unknown ()

  (setq src curr-src-buf)
  (setq line 0)
  (setq col 1)
  (setq type (opal-diag-match 1))
  (setq is-suberror nil)
  (setq true-error t)
)

(defun opal-diag-handle-ocs-line-region ()

  (setq src curr-src-buf)
  (setq line (string-to-int (opal-diag-match 1)))
  (setq col (string-to-int (opal-diag-match 2)))
  (setq eLine line)
  (setq eCol (string-to-int (opal-diag-match 3)))
  (setq type (opal-diag-match 4))
  (setq true-error t)
)

(defun opal-diag-handle-ocs-region ()

  (setq src curr-src-buf)
  (setq line (string-to-int (opal-diag-match 1)))
  (setq col (string-to-int (opal-diag-match 2)))
  (setq eLine (string-to-int (opal-diag-match 3)))
  (setq eCol (string-to-int (opal-diag-match 4)))
  (setq type (opal-diag-match 5))
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

(defun opal-diag-handle-oasys-region ()
  (setq src (opal-diag-find-source (opal-diag-match 2)))
  (if (not src) 
      (opal-diag-handle-oasys-unknown-src-list (opal-diag-match 2))
    (setq curr-src-buf src)
    )
  (setq line (string-to-int (opal-diag-match 3)))
  (setq col (string-to-int (opal-diag-match 4)))
  (setq eLine (string-to-int (opal-diag-match 5)))
  (setq eCol (string-to-int (opal-diag-match 6)))
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

(defun opal-diag-handle-oasys-eval-region ()
  (setq src eval-buf)
  (setq line (+ 1 (string-to-int (opal-diag-match 2))))
  (setq col (string-to-int (opal-diag-match 3)))
  (setq eLine (string-to-int (opal-diag-match 4)))
  (setq eCol (string-to-int (opal-diag-match 5)))
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

(defun opal-diag-handle-suberror-line-region ()
  (setq src curr-src-buf)
  (setq line (string-to-int (opal-diag-match 1)))
  (setq col (string-to-int (opal-diag-match 2)))
  (setq eLine line)
  (setq eCol (string-to-int (opal-diag-match 3)))
  (setq type "suberror")
  (setq true-error t)
)

(defun opal-diag-handle-suberror-region ()
  (setq src curr-src-buf)
  (setq line (string-to-int (opal-diag-match 1)))
  (setq col (string-to-int (opal-diag-match 2)))
  (setq eLine (string-to-int (opal-diag-match 3)))
  (setq eCol (string-to-int (opal-diag-match 4)))
  (setq type "suberror")
  (setq true-error t)
)

(defun opal-diag-handle-checking ()
  (setq curr-src-buf (opal-diag-find-source
		      (concat (opal-diag-match 1) ".sign")))
  (if (not curr-src-buf) (setq unknown-src
			       (concat unknown-src "\n"
				       (concat (opal-diag-match 1) ".sign"))))
  (opal-diag-highlight err-start err-end (get-buffer opal-diag-buffer)
		       'opal-diag-source-error-face)
  (setq true-error nil)
)

(defun opal-diag-handle-compiling ()
  (setq curr-src-buf (opal-diag-find-source
		      (concat (opal-diag-match 2) ".impl")))
  (if (not curr-src-buf) (setq unknown-src
			       (concat unknown-src "\'opal-diag-source-error-face)n"
				       (concat (opal-diag-match 1) ".impl"))))
  (opal-diag-highlight err-start err-end (get-buffer opal-diag-buffer)
		       'opal-diag-source-error-face)
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
	(if eLine
	    (progn
	      (goto-line eLine)
	      (move-to-column eCol)
	      (setq src-end (point))
	      )
	  (forward-char 1)
	  (setq src-end (point))
	  (if (equal 'suberror (opal-diag-parse-type type))
	      (setq src-end (+ src-end 1)))
	  )
	)
    )
  )

(defun opal-diag-parse-ext-for-unknown ()
  (define-key ext-keymap [(button1)] 'opal-diag-not-found)
  (define-key ext-keymap [(button2)] 'opal-diag-not-found)
  (define-key ext-keymap [(button3)] 'opal-diag-not-found)
  (setq new-err-ext 
	(opal-diag-def-diag err-start err-end
			    (get-buffer opal-diag-buffer)
			    ext-keymap
			    'opal-diag-source-error-face 'default))
)

(defun opal-diag-parse-ext-keymap ()
  (define-key ext-keymap "\M-n" 'opal-diag-next-main-error)
  (define-key ext-keymap "\M-p" 'opal-diag-prev-main-error)
  (define-key ext-keymap [(button1)]
    `(lambda (ev) (interactive "e") (mouse-set-point ev) (opal-diag-show-this-error ,i t)))
  (define-key ext-keymap [(button2)]
    'opal-diag-hide-diag-buffer)
  (define-key ext-keymap [(button3)]
    `(lambda (ev) (interactive "e") (mouse-set-point ev)
       (opal-diag-show-this-error ,i t)
       (opal-diag-toggle-extended-flag t)))
  )

(defun opal-diag-parse-src-ext ()
  (let (sface)
    (cond ((equal 'hint (opal-diag-parse-type type))
	   (setq sface 'opal-diag-mouse-hint-face))
	  ((equal 'warning (opal-diag-parse-type type))
	   (setq sface 'opal-diag-mouse-warning-face))
	  (t
	   (setq sface 'opal-diag-mouse-error-face))
	  )
    (setq new-src-ext 
	  (opal-diag-def-diag src-start src-end (get-buffer src)
			      ext-keymap sface 'opal-diag-source-error-face))
    )
  )

(defun opal-diag-parse-err-ext ()
  (let (eface emface)
    (cond ((equal 'hint (opal-diag-parse-type type))
	   (setq eface 'opal-diag-error-hint-face)
	   (setq emface 'opal-diag-mouse-hint-face))
	  ((equal 'warning (opal-diag-parse-type type))
	   (setq eface 'opal-diag-error-warning-face)
	   (setq emface 'opal-diag-mouse-warning-face))
	  (t
	   (setq eface 'opal-diag-error-error-face)
	   (setq emface 'opal-diag-mouse-error-face))
	  )
    (setq new-err-ext 
	  (opal-diag-def-diag err-start err-end
			      (get-buffer opal-diag-buffer)
			      ext-keymap eface emface))
    )
  )

(defun opal-diag-parse-error-found ()
  (opal-diag-parse-counter silent)
  (opal-diag-parse-set-start-position)
  (setq ext-keymap (make-sparse-keymap))
  (if (not src)
      (progn
	(opal-diag-parse-ext-for-unknown)
	(opal-diag-def-errno new-src-ext -1)
	(opal-diag-def-errno new-err-ext -1)
	)
    (opal-diag-parse-ext-keymap)
    (opal-diag-parse-src-ext)
    (opal-diag-parse-err-ext)
    (opal-diag-def-errno new-src-ext i)
    (opal-diag-def-errno new-err-ext i)
    (setq error-list 
	  (cons (opal-diag-make-diag (opal-diag-parse-type type)
		 new-src-ext new-err-ext) 
		error-list))
    (setq i (+ i 1))
    )
  )

(defun opal-diag-define-faces ()
  "define faces to be used for opal diagnostics"

(make-face 'opal-diag-source-error-face)
(set-face-foreground 'opal-diag-source-error-face "black")
(set-face-background 'opal-diag-source-error-face "gold")

(make-face 'opal-diag-mouse-error-face)
(set-face-foreground 'opal-diag-mouse-error-face "red")
(set-face-background 'opal-diag-mouse-error-face opal-diag-background)
(make-face 'opal-diag-error-error-face)
(set-face-foreground 'opal-diag-error-error-face "red")

(make-face 'opal-diag-mouse-warning-face)
(set-face-foreground 'opal-diag-mouse-warning-face "magenta")
(set-face-background 'opal-diag-mouse-warning-face opal-diag-background)
(make-face 'opal-diag-error-warning-face)
(set-face-foreground 'opal-diag-error-warning-face "magenta")

(make-face 'opal-diag-mouse-hint-face)
(set-face-foreground 'opal-diag-mouse-hint-face "SteelBlue")
(set-face-background 'opal-diag-mouse-hint-face opal-diag-background)
(make-face 'opal-diag-error-hint-face)
(set-face-foreground 'opal-diag-error-hint-face "SteelBlue")

)

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

(defvar opal-diag-info-buffer "*opal-diag-information $Revision: 1.13 $*"
  "name of buffer to display extended information" )

(defun opal-diag-extended-show (diag)
  "show information for current diagnostic" 

;  (interactive)
  (switch-to-buffer (opal-diag-err-buffer-of diag))
  (goto-char (opal-diag-err-start diag))
  (setq acterror (opal-current-line))
  (set-buffer opal-diag-info-buffer)
  (erase-buffer)
  (insert acterror)
  (goto-char (point-min))
  (if (re-search-forward "^.*\\(ERROR\\|WARNING\\|HINT\\):" nil t)
      (replace-match "")
    )
  (if (re-search-forward (concat "^" opal-diag-parse-suberror-regexp) nil t)
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


(defun opal-diag-toggle-extended-flag (&optional nogoto)
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
	 (opal-diag-show-current nogoto)
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
  (opal-diag-show-error t)
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

   
(defun opal-diag-show-std-info (diag)
  "show information on standard reaction for error at given position"
  (save-excursion
    (switch-to-buffer (opal-diag-err-buffer-of diag))
    (goto-char (opal-diag-err-start diag))
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

