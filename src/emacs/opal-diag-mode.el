;; emacs mode for processing opal diag files
;; $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/emacs/opal-diag-mode.el,v 1.3 1998-07-01 17:17:39 kd Exp $
;; Author: wg, changed by ralfi

(provide 'opal-diag-mode)

;; buffer-local variables of opal-diag-mode 

(defvar opal-diag-hidden-diag t
  "*if non-nil use hidden opal-diag-files.")

(defvar opal-diag-errors nil
  "Vector of tripels (mark-in-diag-buf,is-suberror,mark-in-src-buf).")

(defvar opal-diag-extended-flag t
  "*if t then show extended help")

(defconst opal-diag-show-all 0 "show all diags")
(defconst opal-diag-show-errors-and-warns 1 "show all but hints")
(defconst opal-diag-show-errors 2 "show only errors.")

(defun opal-diag-error-enable ()
  (if (= opal-diag-show-what opal-diag-show-errors) nil t))

(defun opal-diag-error-warn-enable ()
  (if (= opal-diag-show-what opal-diag-show-errors-and-warns) nil t))

(defun opal-diag-all-enable ()
  (if (= opal-diag-show-what opal-diag-show-all) nil t))

(defun opal-diag-curr-error-enable ()
  (and opal-diag-number-errors
       (>= opal-diag-curr-error 0)
       (< opal-diag-curr-error opal-diag-number-errors)))

(defun opal-diag-next-main-error-enable ()
  (if opal-diag-number-errors
      (cond ((and opal-diag-number-errors (>= opal-diag-curr-error 0) ; Anfang/Mitte
		  (< (+ 1 opal-diag-curr-error) opal-diag-number-errors)) t)
	    ((and opal-diag-number-errors (>= opal-diag-curr-error 0) ; Ende
		  (< opal-diag-curr-error opal-diag-number-errors)) nil)
	    (t (opal-diag-delete-current-diag-buffer) nil) ; sonst
	    )
    nil
    )
)

(defun opal-diag-prev-main-error-enable ()
  (if opal-diag-number-errors
      (cond ((and opal-diag-number-errors (> opal-diag-curr-error 0)) t) ; Mitte/Ende
	    ((and opal-diag-number-errors (>= opal-diag-curr-error 0)) nil) ; Anfang
	    (t (opal-diag-delete-current-diag-buffer) nil) ; sonst
	    )
    nil
    )
)

;;; (defun opal-diag-curr-error-enable ()
;;;   (save-window-excursion
;;;     (save-excursion
;;;       (opal-diag-find-diag)
;;;       (if (not opal-diag-number-errors) (opal-diag-parse))
;;;       (if (and opal-diag-number-errors (>= opal-diag-curr-error 0) 
;;; 	       (< opal-diag-curr-error opal-diag-number-errors)) t
;;; 	(opal-diag-delete-current-diag-buffer) nil
;;; 	))))
;;; 
;;; (defun opal-diag-next-main-error-enable ()
;;;   (save-window-excursion
;;;     (save-excursion
;;;       (opal-diag-find-diag)
;;;       (if (not opal-diag-number-errors) (opal-diag-parse))
;;;       (cond ((and opal-diag-number-errors (>= opal-diag-curr-error 0) ; Anfang/Mitte
;;; 		  (< (+ 1 opal-diag-curr-error) opal-diag-number-errors)) t)
;;; 	    ((and opal-diag-number-errors (>= opal-diag-curr-error 0) ; Ende
;;; 		  (< opal-diag-curr-error opal-diag-number-errors)) nil)
;;; 	    (t (opal-diag-delete-current-diag-buffer) nil) ; sonst
;;; 	    ))))
;;; 
;;; (defun opal-diag-prev-main-error-enable ()
;;;   (save-window-excursion
;;;     (save-excursion
;;;       (opal-diag-find-diag)
;;;       (if (not opal-diag-number-errors) (opal-diag-parse))
;;;       (cond ((and opal-diag-number-errors (> opal-diag-curr-error 0)) t) ; Mitte/Ende
;;; 	    ((and opal-diag-number-errors (>= opal-diag-curr-error 0)) nil) ; Anfang
;;; 	    (t (opal-diag-delete-current-diag-buffer) nil) ; sonst
;;; 	    ))))
;;; 

;;; Diese Funktionen laufen nur im diag-mode und nicht im opal-mode 
;;; (put 'opal-diag-next-main-error 'menu-enable 
;;;      '(opal-diag-next-main-error-enable))
;;; (put 'opal-diag-prev-main-error 'menu-enable 
;;;      '(opal-diag-prev-main-error-enable))
;;; 
;;; (put 'opal-diag-show 'menu-enable '(opal-diag-curr-error-enable))
;;; (put 'opal-diag-kill-error 'menu-enable '(opal-diag-curr-error-enable))
;;; (put 'opal-diag-next-error 'menu-enable '(opal-diag-next-main-error-enable))
;;; (put 'opal-diag-prev-error 'menu-enable '(opal-diag-prev-main-error-enable))


(defvar opal-diag-show-what opal-diag-show-all "show what diagnostics.")
(setq opal-diag-all-enable nil)

(defconst opal-diag-number-errors nil
  "Number of errors.")

(defvar opal-diag-curr-error nil
  "Current position in opal-diag-errors.")

;; global key-bindings
(defun opal-diag-xemacs-menu ()
  "Set the opal-diag-mode menu fur XEmacs"
  (setq opal-diag-menu 
	(list "Diag"
	      ["Show all diagnostics" opal-diag-show-all 
	       :style radio :selected (null (opal-diag-all-enable))]
	      ["Show only errors and warnings" 
	       opal-diag-show-errors-and-warns :style radio
	       :selected (null (opal-diag-error-warn-enable))]
	      ["Show only errors"  opal-diag-show-errors 
	       :style radio :selected (null (opal-diag-error-enable))]
	      "---"
	      ["Show previous (sub)error"  opal-diag-prev-error 
	       :active t ;(opal-diag-prev-main-error-enable)
	       :included (not opal-novice)]
	      ["Show next (sub)error" opal-diag-next-error 
	       :active t ; (opal-diag-next-main-error-enable)
	       :included (not opal-novice) ]
	      ["Kill current error" opal-diag-kill-error 
	       :active t ; (opal-diag-curr-error-enable) 
	       :included (not opal-novice)]
	      ["Show current error" opal-diag-show 
	       :active t ; (opal-diag-curr-error-enable) 
	       ]
	      "---"
	      ["Import item from diagnostic" opal-diag-insert-missing-item
	       :active t 
	       :included (not opal-novice)]
	      "---"
	      ["Extended help" opal-diag-toggle-extended-flag 
	       :style toggle :selected opal-diag-extended-flag]
	      ["Update diagnostics buffer"  opal-diag-update t]
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
	(define-key opal-mode-map [menu-bar diag opal-diag-kill-error]
	  '("Kill current error" . opal-diag-kill-error))
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
   
  (put 'opal-diag-show-errors 'menu-enable '(opal-diag-error-enable))
  (put 'opal-diag-show-errors-and-warns 'menu-enable 
       '(opal-diag-error-warn-enable))
  (put 'opal-diag-show-all 'menu-enable '(opal-diag-all-enable))
  )

(defun opal-mode-diag-keymap ()
  "Set the opal-mode diag keymap."
;; changed by ralfi
;; shortkeys
  (define-key opal-mode-map "\M-n" 'opal-diag-next-main-error)
  (define-key opal-mode-map "\M-p" 'opal-diag-prev-main-error)
  (define-key opal-mode-map "\M-u" 'opal-diag-update)
  (define-key opal-mode-map "\M-h" 'opal-diag-toggle-extended-flag)
  (define-key opal-mode-map "\M-m" 'opal-diag-insert-missing-item)
;;longkeys
  (define-key opal-mode-map "\C-c\C-d\C-n" 'opal-diag-next-main-error)
  (define-key opal-mode-map "\C-c\C-d\C-p" 'opal-diag-prev-main-error)
  (define-key opal-mode-map "\C-c\C-d\C-u" 'opal-diag-update)
  (define-key opal-mode-map "\C-c\C-d\C-c" 'opal-diag-show)
  (define-key opal-mode-map "\C-c\C-d\C-k" 'opal-diag-kill-error)
  (define-key opal-mode-map "\C-c\C-dn" 'opal-diag-next-error)
  (define-key opal-mode-map "\C-c\C-dp" 'opal-diag-prev-error)
  (define-key opal-mode-map "\C-c\C-d\C-e" 'opal-diag-show-errors)
  (define-key opal-mode-map "\C-c\C-d\C-w" 'opal-diag-show-errors-and-warns)
  (define-key opal-mode-map "\C-c\C-d\C-a" 'opal-diag-show-all)
  (define-key opal-mode-map "\C-c\C-d\C-h" 'opal-toggle-extended-flag)
  (define-key opal-mode-map "\C-c\C-d\C-m" 'opal-diag-insert-missing-item)

  (if opal-running-xemacs
      (progn
	(opal-diag-xemacs-menu)
      )
      (opal-diag-fsfemacs-menu)
  )
  )

;; colors for diag-mode
(defconst opal-diag-font-lock-keywords
  (list
   '("\\(ERROR\\|WARNING\\|HINT\\)" (0 'font-lock-function-name-face t t))
   )
)

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
\\[opal-diag-kill-error] = 'opal-diag-kill-error
          kills the current error (and all its suberrors) and steps
          to the next error (see note below)
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

Note that this mode is implemented using GNU lisp markers, and that the
GNU lisp reference manual recommends to delete markers as soon as
possible since they might slow down the editing process. I'am not sure
if this is only paranoid on modern workstations; if you feel editing
is slowed down use \\[opal-diag-kill-error] to step through errors, and
kill diag buffers as soon as they are no longer required."

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'opal-diag-mode)
  (setq mode-name "OC diagnostics")
  (use-local-map opal-mode-map)         ; This provides the local keymap

  (make-local-variable 'opal-diag-errors)
  (make-local-variable 'opal-diag-number-errors) 
  (make-local-variable 'opal-diag-curr-error)
;  (make-local-variable 'opal-diag-show-what)  ;; global gemacht (ralfi)
  (make-local-variable 'opal-diag-hidden-diag)
  (setq buffer-read-only t)

  (run-hooks 'opal-diag-mode-hook))


;; interactions

(defun opal-diag-delete-current-diag-buffer (&optional delete-diag)
  "Delete current diag buffer and pop up the source code buffer, and ask to save all opal files."
  (if (get-buffer oasys-check-diagnostics-buffer)
      (progn
	(if delete-diag (kill-buffer oasys-check-diagnostics-buffer))
	(delete-other-windows)
	(switch-to-buffer oasys-current-opal-buffer)
      )
    (let ((opal-diag-tmp (substring (buffer-name (current-buffer)) 0 -5)))
      (if (string= ".diag" (substring (buffer-name (current-buffer)) -5 nil))
	  (progn
	    (if delete-diag (kill-buffer (current-buffer)))
	    (delete-other-windows)
	    (switch-to-buffer  opal-diag-tmp))
	))
  )
;  (opal-ask-save-opal-buffers)
)

(defun opal-diag-next-main-error (n)
  "Visit next compilation error and corresponding source code, skipping
sub errors."
  (interactive "p")
  (opal-diag-find-diag)
  (let ((i opal-diag-curr-error)
	(j 0))
    (while (< j n)
      (setq i (+ i 1) j (+ j 1))
      (while (and (< i opal-diag-number-errors) (car (cdr (aref opal-diag-errors i))))
	(setq i (+ i 1))))
    (if (>= i opal-diag-number-errors)
	(progn 
	  (setq opal-diag-curr-error opal-diag-number-errors)
	  (opal-diag-delete-current-diag-buffer)
	  (message "No more diagnostics."))
      (setq opal-diag-curr-error i)
      (opal-diag-show-error)
      )))

(defun opal-diag-prev-main-error (n)
  "Visit previous compilation error and corresponding source code,
skipping sub errors."
  (interactive "p")
  (opal-diag-find-diag)
  (let ((i opal-diag-curr-error)
	(j 0))
    (while (< j n)
      (setq i (- i 1) j (+ j 1))
      (while (and (>= i 0) (car (cdr (aref opal-diag-errors i))))
	(setq i (- i 1))))
    (if (< i 0)
	(progn
	  (setq opal-diag-curr-error -1)
	  (opal-diag-delete-current-diag-buffer)
	  (message "No more diagnostics."))
      (setq opal-diag-curr-error i)
      (opal-diag-show-error))))

(defun opal-diag-next-error (n)
  "Visit next compilation error and corresponding source code."
  (interactive "p")
  (opal-diag-find-diag)
  (let ((i (+ opal-diag-curr-error n)))
    (if (>= i opal-diag-number-errors)
	(progn (opal-diag-delete-current-diag-buffer)
	       (message "No more diagnostics."))
      (setq opal-diag-curr-error i)
      (opal-diag-show-error))))

(defun opal-diag-prev-error (n)
  "Visit previous compilation error and corresponding source code."
  (interactive "p")
  (opal-diag-find-diag)
  (let ((i (- opal-diag-curr-error n)))
    (if (< i 0)
	(progn (opal-diag-delete-current-diag-buffer)
	  (message "No more diagnostics."))
      (setq opal-diag-curr-error i)
      (opal-diag-show-error))))

(defun opal-diag-update ()
  "Update diagnostics buffer."
  (interactive)
;  (delete-other-windows)
  (opal-diag-clear-errors)
  (opal-diag-find-diag)
  (if (get-buffer oasys-check-diagnostics-buffer)
      ()
    (find-alternate-file (buffer-file-name))
  )
  (if opal-diag-curr-error
      (setq opal-diag-curr-error 
	    (max 0 (min opal-diag-curr-error 
			(- opal-diag-number-errors 1))))
  )
  (opal-diag-show-error)
  )

(defun opal-diag-show ()
  "Show current error."
  (interactive)
  (let ((x (opal-diag-find-diag t)))
  (if x (opal-diag-show-error))
))

(defun opal-diag-show-all ()
  "Show all diagnostics."
  (interactive)
  (let ((x (opal-diag-find-diag t)))
  (setq opal-diag-show-what opal-diag-show-all)
  (setq opal-diag-number-errors nil)
  (if x (opal-diag-show-error))
))

(defun opal-diag-show-errors-and-warns ()
  "Show only errors and warnings."
  (interactive)
  (let ((x (opal-diag-find-diag t)))
  (setq opal-diag-show-what opal-diag-show-errors-and-warns)
  (setq opal-diag-number-errors nil)
  (if x (opal-diag-show-error))
))

(defun opal-diag-show-errors ()
  "Show only errors."
  (interactive)
  (let ((x (opal-diag-find-diag t)))
  (setq opal-diag-show-what opal-diag-show-errors)
  (setq opal-diag-number-errors nil)
  (if x (opal-diag-show-error))
))

(defun opal-diag-kill-error ()
  "Kill current error."
  (interactive)
  (opal-diag-find-diag)
  (if (= opal-diag-number-errors 0)
      (progn (opal-diag-delete-current-diag-buffer)
	     (error "No more diagnostics.")))
  ;; walk back to main error
  (while (and (> opal-diag-curr-error 0) 
	      (car (cdr (aref opal-diag-errors opal-diag-curr-error))))
    (setq opal-diag-curr-error (- opal-diag-curr-error 1)))
  (let (spos epos error-list (del-cnt 1) (i 0))
     ;; copy until current error
     (while (< i opal-diag-curr-error)
       (setq error-list (cons (aref opal-diag-errors i) error-list))
       (setq i (+ i 1)))
     ;; delete main & sub errors
     (setq spos (marker-position (car (aref opal-diag-errors i))))
     (set-marker (car (aref opal-diag-errors i)) nil)
     (set-marker (car (cdr (cdr (aref opal-diag-errors i)))) nil)
     (setq i (+ i 1))
     (while (and (< i opal-diag-number-errors)
		 (car (cdr (aref opal-diag-errors i))))
       (set-marker (car (aref opal-diag-errors i)) nil)
       (set-marker (car (cdr (cdr (aref opal-diag-errors i)))) nil)
       (setq del-cnt (+ del-cnt 1))
       (setq i (+ i 1)))
     ;; copy rest errors
     (if (< i opal-diag-number-errors)
	 (progn
	   (setq opal-diag-curr-error (- i del-cnt))
	   (setq epos (marker-position (car (aref opal-diag-errors i))))
	   (while (< i opal-diag-number-errors)
	     (setq error-list (cons (aref opal-diag-errors i) error-list))
	     (setq i (+ i 1))))
       (setq opal-diag-curr-error (- opal-diag-number-errors del-cnt 1))
       (setq epos (point-max)))
     (setq opal-diag-number-errors (- opal-diag-number-errors del-cnt))
     (setq opal-diag-errors (vconcat (reverse error-list)))
     (toggle-read-only)
     (delete-region spos epos)
     (toggle-read-only)
     (set-buffer-modified-p nil))
  (opal-diag-show-error))
       
      
(defun opal-diag-find-diag (&optional noerror)
  "From information about the current buffer, find a corresponding
diag buffer and select it. if diagnostics from interpreter exist, 
these are selected"
  (if (eq major-mode 'opal-diag-mode)
      ;; diag buffer already selected
      (if (not opal-diag-number-errors)
	  (opal-diag-parse))
    (let* ((fn (file-name-nondirectory buffer-file-name))
	   (diagfn (concat fn ".diag"))
	   (buf (get-buffer oasys-check-diagnostics-buffer)))
      (if buf
	  (progn
	    (set-buffer buf)
	    (if (not opal-diag-number-errors)
		(opal-diag-parse))
	  )
	(if opal-diag-hidden-diag
	    (setq diagfn (concat "OCS/" diagfn)))
	(if (and fn
		 (string-match ".*\\.\\(sign\\|impl\\|extp\\|intp\\)$" fn))
	    (if (file-readable-p diagfn)
		(progn
		  (if (setq buf (get-file-buffer diagfn))
		      (set-buffer buf)
		    (find-file diagfn))
		  (if (not opal-diag-number-errors)
		      (opal-diag-parse))
		  t)
	      (if noerror
		  nil
		(error (concat "No corresponding diagnostics " diagfn))))
	  (if noerror nil (error "No Opal file."))
	  )
	))))

(defun opal-diag-show-error ()
  "Make current error and corresponding source visible."
  (if (not opal-diag-number-errors)
      (opal-diag-parse))
  (if (and opal-diag-number-errors 
	   (>= opal-diag-curr-error 0) 
	   (< opal-diag-curr-error opal-diag-number-errors))
      (let* ((err (aref opal-diag-errors opal-diag-curr-error))
	     (err-mark (car err))
	     (src-mark (car (cdr (cdr err))))
	     (pop-up-windows t))
	;; show extended help, if flag set
	(switch-to-buffer (marker-buffer src-mark))
	(cond (opal-diag-extended-flag 
  	         (opal-diag-extended-show err-mark src-mark)
	     )
	)
	;; show error in source code
	(switch-to-buffer (marker-buffer src-mark))
	(goto-char src-mark)
	(let ((window (display-buffer (marker-buffer err-mark))))
	  (set-window-point window err-mark)
	  (set-window-start window err-mark))
	;; print message for certain error
	(switch-to-buffer (marker-buffer err-mark))
	(goto-char err-mark)
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
	(switch-to-buffer (marker-buffer src-mark))
	 )
    (opal-diag-delete-current-diag-buffer t)
    (error "No diagnostics."))
  )

(defconst opal-diag-parse-ocs-regexp 
  "<\\([0-9]+\\),\\([0-9]+\\)>\\(ERROR\\|WARNING\\|HINT\\)"
  "regexp to match ocs warnings")

(defconst opal-diag-parse-oasys-regexp
  "\\(ERROR\\|WARNING\\|HINT\\) \\[\\(.*\\) at \\([0-9]+\\)\\.\\([0-9]+\\)"
  "regexp to match oasys warnings")

(defconst opal-diag-parse-oasys-unknown-regexp
  "\\(ERROR\\|WARNING\\|HINT\\) \\[\\(.*\\) at unknown location"
  "regexp to match oasys diagnostics at unknown locations")

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

(defun opal-diag-parse ()
  "Parse current diagnostic buffer und setup diagnostic variables. If buffer oasys-check-diag-buffer exists, the appropriate format is selected."
  (message "Parsing diagnostics ...")
  (let*((error-list nil) 
	(oasys (get-buffer oasys-check-diagnostics-buffer))
	(curr-src (if oasys nil (opal-diag-find-source)))
	src
       )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward opal-diag-error-regexp nil t)
	(let (line colum error-mark src-mark is-suberror type limit true-error)
	  (beginning-of-line)
	  (setq error-mark (point-marker))
	  (save-excursion
	    (forward-line)
	    (setq limit (point))
	  )
	  (cond
	   ((looking-at opal-diag-parse-ocs-regexp)
	    (setq src curr-src)
	    (setq line (string-to-int (opal-diag-match 1)))
	    (setq colum (- (string-to-int (opal-diag-match 2)) 1))
	    (setq type (opal-diag-match 3))
	    (setq is-suberror nil)
	    (setq true-error t)
	    (forward-line)
	   )
	   ((looking-at opal-diag-parse-oasys-regexp)
	    (setq src (opal-diag-match 2))
	    (setq line (string-to-int (opal-diag-match 3)))
	    (setq colum (- (string-to-int (opal-diag-match 4)) 1))
	    (setq type (opal-diag-match 1))
	    (setq is-suberror nil)
	    (setq curr-src src)
	    (setq true-error t)
	    (forward-line)
	   )
	   ((looking-at opal-diag-parse-oasys-unknown-regexp)
	    (setq src (opal-diag-match 2))
	    (setq line 0)
	    (setq colum 0)
	    (setq type (opal-diag-match 1))
	    (setq is-suberror nil)
	    (setq curr-src src)
	    (setq true-error t)
	    (forward-line)
	   )
	   ((looking-at opal-diag-parse-suberror-regexp)
	    (setq src curr-src)
	    (setq line (string-to-int (opal-diag-match 1)))
	    (setq colum (- (string-to-int (opal-diag-match 2)) 1))
	    (setq type "subdiag")
	    (setq is-suberror t)
	    (setq true-error t)
	    (forward-line)
	   )
	   (t
;;	    (error "opal-diag-parse: unknown type of diagnostic: %s" (opal-current-line))
	    (setq true-error nil)
	    (forward-line)
	   )
	  )
	  (if (and true-error
		   (or (= opal-diag-show-what opal-diag-show-all)
		       (string= type "ERROR") 
		       is-suberror
		       (and (= opal-diag-show-what
			       opal-diag-show-errors-and-warns)
			    (string= type "WARNING"))
		       )
		   )
	      (progn
		(save-excursion
		  (opal-diag-set-buffer src)
		  (goto-line line)
		  (move-to-column colum)
		  (setq src-mark (point-marker)))
		(setq error-list 
		      (cons (list error-mark is-suberror src-mark) error-list)))
	    (set-marker error-mark nil)))))
    (setq opal-diag-errors (vconcat (reverse error-list)))
    (setq opal-diag-number-errors (length opal-diag-errors))
    (setq opal-diag-curr-error 0))
  (message "Parsing diagnostics done."))

(defun opal-diag-set-buffer (name)
  "set buffer name. If no such buffer exists, we try to load it via opal-find-structure. If this fails, we ask explicitly."
  (if (get-buffer name)
      (set-buffer name)
    (let ((fn (opal-find-structure name))
	 )
      (if fn
	  (set-buffer (find-file-noselect fn))
	(set-buffer (find-file-noselect
		     (read-file-name (concat "Where is Opal unit " name "? ")
				     nil nil t name)))
      )
    )
  )
)

(defun opal-diag-clear-errors ()
  "Clear markers in opal-diag-errors."
  (if opal-diag-number-errors
      (let ((i 0))
	(while (< i opal-diag-number-errors)
	  (let ((err (aref opal-diag-errors i)))
	    (set-marker (car err) nil)
	    (set-marker (car (cdr (cdr err))) nil))
	  (setq i (+ i 1)))))
;  (setq opal-diag-number-errors nil)
)
	   

(defun opal-diag-find-source (&optional diag)
  "Find source corresponding to diag buffer. This does not work for oasys diagnostics, an error is signaled."
  (if (get-buffer oasys-check-diagnostics-buffer) 
      (error "opal-diag-find-source called with oasys diagnostics!"))
  (if (not diag) (setq diag (current-buffer)))
  (let* ( (fn-diag (file-name-nondirectory (buffer-file-name diag)))
	  (fn-src  (substring fn-diag 0 (- (length fn-diag) 5))))
    (if opal-diag-hidden-diag
	(setq fn-src (concat "../" fn-src)))
    (find-file-noselect fn-src)))

(defun opal-diag-select-source (&optional diag)
  "Find and select source corresponding to diag buffer."
  (if (not diag) (setq diag (current-buffer)))
  (let* ( (fn-diag (file-name-nondirectory (buffer-file-name diag)))
	  (fn-src  (substring fn-diag 0 (- (length fn-diag) 5))))
    (if opal-diag-hidden-diag
	(setq fn-src (concat "../" fn-src)))
    (find-file fn-src)))

(defun opal-diag-read-int ()
  (let ((s nil))
    (while (looking-at "[0-9]")
      (setq s (concat s (char-to-string (following-char))))
      (forward-char 1))
    (string-to-int s)))

;; -- support for extended help

(defvar opal-diag-info-buffer "*opal-diag-information $Revision: 1.3 $*"
  "name of buffer to display extended information" )

(defun opal-diag-extended-show (errmark srcmark)
  "show information for current diagnostic" 

;  (interactive)
  (switch-to-buffer (marker-buffer errmark))
  (goto-char errmark)
  (if (get-buffer oasys-check-diagnostics-buffer)
      (setq acterror (opal-next-line))
    (setq acterror (opal-current-line))
  )
  (opal-diag-check-extended-buffer (marker-buffer errmark) 
				   (marker-buffer srcmark))
  (set-buffer opal-diag-info-buffer)
  (erase-buffer)
  (insert acterror)
  (goto-char (point-min))
  (if (re-search-forward "\\( *[0-9]+\\. \\)?\\(<[0-9]+,[0-9]+>\\)\\(ERROR: \\|WARNING: \\|HINT: \\| \\)?" nil t)
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

(defun opal-diag-check-extended-buffer 
  (opal-diag-buffer  source-buffer)
  "create buffer for diag info, if necessary; then split diagnostices window if necessary for display of this buffer"

;  (interactive)
  (if (and
       (get-buffer opal-diag-info-buffer)
       (get-buffer-window opal-diag-info-buffer)
       (get-buffer-window opal-diag-buffer)
       (get-buffer-window source-buffer))
      () ; do nothing if diag-info-buffer exists and all buffers are displayed
    (let ((bn (get-buffer-create opal-diag-info-buffer))
	  )
      (cond ;((get-buffer-window bn) (switch-to-buffer diag-buffer))
       (t 
	(if source-buffer
	    (switch-to-buffer source-buffer)
	  (switch-to-buffer (opal-diag-find-source))
	  )
	(delete-other-windows)
	(let ((dw (split-window))
	      )
	  (set-window-buffer dw opal-diag-buffer)
	  (select-window dw)
	  (let ((nw (split-window))
		)
	    (set-window-buffer nw bn)
	    (other-window 2)
	    )
	  )
	)
       )
      ) 
    )
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
	 (opal-diag-show)
	)
  )
)

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
    (opal-diag-find-diag)
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
