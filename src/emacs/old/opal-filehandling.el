; LAST EDIT: Mon Oct 30 18:20:01 1995 by Christian Maeder (andromache!maeder) 
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Opal Mode
;;; Opalfile Part
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq opal-sign ".sign")
(setq opal-impl ".impl")
(setq opal-intp ".intp")
(setq opal-extp ".extp")

(defun opal-mode-filehandling-menu-xemacs ()
  "set the opal-mode filehandling menu for XEmacs"
  (setq opal-opalfile-menu 
	(list "Opalfile"
	      ["Save all Opal files" opal-ask-save-opal-buffers t]
	      "---"
	      ["Load all parts" opal-opalfile-all t]
	      "---"
	      ["Load signature"  opal-opalfile-sign t]
	      ["Load implementation"  opal-opalfile-impl t]
;	      ["Load ext. properties" opal-opalfile-extp 
;	       :active t :included (not opal-novice)]
;             ["Load int. properties" opal-opalfile-intp 
;	       :active t :included (not opal-novice)]
	 )
   )
)

(defun opal-mode-filehandling-menu-fsfemacs ()
  "set the opal-mode filehandling menu for FSF Emacs"
  (if opal-novice
      () ; see opal-mode-set-menu'opal-mode.el for opal novices
    (define-key opal-mode-map [menu-bar opal opalfile]
      (cons "Opalfile" (make-sparse-keymap "Opalfile")))
    
;    (define-key opal-mode-map [menu-bar opal opalfile opal-opalfile-extp]
;      '("Load ext. properties" . opal-opalfile-extp))
;    (define-key opal-mode-map [menu-bar opal opalfile opal-opalfile-intp]
;      '("Load int. properties" . opal-opalfile-intp))
    ;; (define-key opal-mode-map [menu-bar opal opalfile opal-opalfile-impl]
    ;;   '("Load implementation" . opal-opalfile-impl))
    ;; (define-key opal-mode-map [menu-bar opal opalfile opal-opalfile-sign]
    ;;   '("Load signature" . opal-opalfile-sign))
    ;; (define-key opal-mode-map [menu-bar opal opalfile t1]
    ;;   '("" . nil))
    ;; (define-key opal-mode-map [menu-bar opal opalfile opal-opalfile-all]
    ;;   '("Load all parts" . opal-opalfile-all))
    ;; (define-key opal-mode-map [menu-bar opal opalfile t0]
    ;;   '("" . nil))
    (define-key opal-mode-map [menu-bar opal opalfile opal-opalfile-ask]
      '("Save all Opal files" . opal-ask-save-opal-buffers))
  )
)

(defun opal-mode-filehandling-keymap ()
  "Set the opal-mode Opal file keymap."
;; Tastenbelegungen von Compile
;;shortkeys
;  (define-key opal-mode-map "\M-e" 'opal-opalfile-error)
;;longkeys
  (define-key opal-mode-map "\C-c\C-le" 'opal-opalfile-error)
  (define-key opal-mode-map "\C-c\C-la" 'opal-opalfile-all)
  (define-key opal-mode-map "\C-c\C-ls" 'opal-opalfile-sign)
  (define-key opal-mode-map "\C-c\C-li" 'opal-opalfile-impl)
  (define-key opal-mode-map "\C-c\C-lp" 'opal-opalfile-intp)
  (define-key opal-mode-map "\C-c\C-lx" 'opal-opalfile-extp)
  (define-key opal-mode-map "\C-c\C-lm" 'opal-make-main-moduls)
  (define-key opal-mode-map "\C-c\C-x\C-s" 'opal-ask-save-opal-buffers)
;  (define-key opal-mode-map [down-mouse-1 down-mouse-3] 'opal-opalfile-all)

;; Menuepunkt Opalfile:
  (if opal-running-xemacs
      (opal-mode-filehandling-menu-xemacs)
      (opal-mode-filehandling-menu-fsfemacs)
  )
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'opal-opalfile-error 'menu-enable 
     '(and (not (opal-compile-running)) (opal-compile-delete-frame-enable)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opal-opalfile-all ()
  "Load the implementation, signature, ext. properties and int. properties of the pointed structure."
  (interactive)
  (let ((opal-filename (opal-get-structure-name "load structure:")))
    (setq opal-file-found nil)
    (if (not opal-use-frames) (delete-other-windows))
    (opal-opalfile opal-impl opal-filename)
    (opal-opalfile opal-sign opal-filename)
    (opal-opalfile opal-intp opal-filename)
    (opal-opalfile opal-extp opal-filename)
    (opal-file-found opal-filename)
    )
  )

(defun opal-file-found (opal-filename)
  (if (not opal-file-found)
      (princ (concat "No structure " (concat opal-filename " found")))))

(defun opal-opalfile (opal-ext &optional opal-filename)
  (let ((fn (if opal-filename ; Test auf opt. Parameter
	       opal-filename 
	     (opal-get-structure-name (opal-file-prompt opal-ext)))))
    (opal-load-structure (opal-load-path) (concat fn opal-ext) nil opal-use-frames)
    fn))

(defun opal-file-prompt (ext)
  "return prompt for asking for ext"
  (cond
   ((string= ext opal-sign) "load signature of:")
   ((string= ext opal-impl) "load implementation of:")
   ((string= ext opal-extp) "load external properties of:")
   ((string= ext opal-intp) "load internal properties of:")
  )
)

(defun opal-mk-alt (alt1 alt2 &optional alt3)
  (let ((ft (concat "\\(" (concat alt1 (concat "\\|" alt2)))))
    (concat
     (if alt3
	 (concat ft (concat "\\|" alt3))
       ft)
     "\\)")))

(defun opal-opalfile-error ()
  "Load the error file and the corresponding diagnostic file."
  (interactive)
  (cond 
   ((get-process "compiling") (princ "Process is running"))
   (t
 ; Test, ob compile-Fenster existiert
    (let ((akt-buffer (current-buffer))
	  (comp-buffer (get-buffer "*opal-compile*"))
	  (op-error "^<[0-9]+,[0-9]+>ERROR:")
	  (c-error "^./\\([^ ]+\\.hc\\.[hc]\\):[0-9]+:"))
      (cond
       ((not comp-buffer) (princ "no compile buffer"))
       (t 
; ins compile fenster wechseln
	(set-buffer "*opal-compile*")
; Cursor platzieren
	(goto-char (point-max))
; auf Error testen
	(cond 
	 ((not (re-search-backward (opal-mk-alt op-error c-error) nil t))
	  (message "No errors found."))
	 (t ;(princ "Errors found.")
; von hinten Filenamen suchen
	  (cond 
	   ((re-search-forward c-error nil t)
	    (setq opal-error-filename 
		  (buffer-substring (match-beginning 1) (match-end 1)))
;	    (error (concat opal-error-filename "c-error scanned."))
	    )
	   ((re-search-backward
	     "^\\(Compiling\\|Checking\\) \\(Implementation\\|Signature\\|External Properties\\|Internal Properties\\) of \\([^ ]+\\) \\.\\.\\.$" nil t)
	    (setq opal-error-filename
		  (concat 
		   (buffer-substring;StructureName
		    (match-beginning 3)
		    (match-end 3))
		   (let ((look (buffer-substring
				(match-beginning 2)
				(match-end 2))))
		     (cond ((string-match "Ext" look) opal-extp)
			   ((string-match "Imp" look) opal-impl)
			   ((string-match "Sig" look) opal-sign)
			   ((string-match "Int" look) opal-intp)
			   )))))
	   (t (error "No opal- or c-error scanned.")))
; Pfad suchen
	  (save-excursion
	    (setq opal-subsystem-ex 
		  (re-search-backward
		   "^\\(Archiving [^ ]* \\.\\.\\.$\\|gmake[^ ]*:\\)" nil t))
	    (setq opal-subsystem-pos (point))
	    )
	  (cond ((not (re-search-backward 
		       "^>>>> Visiting [^ ]* '[^ ]*' .* <<<<" nil t))
		 (setq opal-error-path "."))
		((and opal-subsystem-ex (< (point) opal-subsystem-pos))
		 (setq opal-error-path "."))
		(t (re-search-forward "'")
		   (setq tmp-beg (point))
		   (re-search-forward "'")
		   (setq tmp-end (point))
		   (setq opal-error-path
			 (buffer-substring tmp-beg (- tmp-end 1)))))
;	  (print (concat opal-error-path "/" opal-error-filename) (get-buffer "*scratch*"))
; File laden
	  (if opal-use-frames
	      (set-buffer akt-buffer)
	    (switch-to-buffer akt-buffer)
	    (delete-other-windows))
	  (setq opal-file-found nil)
	  (opal-load-structure (list opal-error-path) 
			       opal-error-filename t)
; diag-mode aktivieren
	  (if opal-file-found
	      (cond
	       ((string-match ".hc.[ch]$" opal-error-filename) t)
	       (opal-use-frames 
		(delete-other-windows)
		(split-window nil (* (/ (window-height) 4) 3))
		(diag-update)) ; nur fuer opal files aufrufen
	       (t (diag-update))
	       ) 
	    (princ (concat 
		    "Structure " opal-error-path "/" opal-error-filename
		    " not found (compile started from other directory?)"))
	    )
	  ))))))
   ))
   
   
(defun opal-opalfile-impl ()
  "Load the implementation of the pointed structure."
  (interactive)
  (setq opal-file-found nil)
  (opal-file-found (concat (opal-opalfile opal-impl) opal-impl))
  )

(defun opal-opalfile-intp ()
  "Load the internal properties of the pointed structure."
  (interactive)
  (setq opal-file-found nil)
  (opal-file-found (concat (opal-opalfile opal-intp) opal-intp))
  )


(defun opal-opalfile-extp ()
  "Load the external properties of the pointed structure."
  (interactive)
  (setq opal-file-found nil)
  (opal-file-found (concat (opal-opalfile opal-extp) opal-extp))
  )


(defun opal-opalfile-sign ()
  "Load the signature of the pointed structure."
  (interactive)
  (setq opal-file-found nil)
  (opal-file-found (concat (opal-opalfile opal-sign) opal-sign))
  )

(defvar opal-hook-path (list)
  "")


(defun opal-load-path-print ()
  "Suchpfad der Module setzen. Die Variable opal-hook-pfad kann verwendet werden, um den Suchpfad zu erweitern. (setq opal-hook-path (list \"./bla\" \"./foo\" ))"
  (interactive)
  (princ (opal-load-path) (get-buffer "*scratch*"))
  )

(defun opal-load-path ()
  "Suchpfad der Module setzen. Die Variable opal-hook-pfad kann verwendet werden, um den Suchpfad zu erweitern. (setq opal-hook-path (list \"./bla\" \"./foo\" ))"
  (interactive)
  (append 
   (list ".")
   opal-hook-path
   (list (concat opal-path "/signs/BasicTypes")
	 (concat opal-path "/signs/Internal/Strange")
	 (concat opal-path "/signs/Functions/General")
	 (concat opal-path "/signs/Functions/Orderings")
	 (concat opal-path "/signs/Functions/Special")
	 (concat opal-path "/signs/AggregateTypes/ProductLike")
	 (concat opal-path "/signs/AggregateTypes/UnionLike")
	 (concat opal-path "/signs/AggregateTypes/SeqLike")
	 (concat opal-path "/signs/AggregateTypes/TreeLike")
	 (concat opal-path "/signs/AggregateTypes/SetLike")
	 (concat opal-path "/signs/AggregateTypes/MapLike")
	 (concat opal-path "/signs/AggregateTypes/Algorithms")
	 (concat opal-path "/signs/System/Debugging")
	 (concat opal-path "/signs/System/Commands")
	 (concat opal-path "/signs/System/Unix")
	 (concat opal-path "/signs/System/Streams")
	 (concat opal-path "/signs/Tools/Formatting")
	 (concat opal-path "/signs/Tools/OpalWin")
	 (concat opal-path "/signs/Tools/OpalWinAdditions")
	 (concat opal-path "/signs/Tools/ParserLight")
	 (concat opal-path "/signs/Tools/Readline")
	 (concat opal-path "/signs/Tools/Tcl")
	 (concat opal-path "/signs/Tools/Tk")
	 )
   )
  )

(defun opal-find-structure (opal-file-name)
  "return full name of opal-file-name in the opal-load-path or nil"
  (interactive "sFilename:")
  (let (a res fn)
    (setq a (opal-load-path))
    (setq res nil)
    (while (and a (not res))
      (setq fn (concat (car a) "/" opal-file-name))
      (if (file-exists-p fn)
	  (setq res fn))
      (setq a (cdr a))
      )
    res
    )
)

(defun opal-add-load-path (path)
  "add path to opal-load-path, if not already contained. (Trailing slash is removed)"
  (interactive "sPath:")
  (if (string-match "\\(.*\\)/$" path)
      (setq path (substring path (match-beginning 1) (match-end 1)))
    )
  (if (not (member path opal-hook-path))
      (setq opal-hook-path (cons path opal-hook-path))
  )
)

(defun opal-load-structure (list opal-filename switch-new-buffer &optional save-window )
  ""
  (setq load-file-fkt
	'(lambda () 
	   (let ((opal-file (concat (car list) (concat "/" opal-filename))))
	     (cond ((file-exists-p opal-file)
		    (let ((erg (opal-get-frame-p(frame-list) opal-filename)))
		      (cond ((car erg)
			     (make-frame-visible (cdr erg))
			     (raise-frame(cdr erg))
			     (select-frame(cdr erg)))
			    (opal-use-frames 
			     (select-frame(new-frame))
			     (find-file opal-file))
			    (switch-new-buffer
			     (split-window nil (* (/ (window-height) 3) 2))
			     (find-file opal-file))
			    (t (let ((size (* (/ (window-height) 5) 4)))
				 (split-window
				  nil 
				  (if (< (- (window-height) size) 4) 
				      (- (window-height) 4) 
				    size))
				 (other-window 1)
				 (find-file opal-file)
				 (other-window -1))
			       )))
		    (setq  opal-file-found t))
		   ))))
  (while (not (null list))
    (cond (save-window
	   (save-window-excursion
	     (funcall load-file-fkt))
	   )
	  (t (funcall load-file-fkt))
	  )
    (setq list (cdr list))
    )
  )



(defun opal-get-structure-name (&optional prompt)
  "Return pointed opal-identifier."
  (interactive)
  (save-excursion
    (let ((opal-ide-begin (progn (begin-opal-ide)(point)))
	  (opal-ide-end (progn (end-opal-ide)(point)))
	  a)
      (setq a 
	    (if (> opal-ide-end (point-max))
		"bla"
	      (buffer-substring opal-ide-begin opal-ide-end))
      )
      (if prompt
	  (read-string prompt a)
	a
      )
    )
  )
)

(defun opal-make-moduls (&optional name)
  "Generate SIGNATURE and IMPLEMENTATION files of arg."
  (interactive)
  (if name
      (error "nyi.")
    (let ((opal-modul-name (capitalize (opal-get-structure-name))))
      (let ((opal-file-sign (concat opal-modul-name ".sign"))
	    (opal-file-impl (concat opal-modul-name ".impl")))
	(if (file-exists-p opal-file-impl)
	    (find-file opal-file-impl)
	  (find-file opal-file-impl)
	  (princ (concat "IMPLEMENTATION " opal-modul-name)(current-buffer))
	  (save-buffer)
	  )
	(delete-other-windows)
	(split-window-vertically)
	(if (file-exists-p opal-file-sign)
	    (find-file opal-file-sign)
	  (find-file opal-file-sign)
	  (princ (concat "SIGNATURE " opal-modul-name)(current-buffer))
	  (save-buffer)
	  )
	))
    ))

(defun opal-make-main-moduls (&optional name)
  "Generate SIGNATURE and IMPLEMENTATION files of arg."
  (interactive)
  (if name
      (error "nyi.")
    (let ((opal-modul-name (capitalize (opal-get-structure-name))))
      (let ((opal-file-sign (concat opal-modul-name ".sign"))
	    (opal-file-impl (concat opal-modul-name ".impl")))
	(if (file-exists-p opal-file-impl)
	    (find-file opal-file-impl)
	  (find-file opal-file-impl)
	  (princ (concat "IMPLEMENTATION " opal-modul-name)(current-buffer))
	  (newline)
	  (newline)
	  (princ (concat "FUN " (downcase opal-modul-name) ": com[void] ")(current-buffer))
	  (newline)
	  (princ (concat "DEF " (downcase opal-modul-name) " == ")(current-buffer))
	  (newline)
	  (save-buffer)
	  )
	(delete-other-windows)
	(split-window-vertically)
	(if (file-exists-p opal-file-sign)
	    (find-file opal-file-sign)
	  (find-file opal-file-sign)
	  (princ (concat "SIGNATURE " opal-modul-name)(current-buffer))
	  (newline)
	  (newline)
	  (princ "IMPORT Com[void]   ONLY com" (current-buffer))
	  (newline)
	  (princ "       Void        ONLY void" (current-buffer))
	  (newline)
	  (newline)
	  (princ (concat "FUN " (downcase opal-modul-name) ": com[void] ")(current-buffer))
	  (newline)
	  (save-buffer)
	  )
	))
    ))

(provide 'opal-filehandling)
