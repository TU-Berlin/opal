;; emacs mode for highlighting (and moving) in trace files

(provide 'opal-trace-mode)

(setq auto-mode-alist (cons '("\\.trace$" . opal-trace-mode) auto-mode-alist))

(defun opal-trace-mode-keymap ()
  "set keymap for opal-trace-mode"
  (interactive)

  (setq opal-trace-mode-map (make-sparse-keymap))
  (define-key opal-trace-mode-map "\C-n" 'opal-trace-next-proofstate)
  (define-key opal-trace-mode-map "\C-p" 'opal-trace-previous-proofstate)
  (define-key opal-trace-mode-map "\M-n" 'opal-trace-next-trace)
  (define-key opal-trace-mode-map "\M-p" 'opal-trace-previous-trace)
  (define-key opal-trace-mode-map "\C-f" 'opal-trace-next-subgoal)
  (define-key opal-trace-mode-map "\C-b" 'opal-trace-previous-subgoal)
  (define-key opal-trace-mode-map "\C-c\C-l" 'opal-trace-reload)
  (define-key opal-trace-mode-map "\M-u" 'opal-trace-reload)
  (define-key opal-trace-mode-map "\C-c\C-r" 'opal-trace-find-source)
  (define-key opal-trace-mode-map "\C-c\C-f" 'opal-trace-fontify-proofstate)
  (if running-xemacs
      (progn
	(define-key opal-trace-mode-map [(alt ?t)] 'opal-trace-reload)
	)
    )
)


(defun opal-trace-mode-set-menu ()
  "set up menu for opal trace mode"

  (interactive)
  (if opal-running-xemacs
      (progn
	 (set-buffer-menubar (copy-sequence current-menubar))
	 (add-submenu nil  
		      (list "Trace"
			    ["next trace" opal-trace-next-trace t]
			    ["next proofstate" opal-trace-next-proofstate t]
			    ["next subgoal" opal-trace-next-subgoal t]
			    "-----"
			    ["previous trace" opal-trace-previous-trace t]
			    ["previous proofstate" 
			     'opal-trace-previous-proofstate t]
			    ["previous subgoal" opal-trace-previous-subgoal t]
			    "-----"
			    ["fontify current proofstate"
			     opal-trace-fontify-proofstate t]
			    "-----"
			    ["reread trace" opal-trace-reload t]
			    ["back to Opal source" opal-trace-find-source t]
			    ))
	 )
    )
  )



	

(defun opal-trace-mode ()
  "major mode for visiting trace files of the proof checker"
  (interactive)
  
  (kill-all-local-variables)
  (setq major-mode 'opal-trace-mode)
  (setq mode-name "Opal prover trace")
  (opal-trace-mode-keymap)
  (use-local-map opal-trace-mode-map)         ; This provides the local keymap
  (setq buffer-read-only t)
  (opal-trace-mode-set-menu)
  (lazy-lock-mode 1)

;  (run-hooks 'opal-trace-mode-hook)
)

;; colors for trace-mode
(defconst opal-trace-font-lock-keywords
  (list
      ; '("\\<SORT\\>\\|\\<ALL\\>\\|\\<AND\\>\\|\\<ANDIF\\>\\|\\<AS\\>\\|\\<COMPLETELY\\>\\|\\<DFD\\>\\|\\<ELSE\\>\\|\\<EX\\>\\|\\<FI\\>\\|\\<IF\\>\\|\\<IN\\>\\|\\<LET\\>\\|\\<NOT\\>\\|\\<ONLY\\>\\|\\<ORIF\\>\\|\\<OR\\>\\|\\<OTHERWISE\\>\\|\\<THEN\\>\\|\\<WHERE\\>\\|\\*\\*\\|->\\|\\<\\.\\>\\|\\<:\\>\\|\\<_\\>\\|===\\|<<=\\|==>\\|<=>\\|==\\|\\\\\\\\\\>" (0 'font-lock-function-name-face nil t))
   '("^Proofstate.*$" (0 'underline t t))
   '("^final proofstate.*$" (0 'underline t t))
   '("^end of proofstate.*$" (0 'underline t t))
   '("\\<Subgoal\\>\\|\\<hypotheses\\>" (0 'modeline-buffer-id t t))
   '("^Trace of .*" (0 'modeline-mousable t t))
;   '("^Trace of \\(.*\\)" (1 'font-lock-comment-face t t))
   '("[a-zA-Z]+[¡¿]" (0 'italic t t))
   '("§[0-9]+" (0 'font-lock-reference-face t t))
   '("§[0-9]+ [^¤§]*¤" (0 'secondary-selection t t))   ;; new formula
   '("§[0-9]+ [^ø§]*ø" (0 'font-lock-string-face t t)) ;; original formula
   '("subgoal #.*$" (0 'italic t t)) 
   '("finished subgoal, 0.*$" (0 'italic t t))
   '("finished subgoal, [1-9].*$" (0 'font-lock-comment-face t t))   
   '("FAILURE PATH" (0 'font-lock-comment-face t t))   
   '("bt #[0-9].*$" (0 'font-lock-comment-face t t))   
   )
  )


(put 'opal-trace-mode 'font-lock-defaults 
       '(opal-trace-font-lock-keywords nil nil nil 'beginning-of-line)
)

(defun opal-trace-next-proofstate ()
  "find next proofstate"
  (interactive)

  (if (looking-at "^ *\\(Proofstate\\|final proofstate\\)")
      (forward-line 1)
    )
  (search-forward-regexp "^ *end of proofstate")
  (search-forward-regexp "^ *\\(Proofstate\\|final proofstate\\)")
  (beginning-of-line)
  (opal-trace-back-nonempty)
)

(defun opal-trace-previous-proofstate ()
  "find next proofstate"
  (interactive)

  (search-backward-regexp "^ *\\(Proofstate\\|final proofstate\\)")
  (beginning-of-line)
  (opal-trace-back-nonempty)
)

(defun opal-trace-back-nonempty ()
  "skip back nonempty lines"

  (beginning-of-line)
  (set-window-start (selected-window) (point))
  (let (a)
    (save-excursion
      (while (not (looking-at "^ *$"))
	(forward-line -1)
	)
      (forward-line 1)
      (setq a (point))
      )
   (set-window-start (selected-window) a)
   )
  )

(defun opal-trace-next-trace ()
  "find next trace"
  (interactive)

  (if (looking-at "^ *Trace")
      (forward-line 1)
    )
  (search-forward-regexp "^ *Trace")
  (beginning-of-line)
  (set-window-start (selected-window) (point))
)

(defun opal-trace-previous-trace ()
  "find next trace"
  (interactive)

  (search-backward-regexp "^ *Trace")
  (beginning-of-line)
  (set-window-start (selected-window) (point))
)

(defun opal-trace-bold-line ()
  "make current line appear in bold face"

  (interactive)
  (save-excursion
    (let (a b)
      (beginning-of-line)
      (setq a (point))
      (end-of-line)
      (setq b (point))
      (insert-face (opal-current-line) 'highlight)
      )
    )
  )

(defun opal-trace-fontify-proofstate ()
  "fontify current proofstate"

  (interactive)
  (save-excursion
    (let (a o)
      (if (search-backward-regexp "^\\(Proofstate\\|final proofstate\\)" nil t)
	  (progn 
	    (beginning-of-line)
	    (setq a (point))
	    (if (search-forward-regexp "^end of proofstate" nil t)
		(progn
		  (end-of-line)
		  (setq o (point))
		  (font-lock-fontify-region a o)
		  )
	      )
	    )
	)
      )
    )
  )
      
(defun opal-trace-reload ()
  "reload proof trace"

  (interactive)
  (let ((pstate (opal-trace-current-proofstate))
	(ptrace (opal-trace-current-trace)))
  (revert-buffer t t t)
  (beginning-of-buffer)
  (if (search-forward ptrace nil t)
      (if (search-forward pstate nil t)
	  (progn
	    (beginning-of-line)
	    (opal-trace-back-nonempty)
	    )
	(message "Could not find %s" pstate)
	)
    (message "Could not find %s" ptrace)
    )
  )
  )

(defun opal-trace-current-proofstate ()
  "return current place"

  (save-excursion
    (if (looking-at "^\\Proofstate")
	(opal-current-line)
      (search-backward-regexp "^\\Proofstate")
      (opal-current-line)
      )
    )
  )

(defun opal-trace-current-trace ()
  "return current trace"
  (save-excursion
    (if (looking-at "^\\Trace")
	(opal-current-line)
      (search-backward-regexp "^\\Trace")
      (opal-current-line)
      )
    )
  )

(defun opal-trace-location ()
  "show current location in proof trace"
  (interactive)
  (message "%s" (concat (opal-trace-current-proofstate)
			(opal-trace-current-trace)))
)

(defun opal-trace-next-subgoal ()
  "find next subgoal"

  (interactive)
  (search-forward-regexp "^ *Subgoal")
  (beginning-of-line)
  (set-window-start (selected-window) (point))
  (forward-line 1)
)

(defun opal-trace-previous-subgoal ()
  "find previous subgoal"

  (interactive) 
  (search-backward-regexp "^ *Subgoal" )
  (beginning-of-line)
  (set-window-start (selected-window) (point))
  (forward-line 1)
)

(defun opal-trace-find-source ()
  "select window of associated opal source"

  (interactive)
  (if (string-match "\\(.*\\)\\.trace" (buffer-name))
      (progn
	(select-window (display-buffer
			(substring (buffer-name) 
				   (match-beginning 1) (match-end 1))))
	(delete-other-windows)
	)
    (error "%s" "cannot determine associated Opal source")
    )
)

;;; Pchecker-related
(defun opal-pcheck-trace ()
  "find associated trace buffer"

  (interactive)
  (let (a b)
    (setq a (buffer-file-name))
    (if (string-match "\\(.*/\\)\\(.*\\.\\(sign\\|impl\\)\\)" a)
	(progn
	  (setq b 
		(concat (substring a (match-beginning 1) (match-end 1))
			"OCS/"
			(substring a (match-beginning 2) (match-end 2))
			".trace"))
	  (if (file-readable-p b)
	      (progn
		(setq f (get-file-buffer b))
		(if f (progn (set-buffer f) (revert-buffer t t)))
		(find-file-other-window b)
		)
	    (error "File `%s' does not exist or is not readable" b)
	    )))
    )
  )

(defun opal-trace-interactive-init ( path )
  "load .trace file in given path"

  (interactive)

  (let (fn b pop-up-frames font-lock-maximum-size)
    (setq pop-up-frames t)
    (setq fn (concat path "/.trace"))
    (if (not (file-readable-p fn))
	(progn
	  (setq b (get-buffer-create ".trace"))
	  (pop-to-buffer b)
	  (insert "File " fn " does not exist or is no readable")
	  )
      (setq b (find-file-noselect fn t))
      (pop-to-buffer b)
      (revert-buffer t t t)
      (goto-char (point-max))
      (opal-trace-previous-proofstate)
      (opal-trace-fontify-proofstate)
      )
    (display-buffer b)
    (raise-frame (car (frames-of-buffer b)))
    )
  )