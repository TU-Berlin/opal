;; outline-support for Opal sources

;; this borrows some definitions from the original outline mode

(provide 'opal-outline)

(defconst opal-outline-extent-keymap nil)

(defun opal-outline-init ()
  "initialize Opal outline mode. "
  (interactive)
  (define-key opal-mode-map "\C-c\C-h\C-h" 'opal-outline-hide)
  (define-key opal-mode-map "\C-c\C-h\C-s" 'opal-outline-show)
  (define-key opal-mode-map "\C-c\C-hs" 'opal-outline-show-all)
  (define-key opal-mode-map "\C-c\C-hh" 'opal-outline-hide-all)
  (define-key opal-mode-map "\C-c\C-h\C-k" 'opal-outline-scan-all)

  (setq opal-outline-extent-keymap (make-sparse-keymap))
  (define-key opal-outline-extent-keymap [(button1)]
    'opal-outline-toggle-mouse)
  (define-key opal-outline-extent-keymap [(return)]
    'opal-outline-toggle)
  (make-face 'opal-outline-extent-button-face)
  (set-face-background 'opal-outline-extent-button-face "lightsalmon")
  (make-face 'opal-outline-extent-button-face-hidden)
  (set-face-background 'opal-outline-extent-button-face-hidden
		       (face-property 'default 'background))
  (set-face-foreground 
   'opal-outline-extent-button-face-hidden
   (face-property 'font-lock-function-name-face 'foreground))
)

(defun opal-outline-hook-functions ()
  (interactive)
  (opal-outline-scan-all)
  )


(defconst opal-outline-begin-regexp   
  "^\\(DEF\\|PROOF.*==\\|IMPORT\\|DATA\\|TYPE\\|PROP\\|JSTF\\)"
  "regexp to mark beginning of regions which may be folded")

(defconst opal-outline-end-regexp 
  "^\\(DEF\\|FUN\\|LAW\\|TYPE\\|DATA\\|PROOF\\|PROP\\|JSTF\\|[ \t]*$\\)"
  "regexp to mark the first line after the end of region to be folded")

(defvar opal-outline-menu 
  (list "Outline"
	["Fold this paragraph" opal-outline-hide t]
	["Unfold this paragraph" opal-outline-show t]
	["Toggle folding" opal-outline-toggle t]
	"---"
	["Fold everything" opal-outline-hide-all t]
	["Unfold everything" opal-outline-show-all t]
	["Scan for keywords" opal-outline-scan-all t]
	))


(defun opal-outline-hide-or-show-region (from to flag)
  "hide region if flag is t, show region if flag is nil"
  (let (buffer-read-only)
    (if flag
	(subst-char-in-region from to ?\n ?\^M t)
	(subst-char-in-region from to ?\^M ?\n t)
	)
    )
  )

(defun opal-outline-on-heading-p ()
  "t, iff point is on a heading line"
  (save-excursion
    (beginning-of-line)
    (and (bolp)
	 (looking-at opal-outline-begin-regexp)))
  )

(defun opal-outline-back-to-heading ()
  "move to previous begin"
  (beginning-of-line)
  (or (opal-outline-on-heading-p)
      (re-search-backward opal-outline-begin-regexp nil t)
			  (error "before first heading"))
  )

(defun opal-outline-end-body (&optional quiet)
  "move to beginning of first line after body"
  (end-of-line)
  (if (re-search-forward opal-outline-end-regexp nil t)
      (progn
	(beginning-of-line)
	(backward-char) 
	t
	)
    (if quiet
	nil
      (error "no end found")
      )
    )
  )


(defun opal-outline-hide-or-show (hide &optional quiet)
  "hide or show (arg nil)body of current (or previous) 
definition or proof body"
 (interactive) 
 (save-excursion
   (let (start end)
     (opal-outline-back-to-heading)
     (setq start (point))
     (if (not (opal-outline-end-body quiet))
	 t
       (setq end (point))
       (beginning-of-line)
       (if (and hide (eq start (point)))
	   (progn
	     (if (not quiet)
		 (message "%s" "no need to hide a single line")
	       )
	     )
	 (opal-outline-hide-or-show-region start end hide)
	 (goto-char start)
	 (opal-outline-extent hide)
	 t
	 )
       )
     )
   )
 )

(defun opal-outline-hidden ()
  "t, iff current def/body is hidden"
  (interactive)
  
  (save-excursion
    (let (result end)
      (opal-outline-back-to-heading)
      (forward-line)
      (setq end (point))
      (forward-line -1)
      (search-forward "\C-M" end t)
      )
    )
  )
    
(defun opal-outline-hide (&optional quiet)
  (interactive)
  (opal-outline-hide-or-show t quiet)
  )

(defun opal-outline-show (&optional quiet)
  (interactive)
  (opal-outline-hide-or-show nil quiet)
  )

(defun opal-outline-toggle ()
  (interactive)
  (if (opal-outline-hidden)
      (opal-outline-show)
    (opal-outline-hide)
    )
  )

(defun opal-outline-toggle-mouse (event)
  (interactive "e")
  (mouse-set-point event)
  (opal-outline-toggle)
  )

(defun opal-outline-extent (hide)
  "called with point at beginning of hideable region. 
   sets extent accordingly"

  (let (start end r)
    (setq start (point))
    (setq end (search-forward-regexp "[A-Z]+" (+ start 10) t))
    (if (not end)
	(error "?? no keyword at beginning of line")
      )
    (setq r (make-extent start end))
    (set-extent-keymap r opal-outline-extent-keymap)
    (if hide
	(set-extent-face r 'opal-outline-extent-button-face)
      (set-extent-face r 'opal-outline-extent-button-face-hidden)
      )
    (set-extent-mouse-face r 'highlight)
    )
  )

(defun opal-outline-hide-all ()
  (interactive)

  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward opal-outline-begin-regexp nil t)
      (beginning-of-line)
      (opal-outline-hide t)
      (forward-line)
      )
    )
)

(defun opal-outline-show-all ()
  (interactive)

  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward opal-outline-begin-regexp nil t)
      (beginning-of-line)
      (opal-outline-show t)
      (forward-line)
      )
    )
)

(defun opal-outline-scan-all ()
  (interactive)
  
  (save-excursion
    (goto-char (point-min))
    (let (start exit)
      (setq exit nil)
      (while (or exit (re-search-forward opal-outline-begin-regexp nil t))
	(beginning-of-line)
	(setq start (point))
	(if (opal-outline-end-body t)
	    (progn
	      (beginning-of-line)
	      (if (not (eq (point) start))
		  (progn
		    (goto-char start)
		    (opal-outline-extent nil)
		    )
		)
	      (setq exit (eq (forward-line) 1))
	      )
	  )
	)
      )
    (if (fboundp 'opal-certify-set-extent)
	(progn
	  (goto-char (point-min))
	  (while (opal-certify-set-extent)
	    nil
	    )
	  )
      )
    )
  )

