;; outline-support for Opal sources

;; this borrows some definitions from the original outline mode
;; since we cannot use the outline mode unchanged, we copy the
;; necessary functions here


(defun opal-outline-init ()
  "initialize Opal outline mode. this function is hooked to the Opal mode."
  (setq selective-display t)
)

(add-hook 'opal-mode-hook 'opal-outline-init)

(defconst opal-outline-begin-regexp   "^\\(DEF\\|PROOF.*==\\)"
  "regexp to mark beginning of regions which may be folded")

(defconst opal-outline-end-regexp 
  "^\\(DEF\\|FUN\\|LAW\\|TYPE\\|DATA\\|PROOF\\|[ \t]*$\\)"
  "regexp to mark the first line after the end of region to be folded")


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

(defun opal-outline-end-body ()
  "move to beginning of first line after body"
  (end-of-line)
  (or (re-search-forward opal-outline-end-regexp nil t)
      (error "no end found"))
  (beginning-of-line)
  (backward-char)
)


(defun opal-outline-hide-or-show (hide)
  "hide or show (arg nil)body of current (or previous) 
definition or proof body"
  (interactive)
  (save-excursion
    (let (start end)
      (opal-outline-back-to-heading)
      (setq start (point))
      (opal-outline-end-body)
      (setq end (point))
      (opal-outline-hide-or-show-region start end hide)
      )
    )
  )

(defun opal-outline-hidden ()
  "t, iff current def/body is hidden"
  )
    
(defun opal-outline-hide ()
  (interactive)
  (opal-outline-hide-or-show t)
  )

(defun opal-outline-show ()
  (interactive)
  (opal-outline-hide-or-show nil)
  )