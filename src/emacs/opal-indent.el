;;; indentation for opal

(provide 'opal-indent)

;; $_Header$

(defun opal-mode-indent ()
  "indent current line according to opal-mode.
Return the amount the indentation changed by."
  (interactive)

;; copied in parts from cperl-mode

  (let (indent
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (setq indent (opal-calculate-indent))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt
    )
  )

(defconst opal-indent-major-keyword
  "^[ \t]*\\(SIGNATURE\\|IMPLEMENTATION\\|IMPORT\\|FUN\\|DEF\\|PROOF\\|TYPE\\|DATA\\|SORT\\|LAW\\|EXTERNAL\\|INTERNAL\\|/\\*\\|--\\|LEMMA\\)[ \n]"
  "*regexp to match keywords with constant indentation 0")

(defconst opal-indent-indent-keyword
  "^[ \t]*\\(JUSTF\\|DATA\\|DEF\\|PROOF\\|LET\\|IF\\|ELSE\\|LAW\\|WHERE\\|TYPE\\)[ \t\n]"
  "*regexp to match lines which increase indentation in reference line")

(defconst opal-indent-outdent-keyword
  "^[ \t]*\\(FI\\|ELSE\\|OTHERWISE\\)"
  "*regexp to match lines whose indentation is to be decreased wrp to reference line")

(defconst opal-indent-monadic  "([ \t]*\\\\[ \t]*.*\\.[ \t]*$"
  "*regexp to match open parentheses which should not be regarded (for example in ' ...  & (\\\\ b.'")

(defconst opal-indent-width 2
  "*indentation for opal-indent functions")


(defun opal-calculate-indent ()
  "calculate indentation of current line for opal"

  (interactive)
  (let (cpos tmppos indent paren) 
    (save-excursion
      (beginning-of-line)
      (setq cpos (point))
;; keine Einrueckung fuer major keywords
      (cond ((looking-at opal-indent-major-keyword)
	     0  ; ==> return 0 for these keywords
	     )
;; Zeile beginnt mit schliessenden Klammern
	    ((looking-at "^[ \t]*\\()+\\)")
	     (goto-char (match-end 1))
	     (backward-sexp)
	     ;; Sonderbehandlung fuer monadische Ausdruecke
	     (cond ((looking-at opal-indent-monadic)
		    (setq tmppos (opal-find-last-open-paren nil))
		    (if tmppos
			tmppos
		      (setq indent (current-indentation))
		      (beginning-of-line)
		      (opal-calculate-indent-def cpos indent)		   
		      ))
	       ;; Normalbehandlung sonst - passende oeffnende Klammer
		   (t
		    (current-column))
	     ))
;; Zeile beginnt mit IN => passendes LET suchen
	    ((looking-at "^[ \t]*IN")
	     (opal-indent-matching-let 0)
	     )
;; Zeile mit FI/ELSE/OTHERWISE => passendes IF suchen
	    ((looking-at opal-indent-outdent-keyword)
	     (opal-indent-matching-if 0)
	     )
;; Zeile beginnt mit IF => passendes IF suchen
	    ((and (looking-at "^[ \t]*IF")
		  (save-excursion (opal-indent-inner-if 0)))
	     (opal-indent-matching-if 0))
;; weder major keyword noch schliessende Klammer	    
	    (t
	     (opal-end-of-reference-line)
	     (setq tmppos (opal-find-last-open-paren nil))
	     (if tmppos
		 tmppos
;		 (+ (current-indentation) opal-indent-width)
	       (setq indent (current-indentation))
	       (beginning-of-line)
	       (opal-calculate-indent-def cpos indent)
	       )
	     )
	    )
    )
  )
)

(defun opal-calculate-indent-def (limit indent)
  "calculate indent for opal DEF, up to limit, current indent indent"
  (interactive)

  (let (newindent letindent)
    (setq letindent nil)
    (cond ((looking-at "^\\(FUN[ \t]*.*:\\).*\\(\\*\\*\\|->\\)[ \t]*$")
	   (setq newindent (+ opal-indent-width (opal-match-len 1))))
	  ((looking-at "^\\(PROOF[ \t]*[^:]*:\\)")
	   (setq newindent (+ opal-indent-width (opal-match-len 1))))
	  ((looking-at "^\\(LEMMA[ \t]*[^:]*:\\)")
	   (setq newindent (+ opal-indent-width (opal-match-len 1))))
	  ((looking-at "^\\([ \t]*IMPORT[ \n]+\\)[^ \t]")
	   (setq newindent (opal-match-len 1)))
	  ((looking-at "^\\([ \t]*\\(TYPE\\|DATA\\).*==[ \t]*\\)[^ \t\n]")
	   (setq newindent (opal-match-len 1)))
	  ((looking-at "^\\([ \t]*/\\*[ \t]*\%*[ \t]*\\)[^ \t]")
	   (setq newindent (opal-match-len 1)))
;	  ((looking-at "^\\([ \t]*LAW.*== [ \t]*\\)")
;	   (setq newindent (+ opal-indent-width (opal-match-len 1))))
	  ((looking-at "^\\([ \t]*\\(LET\\|WHERE\\) \\)\\(.*==[ \t]*\\)")
	   (setq newindent (opal-match-len 1))
	   (setq letindent (+ (opal-match-len 1) 
			      (opal-match-len 3)
			      opal-indent-width)))
	  ((looking-at "^\\([ \t]*IN +\\)[^ \t]")
	   (setq newindent (opal-match-len 1)))
	  ((looking-at opal-indent-indent-keyword)
	   (setq newindent (+ opal-indent-width indent)))
	  (t
	   (setq newindent indent))
	  )
    (goto-char limit)
    ;; and look at current position again
    (cond ((and letindent (not (looking-at ".*== ")))
	   (setq newindent letindent))
	  )
    newindent				; end
    )
  )

(defun opal-find-last-open-paren (poslist)
  "find last unmatched parenthesis in current line"

  (skip-chars-backward "^()\n")
  (let ((ch (preceding-char)))
    (cond ((= 0 ch) ; beginning of buffer
	   nil ; current indentation
	   )
          ((equal ?\) ch)
	   (backward-sexp)
	   (opal-find-last-open-paren nil)
	   )
	  ((equal ?\( ch)
	   (backward-char)
	   (if poslist
	       (opal-find-last-open-paren (cdr poslist))
	     ;; oeffnende Klammer gefunden
	           ;; monadische Klammer => ueberlesen
	     (cond ((looking-at opal-indent-monadic)
		    (opal-find-last-open-paren poslist))
		   ;; lambda => weiter einruecken
		   ((looking-at "\\(([ \t]*\\\\[^.]*\\.[ \t]*\\)[^ \t]")
		    (+ (current-column) (opal-match-len 1)))
		   ;; sonst
		   ( t
		     (1+ (current-column)))
		   )
	     ))
	  (t  ; must be newline
	   (if poslist
	       (1+ (car poslist))
	     nil))
	  )
    )
  )


(defun opal-match-len (no)
  "len of match no"

  (- (match-end no) (match-beginning no))
)

(defun opal-end-of-reference-line ()
  "goto end of reference line of current line"

  (if (= 0 (forward-line -1))
      ;; room left
      (cond ((looking-at "[ \\t]*-- .*$") ; line-comment, whole line
	     (opal-end-of-reference-line))
	    ((looking-at "[ \t]*$")      ; empty line
	     (opal-end-of-reference-line)) 
	    ((looking-at "\\(.*\\)-- .*$") ; line with line-comment
	     (goto-char (match-end 1)))
	    (t ; else go to the end
	     (end-of-line))
	    )
    ;; no room left
      (cond ((looking-at "\\(.*\\)-- .*$") ; line with line-comment
	     (goto-char (match-end 1)))
	    (t ; else go to the end
	     (end-of-line))
	    )
      )
  )

(defun opal-indent-matching-let (depth)
  "find line with matching let, assuming current let-in depth is depth"

  (if (= 0 (forward-line -1))
      ;; room left
       (cond ((looking-at ".*LET.*IN") ; LET and IN in one line
	      (opal-indent-matching-let depth))
	     ((looking-at "\\(.*\\)LET") ; one LET
	      (if (= depth 0)
		  (opal-match-len 1)
		(if (looking-at ".*IN.*LET")
		    (opal-indent-matching-let depth)
		  (opal-indent-matching-let (- depth 1)))
		))
	     ((or (looking-at ".*IN[ \t]")(looking-at ".*IN$")) ; one IN
	      (opal-indent-matching-let (+ depth 1)))
	     (t
	      (opal-indent-matching-let depth))
	     )
    ;; no room left
    nil
    )
)

(defun opal-indent-matching-if (depth)
    "find line with matching if, assuming there exists a previous IF
of same nesting level"

  (if (< (forward-line -1) 0)
      nil  ;; already at first line
    (cond ((looking-at "[ \t]*--") ;; ignore line comment
	   (opal-indent-matching-if depth))
	  ((looking-at ".*DEF") ;; stop at DEF
	   (message "%s" (opal-current-line))
	   (current-indentation))
	  ((looking-at ".*FI")  ;; increase level
	   (opal-indent-matching-if (current-indentation))
	   (opal-indent-matching-if depth))
	  ((looking-at "\\(.*[^a-zA-Z0-9]\\)IF")
	   (cond ((= depth 0) ;; found matching IF
		  (message "%s" (opal-current-line))
		  (opal-match-len 1))
		 ((= depth (opal-match-len 1)) ;; another IF of current nesting
		  (opal-indent-matching-if depth))
		 (t ;; IF of different nesting
		  (forward-line 1) ; so that outer level will look at this line
		  t))) ;; return
	  ((looking-at "^IF")
	   (cond ((= depth 0) ;; found matching IF
		  (message "%s" (opal-current-line))
		  (opal-match-len 1))
		 ((= depth (opal-match-len 1)) ;; another IF of current nesting
		  (opal-indent-matching-if depth))
		 (t ;; IF of different nesting
		  (forward-line 1) ; so that outer level will look at this line
		  t))) ;; return
	  (t ;; else next line
	   (opal-indent-matching-if depth)) 
	  )
    )
)

(defun opal-indent-inner-if (depth &optional thenExpr)
    "t iff IF is not outer IF"

    (let (newThenExpr)
      (if (< (forward-line -1) 0)
	  nil  ;; already at first line
	(if (or (looking-at "[ \t]*--") (looking-at "[ \t]*$"))
	    (setq newThenExpr thenExpr)
	  (setq newThenExpr t)
	  )
	(cond ((looking-at "[ \t]*--") ;; ignore line comment
	       (opal-indent-inner-if depth newThenExpr))
	      ((looking-at ".*DEF") ;; stop at DEF
	       (opal-indent-outer-if))
	      ((looking-at ".*FI")  ;; increase level
	       (opal-indent-inner-if (current-indentation))
	       (opal-indent-inner-if depth newThenExpr))
	      ((looking-at ".*\\(ELSE\\)")
	       (if (= depth 0)
		   (opal-indent-outer-if)
		 (opal-indent-inner-if depth newThenExpr)))
	      ((looking-at ".*\\(IN\\)[ \t]*\\(-- .*\\)?$")
	       (if (= depth 0)
		   (opal-indent-outer-if thenExpr)
		 (opal-indent-inner-if depth newThenExpr)))
	      ((and (looking-at ".*THEN[ \t]*\\(-- .*\\)?$")
		     (not (looking-at "^ *IF"))
		     )
	       (if (= depth 0)
		   (opal-indent-outer-if thenExpr) ;; inner IF if then-expression encountered
		 (opal-indent-inner-if depth newThenExpr))
	       )
	      ((and (looking-at ".*THEN") ;; THEN with expression
		    (not (looking-at "^ *IF"))
		    )
	       (if (= depth 0)
		   t
		 (opal-indent-inner-if depth t)))
	      ((looking-at "\\(.*[^a-zA-Z0-9]\\)IF.*THEN[ \t]*\\(-- .*\\)?$")
	       (cond ((= depth 0) ;; found matching IF with empty THEN 
		      (opal-indent-outer-if thenExpr))
		     ((= depth (opal-match-len 1)) ;; another IF of current nesting
		      (opal-indent-inner-if depth newThenExpr))
		     (t ;; IF of different nesting
		      (forward-line 1) ; so that outer level will look at this line
		      t))) ;; return
	      ((looking-at "\\(.*[^a-zA-Z0-9]\\)IF")
	       (cond ((= depth 0) ;; found matching IF with THEN with expr
		      t)
		     ((= depth (opal-match-len 1)) ;; another IF of current nesting
		      (opal-indent-inner-if depth t))
		     (t ;; IF of different nesting
		      (forward-line 1) ; so that outer level will look at this line
		      t))) ;; return
	      ((looking-at "^ *IF")
	       (cond ((= depth 0) ;; found matching IF
		      t)
		     ((= depth (opal-match-len 1)) ;; another IF of current nesting
		      (opal-indent-inner-if depth newThenExpr))
		     (t ;; IF of different nesting
		      (forward-line 1) ; so that outer level will look at this line
		      t))) ;; return
	      ((looking-at (concat ".*" opal-indent-monadic))
	       (if (= depth 0) (opal-indent-outer-if)
		 (opal-indent-inner-if depth newThenExpr)))
	      (t ;; else next line
	       (opal-indent-inner-if depth newThenExpr)) 
	      )
	)
      )
)

(defun opal-indent-outer-if (&optional inner)
  "print message `first guard', if inner is nil. return inner" 

  (if inner
      nil
    (message "%s" "at first guard")
    )
  inner
)