;; emacs-el-file for parsing opal identifier
;; ralfi Febr. 1994 version 1.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global key-bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(global-set-key "\M-1" 'backward-opal-ide)
;(global-set-key "\M-2" 'forward-opal-ide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zeichenmengen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq not-letgit "[^a-zA-Z0-9]+[?]*")
(setq letgit "[a-zA-Z0-9]+[?]*")


(setq not-special "[^-!#$%&*+./:;<=>?@\\^`{|}~]") ; - muss am Anfang stehen, sonst Sonderbedeutung
					; \\ -> \
(setq special "[-!#$%&*+./:;<=>?@\\^`{|}~]+")

(defun op-mk-alt (alt1 alt2 &optional alt3)
  (let ((ft (concat "\\(" (concat alt1 (concat "\\|" alt2)))))
    (concat
     (if alt3
	  (concat ft (concat "\\|" alt3))
       ft)
     "\\)")))

(defun backward-opal-ide ()
  "Move point backward one opal-ident."
  (interactive)
  (backward-char)
  (while (and (string-match not-special 
			    (char-to-string (following-char)))
	      (string-match not-letgit 
			    (char-to-string (following-char)))
	      (not(= ?_ (following-char))))
    (backward-char)
    )
  (begin-opal-ide)
  )

(defun begin-opal-ide ()
  "Move point to begin of an opal-ident."
  (interactive)
  (cond ((string-match letgit (char-to-string (following-char)))
	 (if (re-search-backward not-letgit nil t)
	     (forward-char)
	   (goto-char (point-min))))
	((= ?? (following-char))
	 (while (= ?? (preceding-char))
	   (backward-char))
	 (if (string-match (op-mk-alt letgit special)
			   (char-to-string (preceding-char)))
	     (progn
	       (backward-char)
	       (begin-opal-ide)
	       )))
	((string-match special (char-to-string (following-char)))
	 (if (re-search-backward not-special nil t)
	     (progn ; Suche erfolgreich
	       (forward-char)
	       (if (= ?? (following-char)) ; Korrektur bei aaa?+++
		   (if (string-match letgit (char-to-string (preceding-char)))
		       (re-search-forward "[?]+"))))
	   (goto-char (point-min))))) ; Suchfehler
  (if (= ?_ (preceding-char))
      (progn
	(while (= ?_ (preceding-char))
	  (backward-char))
	(if (string-match (op-mk-alt letgit special)
			  (char-to-string (preceding-char)))
	    (progn
	      (backward-char)
	      (begin-opal-ide)
	      )))
    )
  )



(defun end-opal-ide ()
  "Move point forward to the end of the current opal-ident."
  (interactive)
  (if (string-match (op-mk-alt special letgit "_")
		    (char-to-string (following-char)))
      (forward-opal-ide))
  )

(defun forward-opal-ide ()
  "Move point forward one opal-ident."
  (interactive)
;  (princ "ok")
  (if (= ?? (following-char))
      (re-search-backward "[^?]" nil t))
  (if (not (= ?_ (following-char)))
      (re-search-forward (op-mk-alt letgit special) nil t))
  (if (= ?_ (following-char))
      (progn
	(while (= ?_ (following-char))
;	  (princ (following-char))
	  (forward-char)
	  )
	(if (string-match (op-mk-alt letgit special) 
			  (char-to-string (following-char)))
	    (forward-opal-ide)
	  )
	)
    )
  )

(defun opal-current-ide ()
  "determine opal identifier at point"

  (interactive)
  (save-excursion
    (forward-char)
    (backward-opal-ide)
    (let ((a (point)))
      (end-opal-ide)
      (let ((b (point)))
	(buffer-substring a b)
	)
      )
    )
  )
      
  

(provide 'opal-parser)
