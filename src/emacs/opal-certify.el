;; support for certification of proofs


(defconst opal-certify-tmp-buffer "*opal-certify*")

(defun opal-certify-clear-tmp ()
  "clear internal tmp buffer"
  (erase-buffer (get-buffer-create opal-certify-tmp-buffer))
  )

(defun opal-certify-initialize-tmp ()
  "insert normalized proof head at point into opal-certify buffer
return name of this proof"

;; code assumes that start and end are declared in the caller's context

  (let (thisbuf)
    (setq thisbuf (current-buffer))
    (beginning-of-line)
    (setq start (point))
    (while (not (looking-at ".*[a-zA-Z 0-9]|-[a-zA-Z 0-9]"))
      (forward-line)
      )
    (if (looking-at ".*[a-zA-Z 0-9]|-[ ]*$")
	(forward-line)
      )
    (end-of-line)
    (setq end (point))
    (opal-certify-clear-tmp)
    (set-buffer opal-certify-tmp-buffer)
    (insert (buffer-substring start end thisbuf))
    (opal-certify-normalize-tmp)
    )
  )

(defun opal-certify-normalize-tmp ()
   "normalize tmp buffer, i.e. remove some text which cannot be generated 
by the Opal compiler and return name of property"

   (set-buffer (get-buffer-create opal-certify-tmp-buffer))
   (goto-char (point-min))
   (re-search-forward "\\(PROP\\|PROOF\\) ")
   (replace-match "")
   (re-search-forward ": ")
   (replace-match " ")
   (while (re-search-forward ":LAW" nil t)
     (replace-match "")
     )
   (goto-char (point-min))
   (re-search-forward "|- ")
   (replace-match " ")
   (goto-char (point-min))
   (while (re-search-forward " +" nil t)
     (replace-match "\n")
     )
   (if (not (equal "\n" (buffer-substring
			 (- (point-max) 1) (point-max))))
     (progn
       (goto-char (point-max))
       (insert "\n")
       )
     )
   (goto-char (point-min))
   (end-of-line)
   ; return value (first line)
   (buffer-substring (point-min) (point))
)    

(defun opal-certify-remove-old-jstf (propname buf)
  "remove this justification in given buffer"

  (save-excursion
    (set-buffer buf)
    (if (re-search-forward
	 (concat "^ *\\(JSTF\\|PROOF\\) *" (regexp-quote propname) " *==") nil t)
	(progn
	  (beginning-of-line)
	  (kill-line 1)
	  (while (and (not (looking-at "^\\(LAW\\|PROOF\\|FUN\\|DEF\\|PROP\\|JSTF\\|TYPE\\|SORT\\|DATA\\)"))
		      (not (looking-at "^[ \t]*$")))
	    (kill-line 1)
	    )
	  )
      )
    )
  )

(defun opal-certify-comment (propname buf)
  "return appropriate comment for signature of propname in Opal buffer buf"
  (save-excursion
    (let (mybuf result)
      (setq mybuf (generate-new-buffer "*GPG*"))
      (set-buffer mybuf)
      (insert "Signature for justification `" propname "'  ")
      (call-process "date" nil t)
      (goto-char (point-max))
      (delete-char -1)
      (setq result (buffer-string))
      (kill-buffer mybuf)
      (setq result result)
      )
    )
  )


(defun opal-certify-sign-proof (passwd)
  "assumes that point is in a line which contains a 
   (single) proof head (proposition). computes a certification and
   inserts it in the following line"
  (interactive "sPassword:")

  (save-excursion
    (let (start end thisbuf proc procbuf result sign startsign endsign propname)
      (setq thisbuf (current-buffer))
      (setq propname (opal-certify-initialize-tmp))
      (opal-certify-remove-old-jstf propname thisbuf)
      (setq procbuf (get-buffer-create "*GPG*"))
      (setq proc (start-process 
		  "*GPG*" procbuf "gpg" "--detach-sig" 
		  "-a" "--output" "-" "--passphrase-fd" "0"
		  "--comment" (opal-certify-comment propname thisbuf)
		  ))
      (process-send-string proc (concat passwd "\n"))
      (process-send-region proc (point-min) (point-max))
      (process-send-eof proc)
      (while (eq 'run (process-status proc))
	(accept-process-output proc 5))
      (setq result (process-exit-status proc))
      (if (> result 0)
	  (progn 
	    ; (kill-buffer (get-buffer "*GPG*")
	    (error "Signing failed with error code %d" result)
	    )
	;; signieren hat geklappt
	(set-buffer procbuf)
	(goto-char (point-min))
	(re-search-forward "-----BEGIN PGP SIGNATURE-----")
	(beginning-of-line)
	(setq startsign (point))
	(re-search-forward "-----END PGP SIGNATURE-----")
	(end-of-line)
	(setq endsign (point))
	(opal-certify-clear-tmp)
	(set-buffer opal-certify-tmp-buffer)
	(insert (buffer-substring startsign endsign procbuf))
	(goto-char (point-min))
	(insert (concat "JSTF " propname " ==\n" "  signed(\x22"))
	(while (search-forward "\n" nil t)
	  (replace-match "\\\\n")
	  )
	(goto-char (point-max))
	(insert "\x22)\n")
	(set-buffer thisbuf)
	(goto-char end)
	(forward-line)
	(insert (buffer-string nil nil opal-certify-tmp-buffer))
	(kill-buffer procbuf)
	)
      )
    )
  )

(defun opal-certify-check-proof ()
  "check signature of current proof"

  (interactive)
  (save-excursion
    (let (start end thisbuf cert result ok)
      (setq thisbuf (current-buffer))
      (setq propname (opal-certify-initialize-tmp))
      (write-file ".proof")
      (set-buffer thisbuf)
      (goto-char (point-min))
      (if (not (re-search-forward (concat "^[ \t]*\\(JSTF\\|PROOF\\)[ \t]*"
					  (regexp-quote propname)
					  "[ \t]*==")))
	  (error "No justification for `%s' found" propname)
	)
      (forward-line)
      (if (not (looking-at "[ \t]*signed(\x22\\(.*\\)\x22)"))
	  (error "No (standard ?) certification found")
	)
      (setq cert (buffer-substring (match-beginning 1) (match-end 1)))
      (opal-certify-clear-tmp)
      (set-buffer opal-certify-tmp-buffer)
      (goto-char (point-max))
      (insert cert)
      (beginning-of-buffer)
      (while (re-search-forward "\\\\n" nil t)
	(replace-match "\n")
	)
      (write-file ".signature")
      (opal-certify-clear-tmp)
      (setq result 
	    (call-process "gpg" nil opal-certify-tmp-buffer nil 
			  "--verify" ".signature" ".proof"))
      (delete-file ".proof")
      (delete-file ".signature")
      (setq ok nil)
      (kill-buffer ".proof")
      (kill-buffer ".signature")
      (cond ((and (numberp result) (= result 0))
	     (message "Verification succeeded")
	     (setq ok t))
	    ((numberp result)
	     (message"Signature not verified (%d)" result))
	    (t
	     (message "Signature not verified (%s)" result))
	    )
      (popup-dialog-box 
       (list (concat "GPG output\n\n" 
		     (buffer-string (point-min opal-certify-tmp-buffer) 
				    (point-max opal-certify-tmp-buffer) 
				    opal-certify-tmp-buffer))
	     ["OK" 'opal-info-nil t]))
      )
    )
  )