;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; opal-import.el
;; support importing items in OPAL structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $_header$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar opal-import-fold t
  "*if t, change long ONLY imports to COMPLETELY imports")

;;; 0) set up keymaps and menus

(defun opal-import-menu-xemacs ()
  "set up opal-mode import menu for XEmacs"

  (interactive)
  (setq opal-import-menu
	(list "Import"
	      ["Structure" opal-import-structure-items t]
	      ["Item" opal-import-item t]
	      "---"
	      ["Show imports of structure" opal-import-show t]
	      "---"
	      ["Correct import maps" opal-correct-alists opal-alist-file]
	)
  )
)

(defun opal-import-popup-xemacs ()
  "popup opal-mode import menu for XEmacs"
  (interactive)

  (popup-menu opal-import-menu)
)

(defun opal-import-menu-fsfemacs ()
  "set up opal-mode import menu for FSF Emacs"

  (interactive)
  (if opal-novice
      () ; see opal-mode-set-menu for opal novices
    (define-key opal-mode-map [menu-bar opal import]
      (cons "Import" (make-sparse-keymap "Import")))

    (define-key opal-mode-map [menu-bar opal import opal-correct-alists]
      '("Correct import maps" . opal-correct-alists))
    (define-key opal-mode-map [menu-bar opal import t1]
      '("" . nil))
    (define-key opal-mode-map [menu-bar opal import opal-import-show]
      '("Show imports" . opal-import-show))
    (define-key opal-mode-map [menu-bar opal import t2]
      '("" . nil))
    (define-key opal-mode-map [menu-bar opal import opal-import-item]
      '("Item" . opal-import-item))
    (define-key opal-mode-map [menu-bar opal import 
					opal-import-structure-items]
      '("Structure" . opal-import-structure-items))
  )
)

(defun opal-import-popup-fsfemacs ()
  "set up opal-mode import popmenu for FSF Emacs"

  (interactive)
  (setq import-keymap (make-sparse-keymap "Import"))

  (define-key import-keymap [opal-correct-alists]
    '("Correct import maps" . opal-correct-alists))
  (define-key import-keymap [t1]
    '("" . nil))
  (define-key import-keymap [opal-import-show]
    '("Show imports" . opal-import-show))
  (define-key import-keymap [t2]
    '("" . nil))
  (define-key import-keymap [opal-import-item]
    '("item" . opal-import-item))
  (define-key import-keymap [opal-import-structure-items]
    '("structure" . opal-import-structure-items))

  (if (not opal-novice)
      (progn
	(fset 'import-menu import-keymap)
	(define-key opal-mode-map [A-down-mouse-2] 'import-menu)
      )
  )
)

(put 'opal-correct-alists 'menu-enable 'opal-alist-file)
(put 'opal-import-item-selection 'menu-enable '(mark t))

(defun opal-mode-import-keymap ()
  "set up keymap for opal-import, initilize menus and import"

  (define-key opal-mode-map "\C-c\C-v\C-s" 'opal-import-structure-items)
  (define-key opal-mode-map "\C-c\C-v\C-c" 'opal-import-structure-completely)
  (define-key opal-mode-map "\C-c\C-v\C-w" 'opal-import-show)
  (define-key opal-mode-map "\C-c\C-v\C-v" 'opal-import-item)
  (define-key opal-mode-map "\C-c\C-v\C-r" 'opal-import-item-selection)
  (define-key opal-mode-map "\C-c\C-v\C-o" 'opal-import-item-current-opal-ide)
  (define-key opal-mode-map "\C-c\C-vs" 'opal-save-alists)
  (define-key opal-mode-map "\C-c\C-vr" 'opal-read-alists)
  (define-key opal-mode-map "\C-c\C-vp" 'opal-print-alists)
  (define-key opal-mode-map "\C-c\C-vc" 'opal-correct-alists)

  (if opal-running-xemacs
      (progn
	(opal-import-menu-xemacs)
	(define-key opal-mode-map [(alt button2)] 'opal-import-popup-xemacs)
      )
      (progn
	(opal-import-menu-fsfemacs)
	(opal-import-popup-fsfemacs)
      )
    )
  ; (opal-setup-alists-buffer) ;; this is a problem for font-lock
)

;;;; 1) ask first for structure and then for items 

(defun opal-import-structure-items ()
  "asks for import structure and item to be imported; adds this item to the import list of the structure."
  (interactive)

  (let* ((str (opal-import-ask-for-structure "Import from structure:"))
        )
        (opal-import-structure-items-1 str)
	(opal-import-show-str str)
  )
)

(defun opal-import-structure-items-1 (str)
  "ask for first item to be imported from structure; empty item -> COMPLETELY"

  (interactive "sImport from structure:")
  (let* ((it (opal-import-ask-for-item-of-structure 
	      str (concat "Item to be added to IMPORT " str 
			  " (or RET for COMPLETELY):")))
	)
    (if (not (string= it ""))
	(progn
	  (opal-import-str-it str it)
	  (sit-for 1)
	  (opal-import-structure-items-2 str)
	)
        (opal-import-completely str)
    )
  )
)

(defun opal-import-structure-items-2 (str)
  "ask for items to be imported from structure; allows for item completion; repeat until empty string is returned"

  (interactive "sImport from structure:")
  (let* ((it (opal-import-ask-for-item-of-structure 
	      str (concat "Item to be added to IMPORT " str " (or RET to end):")))
	)
    (if (not (string= it ""))
	(progn
	  (opal-import-str-it str it)
	  (sit-for 1)
	  (opal-import-structure-items-2 str)
	)
    )
  )
)

(defun opal-import-structure-completely ()
  "ask for structure and add COMPLETELY import"

  (interactive)
  (let* ((str (opal-import-ask-for-structure "Import structure COMPLETELY:"))
	)
        (opal-import-completely str)
	(opal-import-show-str str)
  )
)
        

;;;; 2) determine first item and then ask for structure

(defun opal-import-item-selection ()
  "ask for structure from which to import selection"

  (interactive)
  (let* ((it (buffer-substring (region-beginning) (region-end)))
	 )
        (opal-import-item-1 it)
  )
)

(defun opal-import-item-current-opal-ide ()
  "ask for structure from which to import current opal-identifier"

  (interactive)
  (let* ((it (opal-current-ide))
	 )
        (opal-import-item-1 it)
  )
)

(defun opal-import-item (&optional it)
  "ask for item (default current opal-ide), then ask for structure to import it from"

  (interactive)
  (opal-import-item-1 
   (if it
       it
       (opal-import-ask-for-item "Item:" (opal-current-ide))
   )
  )
)

(defun opal-import-item-1 (it0)
  "ask for structure from which to import item"
  (interactive "sItem:")
  (let* ((it (opal-imports-normalize-it it0))
	 (str (opal-import-ask-for-structure-of-item
	       (concat "Which structure to import " it0 " from:") it0))
	 )
        (if (string= "" str)
	    nil
	  (opal-import-str-it str it0)
	  (opal-alists-insert str it)  ))
)

;;;; 3) the basic insertion functions

(defun opal-import-str-it (struct item &optional kind)
     "asks for import structure and item to be imported; adds this item to the import list of the structure."
  (interactive "sImport from structure:\nsItem to be added to IMPORT %s:")
  (save-excursion
    (let ((case-fold-search nil))
      (if (not kind)
	  (setq kind "")
	)
      (goto-char (point-min))       ;; an Anfang des Buffers
    (cond ((re-search-forward "\\<IMPORT\\>" nil t) 
           ;; IMPORT gefunden
           (cond ((re-search-forward (concat "\\(\\<IMPORT\\>\\)?[ \t]+"
                                      (regexp-quote struct)
                                      "[ \t]*\\<\\(ONLY\\)\\>")
                              nil t)
                  ;; passenden ONLY-Import gefunden
		  (if (looking-at (concat ".* " (regexp-quote item) "[: \n]" ))
		      (message (concat "Already importing " 
				       struct " ONLY " item))
		    (if (and opal-import-fold
			     (> (length (concat (opal-current-line)
						" " item kind)) (frame-width)))
			; line width would exceed frame width ==> fold line
			(progn
			  (beginning-of-line)
			  (re-search-forward "ONLY.*$" nil nil)
			  (replace-match "COMPLETELY" nil nil)
			  (message "[folded]%s" (opal-current-line))
			  )
		      ; else go to end and insert import
		      (end-of-line)
		      (insert (concat " " item kind))
		      (message "[added]%s" (opal-current-line))
		      )
		    )
                 )
                 ((re-search-forward (concat "\\(\\<IMPORT\\>\\)?[ \t]+"
                                      (regexp-quote struct)
                                      "[ \t]*\\<\\(COMPLETELY\\)\\>") nil t)
                  ;; passenden COMPLETELY-Import gefunden
                  (message "%s" (concat "Found " 
					(opal-current-line)))
                 )
                 (t 
                  ;; kein passender Import
                  (cond (;(y-or-n-p (concat "New IMPORT " struct 
                         ;                  " ONLY " item " ? "))
			 t
                         (goto-char (point-max))  ;; ans Ende
                         (re-search-backward "\\<\\(ONLY\\|COMPLETELY\\)\\>") 
                                        ; mit Fehler!
                         (forward-line)
                         (insert (concat "       " 
					 struct " ONLY " item kind "\n"))
                         (forward-line -1) ;; wieder zurueck
                         (message "[new import]%s" (opal-current-line))
                        )
                        (t ;; Benutzer hat "nein" gesagt
                         (message "%s" 
                                  (concat "adding IMPORT " struct " aborted"))
                        )
                   )
                 )
            ))
            (t
             ;; ueberhaupt keine Zeile mit IMPORT gefunden
             (cond ((y-or-n-p (concat "No IMPORT found! Inserting IMPORT "
                                       struct " ONLY " item " ? "))
		    (re-search-forward "SIGNATURE\\|IMPLEMENTATION\\|EXTERNAL PROPERTIES\\|INTERNAL PROPERTIES" nil t)
                    (forward-line)
                    (insert (concat "\nIMPORT " 
				    struct " ONLY " item kind "\n"))
                    (forward-line -1) ;; wieder zurueck
                    (message "%s" (opal-current-line))
                   )
                   (t ;; kein neuer IMPORT
                    (message "%s" (concat "No first IMPORT inserted" ))
                   )
             ) 
            )
    ))
  )
)

(defun opal-import-completely (struct)
     "asks for import structure to be imported completely"
  (interactive "sImport structure COMPLETELY:")
  (save-excursion
    (let ((case-fold-search nil))
    (goto-char (point-min))       ;; an Anfang des Buffers
    (cond ((re-search-forward "\\<IMPORT\\>" nil t) 
           ;; IMPORT gefunden
           (cond ((re-search-forward (concat "\\(\\<IMPORT\\>\\)?[ \t]+"
                                      (regexp-quote struct)
                                      "[ \t]*\\<\\(ONLY\\)\\>")
                              nil t)
                  ;; passenden ONLY-Import gefunden
                  (if (y-or-n-p "Found ONLY import! Change to COMPLETELY? ")
		      (progn
			(beginning-of-line)
			(re-search-forward "ONLY.*$" nil nil)
			(replace-match "COMPLETELY" nil nil)
		      )
		  )
                 )
                 ((re-search-forward (concat "\\(\\<IMPORT\\>\\)?[ \t]+"
                                      (regexp-quote struct)
                                      "[ \t]*\\<\\(COMPLETELY\\)\\>") nil t)
                  ;; passenden COMPLETELY-Import gefunden
                  (message "%s" (concat "Already importing" 
					(opal-current-line)))
                 )
                 (t 
                  ;; kein passender Import
                  (cond (;(y-or-n-p (concat "New IMPORT " struct 
                         ;                  " ONLY " item " ? "))
			 t
                         (goto-char (point-max))  ;; ans Ende
                         (re-search-backward "\\<\\(ONLY\\|COMPLETELY\\)\\>") 
                                        ; mit Fehler!
                         (forward-line)
                         (insert (concat "       " struct " COMPLETELY\n"))
                         (forward-line -1) ;; wieder zurueck
                         (message "%s" (opal-current-line))
                        )
                        (t ;; Benutzer hat "nein" gesagt
                         (message "%s" 
                                  (concat "adding IMPORT " struct " aborted"))
                        )
                   )
                 )
            ))
            (t
             ;; ueberhaupt keine Zeile mit IMPORT gefunden
             (cond ((y-or-n-p (concat "No IMPORT found! Inserting IMPORT "
                                       struct " COMPLETELY ? "))
		    (re-search-forward "SIGNATURE\\|IMPLEMENTATION\\|EXTERNAL PROPERTIES\\|INTERNAL PROPERTIES" nil t)                    (forward-line)
                    (insert (concat "\nIMPORT " struct " COMPLETELY\n"))
                    (forward-line -1) ;; wieder zurueck 
                    (message "%s" (opal-current-line)) 
                   )
                   (t ;; kein neuer IMPORT 
                    (message "%s" (concat "No first IMPORT inserted" ))
                   )
             ) 
            )
    ))
  )
) 

;;;; 4) Just show imports

(defun opal-import-show ()
  "show IMPORT line of given structure, allow structure completion"

  (interactive)
  (opal-import-show-str
   (opal-import-ask-for-structure "Show imports from structure:")
   )
)

(defun opal-import-show-str (struct)
  "show IMPORT line of given structure."

  (interactive "sShow imports from structure:")
  (save-excursion
    (goto-char (point-min))       ;; an Anfang des Buffers
    (cond ((re-search-forward (concat "[ \t]+" (regexp-quote struct)
                                      "[ \t]+\\<\\(ONLY\\|COMPLETELY\\)\\>")
                              nil t)
           (message "%s" (opal-current-line))
          )
          (t
           (message "%s" (concat "No IMPORT of " struct " found"))
          )
)))

;;;; 5) asking for structures or items with completion


(defun opal-import-ask-for-structure (prompt)
  "ask for structure, allow completion"

  (interactive)
  (let* ((str (completing-read prompt opal-structures-to-items-alist 
			     nil nil "" opal-structures-history))
	(str1 (opal-imports-normalize-str str))
       )
       (if (undefd str1 opal-structures-to-items-alist)
	   (setq opal-structures-to-items-alist
		 (cons (list str1 nil) opal-structures-to-items-alist))
       )
       str
  )
)

(defun opal-import-ask-for-item (prompt &optional default)
  "ask for item, allow completion"

  (interactive)
  (let* ((it0 (completing-read prompt opal-items-to-structure-alist 
			     nil nil default opal-structures-history))
	(it (opal-imports-normalize-it it0))
       )
       (if (undefd it opal-items-to-structure-alist)
	   (setq opal-items-to-structure-alist
		 (cons (list it nil) opal-items-to-structure-alist))
       )
       it
  )
)

(defun opal-import-ask-for-item-of-structure (str prompt)
  "ask for item in structure, allow completion"

  (interactive "sStructure:\nsPrompt:")
  (let* ((str1 (opal-imports-normalize-str str))
	 (item-completions1 (cdr (assoc str1 opal-structures-to-items-alist)))
	 (item-completions (if (null (car item-completions1)) 
			       nil item-completions1))
	 (item (completing-read prompt item-completions
				nil nil "" opal-items-history))
	 (item1 (opal-imports-normalize-it item))
        )
        (prog1
          item1
	  (opal-alists-insert str item1)
	)
  )
)

(defun opal-import-ask-for-structure-of-item (prompt it0)
  "ask for structure to import item from, allow intelligent completion"

  (interactive "sPrompt:\nsItem:")
  (let* ((it (opal-imports-normalize-it it0))
	 (str-completions1 (cdr (assoc it opal-items-to-structure-alist)))
	 (str-completions (if (null (car str-completions1)) 
			       opal-structures-to-items-alist
			       str-completions1))
	 (str-default (opal-alists-default str-completions))
	 (str (completing-read prompt str-completions
				nil nil str-default opal-structures-history))
        )
        (prog1
          str
	  (opal-alists-insert str it)
	)
  )
)

(defun opal-imports-normalize-str (str)
  "normalizes structure (strips off instantiation)"

  (string-match "[a-zA-Z0-9]+" str)
  (substring str (match-beginning 0) (match-end 0))
)

(defun opal-imports-normalize-it (it)
  "normalizes item (strips of kind)"

  (if (string-match "\\( *: *SORT\\)$" it)
      (substring it 0 (match-beginning 1))
    it)
  )



(defun opal-import-it-default (it)
  "return default structure from which item will be imported or \"\""

  (opal-alists-default (cdr (assoc it opal-items-to-structure-alist)))
)

;;;; 6) setting up and managing the alists needed for completion

(defvar opal-structures-history nil)
(defvar opal-items-history nil)

(defvar opal-items-to-structure-alist nil)
(defvar opal-structures-to-items-alist nil)

(if (not opal-novice)
    (defvar opal-alist-file "~/.opal-alists"
      "*file to save OPAL alists in")
    (defvar opal-alist-file nil
      "*file to save OPAL alists in")
)

(defun opal-alists-insert (str it)
  "add pair structure str <-> item it to alists"

  (interactive "sStructure:\nsItem to IMPORT from %s:")
  (if (or (string= "" str)
	  (string= "" it)) nil
  (let* ((str1 (progn
		 (string-match "[a-zA-Z0-9]+" str)
		 (substring str (match-beginning 0) (match-end 0))
	       ))
	 (it1 it)
	 (item-completions1 (cdr (assoc str1 opal-structures-to-items-alist)))
	 (item-completions (if (null (car item-completions1)) 
			       nil item-completions1))
         (str-completions1 (cdr (assoc it1 opal-items-to-structure-alist)))
	 (str-completions (if (null (car str-completions1)) 
			       nil str-completions1))
	 )
    (cond ((null (car item-completions))
	   (setq opal-structures-to-items-alist
		 (redef str1 (list (list it nil))
			opal-structures-to-items-alist)
		 ))
	  ((undefd it1 item-completions)
	   (setq opal-structures-to-items-alist
		 (redef str1 (cons (list it1 nil) item-completions)
			opal-structures-to-items-alist)
		 ))
    )
    (cond ((null (car str-completions))
	   (setq opal-items-to-structure-alist
		 (redef it1 (list (list str1 nil))
			opal-items-to-structure-alist)
		 ))
	  ((undefd str1 str-completions)
	   (setq opal-items-to-structure-alist
		 (redef it1 (cons (list str1 nil) str-completions)
			opal-items-to-structure-alist)
		 ))
	  )
    (opal-print-alists)
    )
))

(defun opal-alists-delete (str it)
  "delete pair structure str <-> item it to alists"

  (interactive "sStructure:\nsItem to remove from IMPORT lists %s:")
  (if (or (string= "" str)
	  (string= "" it)) nil
  (let* ((str1 (progn
		 (string-match "[a-zA-Z0-9]+" str)
		 (substring str (match-beginning 0) (match-end 0))
	       ))
	 (it1 it)
	 (item-completions1 (cdr (assoc str1 opal-structures-to-items-alist)))
	 (item-completions (if (null (car item-completions1)) 
			       nil item-completions1))
         (str-completions1 (cdr (assoc it1 opal-items-to-structure-alist)))
	 (str-completions (if (null (car str-completions1)) 
			       nil str-completions1))
	 )
    (cond ((null (car item-completions)) nil)
	  ((undefd it1 item-completions) nil)
	  ( t   (setq opal-structures-to-items-alist
		      (redef str1 
			     (delete (assoc it1 item-completions) 
				     item-completions)
			     opal-structures-to-items-alist)))
    )
    (cond ((null (car str-completions)) nil)
	  ((undefd str1 str-completions) nil)
	  ( t   (setq opal-items-to-structure-alist
		      (redef it1 
			     (delete (assoc str1 str-completions) 
				     str-completions)
			     opal-items-to-structure-alist)))
    )
    (opal-print-alists)
    )
))

(defun opal-alists-delete-str (&optional str1)
  (interactive)

  (let* ((str (if str1 str1 
		(opal-import-ask-for-structure 
		 "Structure to delete from list structure -> items:")))
	 (ass (assoc str opal-structures-to-items-alist))	  
	)
    (if ass
	(setq opal-structures-to-items-alist 
	      (delete ass opal-structures-to-items-alist))
    )
  )
  (opal-print-alists)
)

(defun opal-alists-delete-it (&optional it1)
  (interactive)

  (let* ((it (if it1 it1 
		(opal-import-ask-for-item
		 "Item to delete from list item -> structures:")))
	 (ass (assoc it opal-items-to-structure-alist))
	)
    (if ass
	(setq opal-items-to-structure-alist
	      (delete ass opal-items-to-structure-alist))
    )
  )
  (opal-print-alists)
)

(defun opal-alists-default (l)
  "l ist a list of form ((STRING X) (STRING X) ... ). return default string or the empty string if no default can be determined"

  (if (= 1 (length l))
	 (car (car l))
    (let ((result nil))
      (while l
	(if (equal (cdr (car l)) '('default))
	    (progn
	      (setq result (car (car l)))
	      (setq l nil)
	      )
	  (setq l (cdr l))
	  )
	)
      (if (not result)
	  (setq result "")
	)
      result
      )
    )
  )

(defun opal-save-alists () (interactive) (opal-save-alists-i opal-alist-file))
(defun opal-read-alists () (interactive) (opal-read-alists-i opal-alist-file))
(defun opal-print-alists () (interactive) 
  (opal-print-alists-i opal-alist-file))

(defun opal-setup-alists-buffer () 
  (interactive) 
  (opal-setup-alists-buffer-i opal-alist-file)
)

(defun opal-correct-alists () (interactive) 
"call this function, after you manually corrected the opal-alist-file"
  (opal-correct-alists-i opal-alist-file))


(defun opal-save-alists-i (file)
  "save OPAL alists to file"

  (interactive "FFile to save opal alists:")
  (if file
      (progn
	(save-excursion
	  (let ((opal-save-buffer (set-buffer (find-file-noselect file)))
		)
	    (delete-region (point-min) (point-max))
	    (print opal-structures-to-items-alist opal-save-buffer)
	    (print opal-items-to-structure-alist opal-save-buffer)
	    (insert ";; opal alists saved " (current-time-string))
	    (save-buffer)
					;    (kill-buffer opal-save-buffer)
	    )
	  )
	)
  )
)

(defun opal-read-alists-i (file)
  "read OPAL alists from file"

  (interactive "FFile to read opal alists:")
  (if file
      (if (file-exists-p file)
	  (progn
	    (save-excursion
	      (let ((opal-save-buffer (set-buffer (find-file-noselect file)))
		    )
		(goto-char (point-min))
		(setq opal-structures-to-items-alist (read opal-save-buffer))
		(setq opal-items-to-structure-alist (read opal-save-buffer))
					;    (kill-buffer opal-save-buffer)
	      )
	    )
	  )
	  (progn
	    (setq opal-structures-to-items-alist bibop-structures-list)
	    (opal-print-alists-i file)
	  )
	)
    )
)

(defun opal-print-alists-i (file)
  "print OPAL alists to buffer associated with file"

  (interactive "FFile buffer to print opal alists:")
  (if file
    (progn
      (save-excursion
	(let ((opal-save-buffer (set-buffer (find-file-noselect file)))
	      )
	  (delete-region (point-min) (point-max))
	  (print opal-structures-to-items-alist opal-save-buffer)
	  (print opal-items-to-structure-alist opal-save-buffer)
	  (insert ";; opal alists saved " (current-time-string))
	  (set-buffer-modified-p nil)
	  )
	)
      )
  )
)

(defun opal-correct-alists-i (file)
  "save OPAL alists buffer associated to file and re-read it"

  (interactive "FFile to use:")
  (if file
      (progn
	(save-excursion
	  (let ((opal-save-buffer (set-buffer (find-file-noselect file)))
	       )
	    (save-buffer)
	    (opal-read-alists-i file)
	  )
	)
      )
  )
)


(defun opal-setup-alists-buffer-i (file)
  "initialize opal-alists-buffer for file"

  (interactive "FFile of Opal alists:")
  (if file
      (progn
	(save-excursion
	  (opal-read-alists-i file)
	  (set-buffer (find-file-noselect file))
	  (setq require-final-newline nil)
	  (add-hook 'kill-emacs-hook 'opal-save-alists)
	  )
      )
      (progn
	(setq opal-structures-to-items-alist bibop-structures-list)
      )
    )
)

(defun undefd (obj alist)
  
  (null (assoc obj alist))
)

(defun redef (dom codom alist)

  (cons
   (cons dom codom)
   (delete (assoc dom alist) alist)
  )
)

;;;; 7) value to initialize opal-structures-to-items-alist

(defvar bibop-structures-list
  '(
; Internal/Compiler
      ("ABORT" nil)
      ("PREDEF_BOOL" nil)
      ("DENOTATION" nil)
; Internal/Strange
      ("CAST" nil)
      ("EQUALITY" nil)
      ("INLINE" nil)
; BasicTypes
      ("BoolConv" nil)
      ("Char" nil)
      ("CharConv" nil)
      ("Denotation" nil)
      ("Int" nil)
      ("IntConv" nil)
      ("Nat" ("nat" nil))
      ("NatConv" nil)
      ("PrintableChar" nil)
      ("Real" nil)
      ("RealConv" nil)
      ("Rel" nil)
      ("RelConv" nil)
      ("SmallReal" nil)
      ("Subrel" nil)
      ("SubrelConv" nil)
      ("Void" nil)
      ("VoidConv" nil)
; Functions/General
      ("Compose" nil)
      ("ComposePar" nil)
      ("Control" nil)
      ("Funct" nil)
      ("FunctConv" nil)
      ("Predicate" nil)
; Functions/Orderings
      ("InducedRel" nil)
      ("OrderingByInjection" nil)
      ("OrderingByLess" nil)
; Functions/Special
      ("AcceleratorC" nil)
      ("AcceleratorF" nil)
; AggregateTypes/ProductLike
      ("AnonPair" nil)
      ("AnonQuadruple" nil)
      ("AnonTriple" nil)
      ("Pair" nil)
      ("PairCompare" nil)
      ("PairConv" nil)
      ("PairMap" nil)
      ("Quadruple" nil)
      ("QuadrupleConv" nil)
      ("QuadrupleMap" nil)
      ("Triple" nil)
      ("TripleConv" nil)
      ("TripleMap" nil)
; AggregateTypes/UnionLike
      ("Option" nil)
      ("OptionCompare" nil)
      ("OptionConv" nil)
      ("OptionMap" nil)
      ("Union2" nil)
      ("Union2Conv" nil)
      ("Union3" nil)
      ("Union3Conv" nil)
      ("Union4" nil)
      ("Union4Conv" nil)
; AggregateTypes/SeqLike
      ("BTUnion" nil)
      ("BTUnionConv" nil)
      ("ISeq" nil)
      ("ISeqConv" nil)
      ("ISeqFilter" nil)
      ("ISeqIndex" nil)
      ("ISeqMap" nil)
      ("ISeqMapEnv" nil)
      ("ISeqRandom" nil)
      ("ISeqSort" nil)
      ("ISeqUnreduce" nil)
      ("ISeqZip" nil)
      ("Seq" (seq nil))
      ("SeqCompare" nil)
      ("SeqConv" nil)
      ("SeqFilter" nil)
      ("SeqFold" nil)
      ("SeqIndex" nil)
      ("SeqMap" nil)
      ("SeqMapEnv" nil)
      ("SeqOfSeq" nil)
      ("SeqReduce" nil)
      ("SeqSort" nil)
      ("SeqZip" nil)
      ("String" nil)
      ("StringAux" nil)
      ("StringConv" nil)
      ("StringConversion" nil)
      ("StringFilter" nil)
      ("StringFold" nil)
      ("StringFormat" nil)
      ("StringIndex" nil)
      ("StringMap" nil)
      ("StringMapSeq" nil)
      ("StringReduce" nil)
      ("StringScan" nil)
; AggregateTypes/TreeLike
      ("BSTree" nil)
      ("BSTreeCompare" nil)
      ("BSTreeConv" nil)
      ("BSTreeFilter" nil)
      ("BSTreeIndex" nil)
      ("BSTreeMap" nil)
      ("BSTreeMapEnv" nil)
      ("BSTreeReduce" nil)
      ("BSTreeZip" nil)
      ("Heap" nil)
      ("HeapCompare" nil)
      ("HeapConv" nil)
      ("HeapFilter" nil)
      ("HeapIndex" nil)
      ("HeapMap" nil)
      ("HeapMapEnv" nil)
      ("HeapReduce" nil)
      ("HeapZip" nil)
      ("IndexingOfTrees" nil)
      ("Tree" nil)
      ("TreeCompare" nil)
      ("TreeConv" nil)
      ("TreeFilter" nil)
      ("TreeIndex" nil)
      ("TreeMap" nil)
      ("TreeMapEnv" nil)
      ("TreeReduce" nil)
      ("TreeZip" nil)
; AggregateTypes/SetLike
      ("Bag" nil)
      ("BagConv" nil)
      ("BagFilter" nil)
      ("BagFold" nil)
      ("BagMap" nil)
      ("BagReduce" nil)
      ("Bitset" nil)
      ("BitsetConv" nil)
      ("BitsetFilter" nil)
      ("BitsetFold" nil)
      ("BitsetMap" nil)
      ("BitsetReduce" nil)
      ("Set" nil)
      ("SetByBST" nil)
      ("SetByBSTConstr" nil)
      ("SetByBSTConv" nil)
      ("SetByBSTFilter" nil)
      ("SetByBSTFold" nil)
      ("SetByBSTMap" nil)
      ("SetByBSTMapEnv" nil)
      ("SetByBSTOfSetByBST" nil)
      ("SetByBSTReduce" nil)
      ("SetByInj" nil)
      ("SetByInjConv" nil)
      ("SetByInjFilter" nil)
      ("SetByInjFold" nil)
      ("SetByInjMap" nil)
      ("SetByInjReduce" nil)
      ("SetByPred" nil)
      ("SetByPredConstr" nil)
      ("SetByPredConv" nil)
      ("SetByPredFilter" nil)
      ("SetConstr" nil)
      ("SetConv" nil)
      ("SetFilter" nil)
      ("SetFold" nil)
      ("SetMap" nil)
      ("SetMapEnv" nil)
      ("SetOfSet" nil)
      ("SetReduce" nil)
; AggregateTypes/MapLike
      ("Array" nil)
      ("ArrayConv" nil)
      ("ArrayFilter" nil)
      ("ArrayFold" nil)
      ("ArrayMap" nil)
      ("ArrayReduce" nil)
      ("MapByBST" nil)
      ("MapByBSTCompose" nil)
      ("MapByBSTConv" nil)
      ("MapByBSTFilter" nil)
      ("MapByBSTInvert" nil)
      ("MapByBSTMap" nil)
      ("MapByBSTReduce" nil)
      ("MapByOS" nil)
      ("MapByOSCompose" nil)
      ("MapByOSConv" nil)
      ("MapByOSFilter" nil)
      ("MapByOSInvert" nil)
      ("MapByOSMap" nil)
      ("MapByOSReduce" nil)
      ("MapCompose" nil)
      ("MapConv" nil)
      ("MapFilter" nil)
      ("MapInvert" nil)
      ("MapMap" nil)
      ("MapNotForUserPurpose" nil)
      ("MapReduce" nil)
; System/Debugging
      ("DEBUG" nil)
; System/Commands
      ("Com" nil)
      ("ComAction" nil)
      ("ComAgent" nil)
      ("ComAgentConv" nil)
      ("ComCheck" nil)
      ("ComChoice" nil)
      ("ComCompose" nil)
      ("ComConv" nil)
      ("ComSeqReduce" nil)
      ("ComService" nil)
      ("ComServiceConv" nil)
      ("ComTimeout" nil)
      ("Env" nil)
      ("Random" nil)
; System/Unix
      ("BinFile" nil)
      ("File" nil)
      ("FileConv" nil)
      ("FileSystem" nil)
      ("FileSystemConv" nil)
      ("FileSystemFun" nil)
      ("Process" nil)
      ("ProcessConv" nil)
      ("ProcessCtrl" nil)
      ("ProcessCtrlConv" nil)
      ("ProcessCtrlFun" nil)
      ("Signal" nil)
      ("SignalConv" nil)
      ("SysTime" nil)
      ("SysTimeConv" nil)
      ("UnixFailures" nil)
      ("UserAndGroup" nil)
      ("UserAndGroupConv" nil)
      ("UserAndGroupFun" nil)
      ("Wait" nil)
      ("WaitConv" nil)
; System/Streams
      ("BinStream" nil)
      ("Stream" nil)
; opalwin/TclTk
      ("ComSemaphor" nil)
      ("ComSeqAction" nil)
      ("ComSeqMap" nil)
      ("ComState" nil)
      ("ComStateTrans" nil)
      ("ComXAction" nil)
      ("Tcl" nil)
      ("Tk" nil)
; opalWin/OpalWin
      ("WinAppl" nil)
      ("WinButton" nil)
      ("WinCanvas" nil)
      ("WinCanvasEditor" nil)
      ("WinConfig" nil)
      ("WinDragDrop" nil)
      ("WinDrawEditor" nil)
      ("WinEmitter" nil)
      ("WinEvent" nil)
      ("WinImage" nil)
      ("WinInternal" nil)
      ("WinMenu" nil)
      ("WinMenuView" nil)
      ("WinRegulator" nil)
      ("WinScrollbar" nil)
      ("WinScroller" nil)
      ("WinSelector" nil)
      ("WinTag" nil)
      ("WinTclTk" nil)
      ("WinText" nil)
      ("WinTextEditor" nil)
      ("WinView" nil)
      ("WinWindow" nil)
   )
)

;;; 8) provide opal-import
(provide 'opal-import)
