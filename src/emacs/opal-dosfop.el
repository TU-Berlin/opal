;;; functions for convenient editing of "dosfop"-documented OPAL sources

(defun opal-mode-dosfop-keymap ()
  "Set the opal-mode dosfop keymap."
;; Tastenbelegungen von DOSFOP
;;longkeys
  (define-key opal-mode-map "\C-c\C-d\C-f" 'dosfop-docs-to-doc-file)
  (define-key opal-mode-map "\C-c\C-d\C-l" 'dosfop-docs-to-current-buffer)
  (define-key opal-mode-map "\C-c\C-d\C-n" 'dosfop-next-documentary)
  (define-key opal-mode-map "\C-c\C-d\C-p" 'dosfop-prev-documentary)
  (define-key opal-mode-map "\C-c\C-di" 'dosfop-insert-@item)
  (define-key opal-mode-map "\C-c\C-dn" 'dosfop-insert-@noindent)
  (define-key opal-mode-map "\C-c\C-dt" 'dosfop-insert-@table)
  (define-key opal-mode-map "\C-c\C-dc" 'dosfop-insert-@code)
  (define-key opal-mode-map "\C-c\C-dx" 'dosfop-insert-@index)
  (define-key opal-mode-map "\C-c\C-dm" 'dosfop-insert-@center)
  (define-key opal-mode-map "\C-c\C-dh" 'dosfop-insert-@emph)
  (define-key opal-mode-map "\C-c\C-df" 'dosfop-insert-@footnote)
  (define-key opal-mode-map "\C-c\C-d\C-i" 'dosfop-insert-@i)
  (define-key opal-mode-map "\C-c\C-dz" 'dosfop-insert-@itemize)
  (define-key opal-mode-map "\C-c\C-de" 'dosfop-insert-@end)
  (define-key opal-mode-map "\C-c\C-dq" 'dosfop-insert-@quotation)
  (define-key opal-mode-map "\C-c\C-d\C-d" 'dosfop-insert-long-documentary-environment)
  (define-key opal-mode-map "\C-c\C-d\C-s" 'dosfop-insert-long-subsection-environment) 
  (define-key opal-mode-map "\C-c\C-dd" 'dosfop-insert-short-documentary-environment)
  (define-key opal-mode-map "\C-c\C-ds" 'dosfop-insert-short-subsection-environment)
  (cond (opal-running-xemacs (opal-dosfop-menu-xemacs))
        (t                   (opal-dosfop-menu-fsfemacs))
  )
)

(defun opal-dosfop-menu-fsfemacs ()
  "define opal-dosfop menu for FSF Emacs"
  (if (not opal-novice)
      (progn
	;; Menuepunkt DOSFOP:
	(define-key opal-mode-map [menu-bar opal dosfop]
	  (cons "Dosfop" (make-sparse-keymap "Dosfop")))
	
	(define-key opal-mode-map [menu-bar opal dosfop dosfop-docs-to-doc-file]
	  '("Move OUT documentaries" . dosfop-docs-to-doc-file))
	(define-key opal-mode-map [menu-bar opal 
					    dosfop dosfop-docs-to-current-buffer] 
	  '("Move IN documentaries" . dosfop-docs-to-current-buffer))
	
	(define-key opal-mode-map [menu-bar opal dosfop t1]    '("" . nil))
	
	(define-key opal-mode-map [menu-bar opal dosfop dosfop-next-documentary]
	  '("Goto next documentary" . dosfop-next-documentary))
	(define-key opal-mode-map [menu-bar opal dosfop dosfop-prev-documentary] 
	  '("Goto previous documentary" . dosfop-prev-documentary))
	
	(define-key opal-mode-map [menu-bar opal dosfop t2]    '("" . nil))
	
	(define-key opal-mode-map [menu-bar opal dosfop dosfop-insert-@itemize] 
	  '("Insert-@itemize" . dosfop-insert-@itemize))
	(define-key opal-mode-map [menu-bar opal dosfop dosfop-insert-@table]  
	  '("Insert-@table" . dosfop-insert-@table))
	(define-key opal-mode-map [menu-bar opal dosfop dosfop-insert-@quotation]
	  '("Insert-@quotation" . dosfop-insert-@quotation))
	(define-key opal-mode-map [menu-bar opal dosfop dosfop-insert-@end] 
	  '("Insert-@end" . dosfop-insert-@end))
	(define-key opal-mode-map [menu-bar opal dosfop dosfop-insert-@i] 
	  '("Insert-@i" . dosfop-insert-@i))
	(define-key opal-mode-map [menu-bar opal dosfop dosfop-insert-@footnote] 
	  '("Insert-@footnote" . dosfop-insert-@footnote))
	(define-key opal-mode-map [menu-bar opal dosfop dosfop-insert-@emph] 
	  '("Insert-@emph" . dosfop-insert-@emph))
	(define-key opal-mode-map [menu-bar opal dosfop dosfop-insert-@center] 
	  '("Insert-@center" . dosfop-insert-@center))
	(define-key opal-mode-map [menu-bar opal dosfop dosfop-insert-@index] 
	  '("Insert-@index" . dosfop-insert-@index))
	(define-key opal-mode-map [menu-bar opal dosfop dosfop-insert-@code] 
	  '("Insert-@code" . dosfop-insert-@code))
	(define-key opal-mode-map [menu-bar opal dosfop dosfop-insert-@noindent] 
	  '("Insert-@noindent" . dosfop-insert-@noindent))
	(define-key opal-mode-map [menu-bar opal dosfop dosfop-insert-@item] 
	  '("Insert-@item" . dosfop-insert-@item))
	
	(define-key opal-mode-map [menu-bar opal dosfop t3]    '("" . nil))
	
	(define-key opal-mode-map [menu-bar opal dosfop 
					    dosfop-insert-short-subsection-environment] 
	  '("Insert-line-sectioning-env -- %$$" . 
	    dosfop-insert-short-subsection-environment))
	(define-key opal-mode-map [menu-bar opal dosfop 
					    dosfop-insert-short-documentary-environment]
	  '("Insert-line-documentary-env -- % " . 
	    dosfop-insert-short-documentary-environment))
	(define-key opal-mode-map [menu-bar opal dosfop 
					    dosfop-insert-long-subsection-environment]
	  '("Insert-nested-sectioning-env /* %$$ */" . 
	    dosfop-insert-long-subsection-environment))
	(define-key opal-mode-map [menu-bar opal dosfop 
					    dosfop-insert-long-documentary-environment]
	  '("Insert-nested-documentary-env /* % */" . 
	    dosfop-insert-long-documentary-environment))
      )
  )
)

(defun opal-dosfop-menu-xemacs ()
  "define opal-dosfop menu for xeamcs"

  (setq opal-dosfop-menu 
	(list "Dosfop"
	      ["Insert-nested-documentary-env /* % */" 
	       dosfop-insert-long-documentary-environment t]
	      ["Insert-nested-sectioning-env /* %$$ */" 
	       dosfop-insert-long-subsection-environment t]
	      ["Insert-line-documentary-env -- %"  
	       dosfop-insert-short-documentary-environment t]
	      ["Insert-line-sectioning-env -- %$$"  
	       dosfop-insert-short-subsection-environment t]
	      "---"
	      (list "Insert command "
		    ["Insert-@item" dosfop-insert-@item t]
		    ["Insert-@noindent" dosfop-insert-@noindent t]
		    ["Insert-@code" dosfop-insert-@code t]
		    ["Insert-@index" dosfop-insert-@index t]
		    ["Insert-@center" dosfop-insert-@center t]
		    ["Insert-@emph" dosfop-insert-@emph t]
		    ["Insert-@footnote" dosfop-insert-@footnote t]
		    ["Insert-@i" dosfop-insert-@i t]
		    ["Insert-@end" dosfop-insert-@end t]
		    ["Insert-@quotation" dosfop-insert-@quotation t]
		    ["Insert-@table" dosfop-insert-@table t]
		    ["Insert-@itemize" dosfop-insert-@itemize t]
		    )
	      "---"
	      ["Goto previous documentary" dosfop-prev-documentary t]
	      ["Goto next documentary" dosfop-next-documentary t]
	      "---"
	      ["Move IN documentaries" dosfop-docs-to-current-buffer t]
	      ["Move OUT documentaries" dosfop-docs-to-doc-file t]
	      ))
  )

;;*****************************************
;; writing out and reading in documentaries
;;*****************************************

(defvar doc-buffer-name nil)

(defun dosfop-docs-to-doc-file()
  "writes all documentaries of the current buffer to its corresponding
   documentation buffer *.doc if that buffer not already exists; in case
   it exists, nothing is done and nil is returned."
  (interactive)

  ;set documentary buffer name  
  (setq doc-buffer-name (concat (buffer-name) ".doc"))

  ;already existent?  
  (if (file-exists-p doc-buffer-name)                                
      (message "documentation file already exists: read in first") ;don't overwrite!
      (progn                                             
         ;reserve doc buffer for output
         (find-file-noselect doc-buffer-name)                     
         (save-excursion                                             
            ;start home in source buffer
            (goto-char(point-min))                                   
            (write-all-docs-to-doc-buffer 1)
            ;save source buffer name
            (let ((curr-buf (buffer-name))                  
                 )
                 ;current buffer is doc buffer
                 (set-buffer doc-buffer-name)  
                 ;save doc buffer to file
                 (save-buffer)
                 ;kill doc buffer
                 (kill-buffer doc-buffer-name)
                 ;restore source buffer
                 (set-buffer curr-buf)   
            );let  
         )
      )
) )


(defun write-all-docs-to-doc-buffer (doc-no)
  "recursive calls for each documentary found; the documentary label is
   increased with every recursive call"
  (if (write-next-doc-to-doc-buffer doc-no)
      (write-all-docs-to-doc-buffer (+ doc-no 1))
      doc-no
) )


(defun write-next-doc-to-doc-buffer (doc-no)
  "pos-no has to contain the actual position of the possible next documentary
   w.r.t. the whole buffer; doc-buffer-name is the destination where the
   documentary found is written to; the function returns nil <=> do next 
   documentation is found; true otherwise."

  ;search for doc in current buffer
  (let ( (search-res (search-doc)))
    ;search-res != nil ?
    (if search-res                                              
        ;save source buffer name
        (let ( (curr-buf-name (buffer-name))                           
               (doc-string   
                  (buffer-substring (car (cdr search-res))
                                    (cdr (cdr search-res)))
                )
             )
             (progn 
                ;erase doc in source file 
               (delete-region (car (cdr search-res)) (cdr (cdr search-res))) 
                ;move point to start of erased doc
               (goto-char (car (cdr search-res)))
               (let ( (label-string 
                      (if (eq 'nested (car search-res))
                         (concat(concat "/* %" (int-to-string doc-no))  
                                        "*/"
                         )
                         (concat(concat "-- %" (int-to-string doc-no))  
                                        "\n"
                         )
                       )
                    ))
            
                    ;insert mark with label instead
                    (insert label-string)
                    ;position point at end of inserted label
                    (goto-char (+ (car (cdr search-res))                    
                                  (length label-string)))
               )
               ;make doc-buffer current
               (set-buffer doc-buffer-name)
               ;move point at end of doc buffer
               (goto-char (point-max))
               ;append deleted doc to doc buffer 
               (insert doc-string)  
               (insert "\n")
               ;restore source buffer
               (set-buffer curr-buf-name)
               t
             )
        )
        ;no next doc found in source
        nil  
    )
  )
)


(defun dosfop-docs-to-current-buffer()
  (interactive)
  ;set doc buffer name
  (setq doc-buffer-name (concat (buffer-name) ".doc"))      

  ;does the doc buffer already exist?
  (if (file-exists-p doc-buffer-name)
      (progn
         ;create buffer for doc file
         (find-file-noselect doc-buffer-name)
         ;save source buffer name
         (let ( (curr-buf (buffer-name))
              )
              ;doc buffer is current buffer
              (set-buffer doc-buffer-name)
              ;start at home in doc buffer
              (goto-char (point-min))
              ;restore source buffer as current
              (set-buffer curr-buf)
              (read-all-docs-to-current-buffer 1)
              ;delete doc file
              (delete-file doc-buffer-name)
              ;also delete corresponding buffer
              (kill-buffer doc-buffer-name) 
         );let
      );progn
      ;assumed doc file does not exist
      (message "no documentation file present")
) )

(defun read-all-docs-to-current-buffer(doc-pos)
  "recursively inserts all documentations stored in the file doc-buffer-name
   into its corresponding former positions"
  (if (read-next-doc-to-current-buffer doc-pos)
      (read-all-docs-to-current-buffer (+ doc-pos 1))
      t
) )


(defun read-next-doc-to-current-buffer (doc-pos)
  "reads the documentation and its references in the buffer doc-buffer-name
   and replaces the corresponding mark in the current buffer with the
   stored documentation; the function returns nil <=> no additional
   documentation can be found in the documentation file; t otherwise."
  ;save source buffer name
  (let ( (curr-buf (buffer-name))                           
       )
       ;doc buffer is current buffer
       (set-buffer doc-buffer-name)                                    
       ;search for doc in doc buffer
       (let ((search-res (search-doc))
            )
         (if search-res
             ;extract found doc 
          (let ( (doc-string (buffer-substring(car (cdr search-res))
                                              (cdr (cdr search-res))
                ) )           )
                ;restore source buffer
                (set-buffer curr-buf)                                  
                (save-excursion                                          
                   ;start from beginning
                   (goto-char(point-min))                             
                   ;search for doc-mark in source buffer
                   ;if doc-mark is not found the search is skipped and the next doc-mark
                   ;is searched
                   (if (re-search-forward (concat
                                             (concat
                                                (concat               
                                                 "/\\* %" 
                                                 (int-to-string doc-pos)
                                                )
                                              "\\*/\\|"
                                             )
                                             (concat
                                                (concat               
                                                 "-- %" 
                                                 (int-to-string doc-pos)
                                                )
                                                "\n"
                                             )
                                          ) nil t 1
                       )
                        ;replace doc-string for doc-mark
                       (replace-match doc-string)                     
                   );if
                   t
                );save-excursion
           );let
           nil
       );if
    );let
   );let
);defun


(defun search-doc()
  "searches for the start- and ending pos of a documentary in the current buffer"
  ;search for nested-documentary ot line-documentary
  ;sectioning- and ignored-documentries are not considered
  (if (re-search-forward "/\\*\\ %[^\\$-]\\|--\\ %[^\\$-]" nil t 1)
     (let ( (start-pos (- (point) 5)))
       ;check if line- or nested-documentary has been detected
       (if (char-equal (char-after start-pos) ?/)
         (if (search-forward "*/" nil t 1)
           (let ( (end-pos (point) ))
             (cons 'nested (cons start-pos end-pos))
           );let
           nil
         );if
         (if (search-forward "\n" nil t 1)
           (let ( (end-pos (point) ))
             (cons 'line (cons start-pos end-pos))
           );let
           nil
         );if
       );if
     );let
  );if
);defun


(defun save-file-buffer (buf-name)
  "saves the buffer-name in its corresponding file"
  (let ( (curr-buf (buffer-name))
       )
       (progn 
         (set-buffer buf-name)
         (save-buffer buf-name)
         (set-buffer curr-buf)
       )
) )    


;;*******************************
;; movement between documentaries
;;*******************************

(defun dosfop-next-documentary()
  (interactive)
  (re-search-forward "/\\*\\ %\\|-- %" nil t 1)
)

(defun dosfop-prev-documentary()
  (interactive)
  (re-search-backward "/\\*\\ %\\|-- %" nil t 1)
)

;;******************
;; input convenience
;;******************

(defconst dosfop-environment-regexp
  "^@\\(f?table\\|enumerate\\|itemize\\|ifinfo\\|iftex\\|quotation\\|flushleft\\|flushright\\|ignore\\|group\\|tex\\)"
  "Regexp for environment-like TexInfo list commands.
Subexpression 1 is what goes into the corresponding `@end' statement.")

;; The following dosfop-insert-@end command not only inserts a SPC
;; after the @end, but tries to find out what belongs there.  It is
;; not very smart: it does not understand nested lists.

(defun dosfop-insert-@end ()
  "Insert the matching `@end' for a @table etc. in a texinfo documentary."
  (interactive)
  (let ((string (save-excursion
		(if (re-search-backward
		     dosfop-environment-regexp nil t) 
		    (buffer-substring (match-beginning 1)
				      (match-end 1))))))
    (insert "@end ")
    (if string (insert string))
  );let
);defun


;;-----------------------------------
;;surrounding marked text with braces
;;-----------------------------------

(defun dosfop-insert-@-with-marked-text (string)
  "inserts @string and encloses some marked text with braces {}"
  (if (dosfop-region-active)
      (progn
        (save-excursion
           (if (< (point) (mark))                     
              ;"change" point and mark if necessary
              (progn
                 (insert "@" string "{")
                 (goto-char(mark))
                 (insert "}")
              )
              (progn
                (save-excursion
                  (goto-char (mark))
                  (insert "@" string "{")
                )
                (insert "}")
              )
           );if
        )
      )
      (progn
         (insert "@" string "{}")                        ;no marked text
         (backward-char)
      )
  );if
);defun


(defun dosfop-region-active ()
  "tells whether region is active for FSF and X Emacs"
  (interactive)

  (if opal-running-xemacs
      (region-exists-p)
      (mark t)
  )
)

(defun dosfop-insert-braces ()
  "Make a pair of braces and be poised to type inside of them.
Use \\[up-list] to move forward out of the braces."
  (interactive)
  (insert "{}")
  (backward-char)
);defun

;;--------------------------------
;; insertation of Texinfo commands
;;--------------------------------

(defun dosfop-insert-@code ()
  "Insert the string @code in a texinfo documentary."
  (interactive)
  (dosfop-insert-@-with-marked-text "code")
);defun

(defun dosfop-insert-@index ()
  "Insert the string @index in a texinfo documentary."
  (interactive)
  (dosfop-insert-@-with-marked-text "index")
);defun

(defun dosfop-insert-@center ()
  "Insert the string @center in a texinfo documentary."
  (interactive)
  (dosfop-insert-@-with-marked-text "center")
);defun

(defun dosfop-insert-@emph ()
  "Insert the string @emph in a texinfo documentary."
  (interactive)
  (dosfop-insert-@-with-marked-text "emph")
);defun

(defun dosfop-insert-@footnote ()
  "Insert the string @footnote in a texinfo documentary."
  (interactive)
  (dosfop-insert-@-with-marked-text "footnote")
);defun

(defun dosfop-insert-@i ()
  "Insert the string @i in a texinfo documentary."
  (interactive)
  (dosfop-insert-@-with-marked-text "i")
);defun

(defun dosfop-insert-@itemize ()
  "Insert the string @itemize in a texinfo documentary."
  (interactive)
  (insert "@itemize\n@item \n@end itemize")
  (previous-line 1)
  (end-of-line)
);defun


(defun dosfop-insert-@item ()
  "Insert the string @item in a texinfo decumentary."
  (interactive)
  (insert "@item ")

);defun

(defun dosfop-insert-@noindent ()
  "Insert the string @noindent in a texinfo documentary."
  (interactive)
  (insert "@noindent\n")
);defun

(defun dosfop-insert-@table ()
  "Insert the string @table in a texinfo documentary."
  (interactive)
  (insert "@table @asis\n@item \n@end table")
  (previous-line 1)
  (end-of-line)
);defun

(defun dosfop-insert-@quotation ()
  "Insert the string @quotation in a texinfo documentary."
  (interactive)
  (insert "@quotation\n\n@end quotation")
  (previous-line 1)
);defun

;;----------------------------------------
;; insertation of documentary environments
;;----------------------------------------

(defun dosfop-insert-long-documentary-environment ()
  "Insert the environment necessary to write down a documentary"
  (interactive)
  (insert "/* % */")
  (backward-char 3)                                     ;move back to space after %
);defun

(defun dosfop-insert-long-subsection-environment ()
  "Insert the environment necessary to write down a module-subsection"
  (interactive)
  (insert "/* %$$ */")
  (backward-char 4)                                     ;move back to second $
);defun

(defun dosfop-insert-short-documentary-environment ()
  "Insert the environment necessary to write down a documentary"
  (interactive)
  (insert "-- % \n")
  (previous-line 1)
  (end-of-line)
);defun

(defun dosfop-insert-short-subsection-environment ()
  "Insert the environment necessary to write down a module-subsection"
  (interactive)
  (insert "-- %$$\n")
  (previous-line 1)
  (end-of-line)
  (backward-char 1)                                     ;move back to second $
);defun

(provide 'opal-dosfop)
