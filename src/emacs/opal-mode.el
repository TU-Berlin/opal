 ;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Opal Mode
;;; Main Part
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialize
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'opal-mode)
(defvar opal-path 
  (cond ((getenv "OCS") (getenv "OCS"))
	((getenv "OCSDIR") (getenv "OCSDIR"))
	(t "/opt/ocs-2.3n")
	)
  "*root directory of Opal installation. If environment variable OCS
or OCSDIR are defined these are used otherwise /usr/ocs is taken as default.")

(defvar opal-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(defvar opal-mode-map nil)   ; Create a mode-specific keymap
(defvar opal-mode-alist nil) ; True if alist is expand 
(defvar opal-novice nil) ; true, if user should see only simple things
(defvar opal-pchecker nil) ; true if pchecker support should be activated

(require 'opal-parser)
;(require 'opal-abbrev-mode)
;(require 'opal-browser)
(require 'opal-filehandling)
(require 'opal-diag-mode)
(require 'opal-compile)
;(require 'opal-dired)
(require 'opal-dosfop)   ;;; DOSFOP
(require 'opal-switch)
(require 'opal-import)
(require 'opal-defs-mode)
(require 'opal-toolbar)
(require 'opal-oasys)




;; opal syntax definition
(setq opal-syntax-special (concat "\\(?:[!#$%&\\*\\+\\-./:;<=>?"
				      "@\\\\~|^`¡¢£¤¥¦§¨©ª«¬"
				      "®¯°±²³´µ¶·¹¸º»¼½¾¿×÷]\\)"))


(setq opal-syntax-letgit "\\(?:[a-zA-Zßà-öø-ÿÀ-ÖØ-ÝÞ0-9]\\)")

(setq opal-syntax-extra "\\(?:[][\"\(\),']\\)")

(setq opal-syntax-alphanumA 
  (concat "\\(?:\\(?:" opal-syntax-letgit "\\|_\\)+\\?*\\)"))

(setq opal-syntax-alphGraph 
  (concat "\\(?:_\\(?:" opal-syntax-alphanumA 
	  "\\|" opal-syntax-special "+\\)\\)"))

(setq opal-syntax-alphanum 
  (concat "\\(?:"opal-syntax-alphanumA opal-syntax-alphGraph "*_*\\)"))

(setq opal-syntax-graphic 
  (concat "\\(?:\\(?:" opal-syntax-special "\\)+" opal-syntax-alphGraph "*_*\\)"))


(setq opal-syntax-ide 
  (concat "\\(?:"opal-syntax-alphanum "\\|" opal-syntax-graphic "\\)"))


(if opal-running-xemacs
 ;;     (progn
 ;; ;      (require 'opal-oasys)
 ;; ;      (require 'oasys-mode)
 ;;       (require 'opal-outline)
 ;;       (add-hook 'opal-mode-hook 'opal-outline-hook-functions)
 ;;       (if opal-pchecker
 ;; 	  (progn
 ;; 	    (require 'opal-trace-mode)
 ;; 	    (require 'opal-certify)
 ;; 	    (add-hook 'opal-mode-hook 'opal-certify-keymap)
 ;; 	    )
 ;; 	)
 ;;       (require 'opal-info)
 ;;       (add-hook 'opal-mode-hook 'opal-info-keymap)
 ;;       (message "XEmacs is not supported at the moment")
 ;;       )
(message "XEmacs is not supported at the moment")
)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opal-mode-set-nonexist-keymap ()
  "If keymap opal-mode-map not exists then set the keymap of the opal-mode."
  (interactive)
  (if opal-mode-map
      ()         ; Do not change the keymap if it is already set up
    (opal-mode-set-keymap)
    )
  )

(defun opal-mode-set-keymap ()
  "Set the keymap of the opal-mode."
  (interactive)
  (setq opal-mode-map (make-sparse-keymap))
  (if opal-running-xemacs
      nil
      (define-key opal-mode-map [menu-bar] (make-sparse-keymap))
  )
  (opal-mode-diag-keymap)
  (if opal-running-xemacs
      nil
      (define-key opal-mode-map [menu-bar opal] 
	(cons "OPAL" (make-sparse-keymap "OPAL")))
  )
  ;; reverse order of appearance in OPAL menu! (for FSF only)
  (opal-mode-misc-keymap)
  ;(if opal-running-xemacs (opal-outline-init))
  (opal-mode-oasys-keymap)
  (opal-mode-import-keymap)
  (opal-mode-switch-keymap)
  (opal-mode-filehandling-keymap)
  (opal-mode-compile-keymap)
  ;;(opal-mode-browser-keymap)
  (opal-mode-dosfop-keymap)

  (opal-mode-set-menu)

  (define-key opal-mode-map "\C-c\C-q\C-t" 'opal-mode-print-alist) ; debug
  (define-key opal-mode-map "\C-c\C-q\C-l" 'opal-mode-load) ; debug
  (define-key opal-mode-map "\C-c\C-^\C-t" 'opal-mode-print-alist) ; debug
  (define-key opal-mode-map "\C-c\C-^\C-l" 'opal-mode-load) ; debug
  (define-key opal-mode-map "\M-b" 'backward-opal-ide)
  (define-key opal-mode-map "\M-f" 'forward-opal-ide)
  )



(defun opal-mode-set-menu-fsfemacs ()
  (if opal-novice
	   (progn
	     (define-key opal-mode-map 
	       [menu-bar opal opal-switch-to-impl]
	       '("Switch to IMPLEMENTATION part" . opal-switch-to-impl))
	     (define-key opal-mode-map 
	       [menu-bar opal opal-switch-to-sign]
	       '("Switch to SIGNATURE part" . opal-switch-to-sign))
	     (define-key opal-mode-map 
	       [menu-bar opal opal-switch-to-all]
	       '("Show SIGNATURE and IMPLEMENTATION" . opal-switch-2))
	     (define-key opal-mode-map [menu-bar opal opal-opalfile-ask]
	       '("Save all Opal files" . opal-ask-save-opal-buffers))	     
	   )
       )
)
(defun opal-mode-set-menu ()
  "Set the menu spec. of the opal mode"
  (interactive)
  (if opal-running-xemacs
					; ------ XEmacs related
      (opal-mode-set-menu-xemaxs)
					; --- FSF Emacs related
      (opal-mode-set-menu-fsfemacs)
    )
)

(defun opal-mode-append-alist ()
  "Append .impl .sign .extp .intp .diag SysDefs ProjectDefs to auto-mode-alist."
  (interactive)
  (if opal-mode-alist
      ()
    (setq auto-mode-alist
	  (cons '("\\.sign$" . opal-mode) 
	  (cons '("\\.impl$" . opal-mode) 
  	  (cons '("\\.extp$" . opal-mode) 
	  (cons '("\\.intp$" . opal-mode) 
	  (cons '("\\.diag$" . opal-diag-mode)
	  (cons '("SysDefs\\|ProjectDefs" . opal-defs-mode)
		auto-mode-alist)))))))
  (setq opal-mode-alist 1)
  )
)

(defvar opal-font-lock-keywords-simple

  (list
   '("\\(IMPORT\\|SORT\\|FUN\\|TYPE\\|DATA\\|DEF\\|IMPLEMENTATION\\|SIGNATURE\\|EXTERNAL\\|INTERNAL\\|PROPERTIES\\|LAW\\|PROOF\\|SPC\\|PRE\\|POST\\|THEORY\\|ASSERT\\|ASSUME\\|PROP\\|JSTF\\|JUSTF\\|LEMMA\\|GOAL\\)" (0 'font-lock-function-name-face t t))
   '("\\<ALL\\>\\|\\<AND\\>\\|\\<ANDIF\\>\\|\\<AS\\>\\|\\<COMPLETELY\\>\\|\\<DFD\\>\\|\\<ELSE\\>\\|\\<EX\\>\\|\\<FI\\>\\|\\<IF\\>\\|\\<IN\\>\\|\\<LET\\>\\|\\<NOT\\>\\|\\<ONLY\\>\\|\\<ORIF\\>\\|\\<OR\\>\\|\\<OTHERWISE\\>\\|\\<THEN\\>\\|\\<WHERE\\>\\|\\*\\*\\|->\\|\\<\\.\\>\\|:\\|_\\|==[=>?]\\|<<=\\|<=>\\|\\\\\\\\\\||-\\>" (0 'font-lock-keyword-face nil t))
;   '("/\\* %.*\\*/" (0 'font-lock-doc-string-face nil t))
;   '("/\\* [^%][^\\$].*\\*/" (0 'font-lock-comment-face t t))
;   '("-- %.*$" (0 'font-lock-doc-face t t))
   '("-- .*$" (0 'font-lock-comment-face t t))
   '("/\\$.*\\$/" (0 'font-lock-preprocessor-face t t))
   `("\\(CERTIFICATION\\|FORMALTEST\\|FORMALPROOF\\|SYNTHESIS\\)" (0 'font-lock-reference-face t t))
   )
)


(defvar opal-font-lock-keywords-extended
  (list
   ; comments
   ;; (list (concat
   ;; 	  "\\(^\\|[^-!#$%&*+./:;\<=>?@\\^_`{|}~]\\)"
   ;; 	  "\\(/\\*[ \t]%.*\\($.*\\)*\\*\/\\)")
   ;; 	 '(2 'font-lock-doc-face t t))
   (list (concat
	  "\\(^\\|[^-!#$%&*+./:;<=>?@\\^_`{|}~]\\)"
	  "\\(--\\( .*\\)?$\\)")
	 '(2 'font-lock-comment-face t t))
   ;; (list (concat
   ;; 	  "\\(^\\|[^-!#$%&*+./:;\<=>?@\\^_`{|}~]\\)"
   ;; 	  "\\(-- %.*$\\)")
   ;; 	 '(2 'font-lock-doc-face t t))
   ;; (list (concat
   ;; 	  "\\(^\\|[^-!#$%&*+./:;\<=>?@\\^_`{|}~]\\)"
   ;; 	  "\\(/\\$.*\\$/\\)")
   ;; 	 '(2 'font-lock-preprocessor-face t t))
   ; alphanumerical keywords
   (list (concat
	  "\\(^\\|[^0-9a-zA-Z_]\\)"
	  "\\("
	  "ALL\\|AND\\|ANDIF\\|AS\\|COMPLETELY\\|DFD\\|ELSE\\|EX\\|"
	  "FI\\|IF\\|IN\\|LET\\|NOT\\|ONLY\\|ORIF\\|OR\\|OTHERWISE\\|"
	  "THEN\\|WHERE\\|FUN\\|DEF\\|SORT\\|TYPE\\|DATA\\|IMPORT"
	  "\\)"
	  "\\($\\|[^0-9a-zA-Z_]\\)"
	  )
	 '(2 'font-lock-keyword-face nil t))
   ; graphical keywords
   (list (concat
	  "\\(?:^\\|[^-!#$%&*+./:;<=>?@\\^_`{|}~]\\)"
	  "\\("
	  "\\*\\*\\|->\\|\\.\\|:\\|==\\|===\\|<<="
	  "\\|==>\\|<=>\\|\\\\\\\\"
	  "\\)"
	  "\\(?:$\\|[^-!#$%&*+./:;<=>?@\\^_`{|}~]\\)")
	 '(1 'font-lock-builtin-face nil t))
   ; underscore
   (list (concat
	  "\\(^\\|[ \t\(,]\\)"
	  "\\(_\\)\\([ \t\),]\\|$\\)")
	 '(2 'font-lock-keyword-face nil t))
   ; file identifying keywords
   (list (concat
	  "^\\("
	  "SIGNATURE\\|IMPLEMENTATION\\|"
	  "\\(?:EXTERNAL\\|INTERNAL\\)[ \t]+PROPERTIES"
	  "\\)\\(?:[ \t]+\\)"
	  "\\("
	  opal-syntax-ide
	  "\\)"
	  )
	 '(1 'font-lock-keyword-face nil t)
	 '(2 'font-lock-variable-name-face nil t))
   ; imports
   (list (concat
	  "^\\(\\(IMPORT\\)?\\)[ \t]+"
	  "\\("
	  "_*\\([0-9a-zA-Z]+\\?*\\|[-!#$%&*+./:;<=>?@\\^`{|}~]+\\)?"
	  "\\("
	  "_+\\([0-9a-zA-Z]+\\?*\\|[-!#$%&*+./:;<=>?@\\^`{|}~]+\\)?"
	  "\\)*"
	  "\\)"
	  "[ \t]*\\(\\(\\[[^ \t]*\\]\\)?\\)[ \t]+"
	  "\\(ONLY\\|COMPLETELY\\)")
	 '(1 'font-lock-keyword-face nil t)
	 '(3 'font-lock-variable-name-face nil t)
	 '(7 'font-lock-type-face nil t)
	 '(9 'font-lock-keyword-face nil t))
   ; Numbers
   (list (concat "\\(?:^\\|" opal-syntax-special "\\|" opal-syntax-extra "\\|[ \t]+" "\\)"
		 "\\([0-9]+\\)" 
		 "\\(?:$\\|" opal-syntax-special "\\|" opal-syntax-extra "\\|[ \t]+" "\\)" 
	  )
	 '(1 'font-lock-constant-face nil t)
	 )
   ; Identifiers
   (list (concat "\\(" opal-syntax-alphanum "\\)")
	 '(0 'default nil t)
	 )
  ; Operators
  (list (concat "\\(" opal-syntax-special "\\)+")
	'(0 'font-lock-variable-name-face nil t)
	)
  
  )
  )

















(defun opal-init-file ()
  "initializes empty structure part."
  (interactive)
  (cond ((opal-in-sign)
         (insert (concat "SIGNATURE " (opal-buffer-to-structure)))
         (set-buffer-modified-p nil))
        ((opal-in-impl)
         (insert (concat "IMPLEMENTATION " (opal-buffer-to-structure)))
         (set-buffer-modified-p nil))
        ((opal-in-extp)
         (insert (concat "EXTERNAL PROPERTIES " (opal-buffer-to-structure)))
         (set-buffer-modified-p nil))
        ((opal-in-intp)
         (insert (concat "INTERNAL PROPERTIES " (opal-buffer-to-structure)))
         (set-buffer-modified-p nil))
  )
)

(defun opal-add-buffer-to-load-path ()
  "add path of current buffer to opal-load-path, if it is a opal buffer"
  (if (equal major-mode 'opal-mode)
      (opal-add-load-path (file-name-directory (buffer-file-name)))
    )
  nil
)

(defun opal-buffer-to-structure ()
   (string-match "\\([^.]*\\)\\..*" (buffer-name))
   (substring (buffer-name) (match-beginning 1) (match-end 1))
)

(defun opal-in-sign () (string-match "\\([^.]*\\).sign$" (buffer-name)))
(defun opal-in-impl () (string-match "\\([^.]*\\).impl$" (buffer-name)))
(defun opal-in-extp () (string-match "\\([^.]*\\).extp$" (buffer-name)))
(defun opal-in-intp () (string-match "\\([^.]*\\).intp$" (buffer-name)))

(defun opal-mode-auto-fill ()
  "function to perform autofill in opal-mode"
  (interactive)

  (let ((doit t))
    (if (> (current-column) fill-column)
	(save-excursion
	  (beginning-of-line)
	  (if (looking-at "\\(.*\\)-- ")
	      (progn
		(replace-match "\\1/* ")
		)
	    )
	  )
      )
    (save-excursion
      (beginning-of-line)
      (setq doit (not (looking-at ".*\\<ONLY\\>")))
      )
    (if doit
      (do-auto-fill)  ;; do the standard thing
      )
    )
)
	  
(defun opal-mode-indent-init ()
  "initialize indenting for opal-mode"
  (interactive)

  (require 'opal-indent)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'opal-mode-indent)
)

(defun opal-ask-save-opal-buffers ()
  "offer save for every modified opal buffer"
  (interactive)
  (mapcar (function opal-ask-save-opal-buffer) (buffer-list))
)

(defun opal-ask-save-opal-buffer (buffer &optional dontask)
  "if buffer is opal buffer (i.e. in opal-mode) and modified, offer to save it"
  (let ((blv (buffer-local-variables buffer))
       )
    (if (or
	 (equal (assoc 'major-mode blv) '(major-mode . opal-mode))
	 (equal (assoc 'major-mode blv) '(major-mode . opal-defs-mode)))
	(if (and (buffer-modified-p buffer) (buffer-file-name buffer))
	    (if (or dontask
		    (y-or-n-p (concat "Save OPAL unit " 
				      (file-name-nondirectory 
				       (buffer-file-name buffer))
				      "? "))
		    )
		(save-excursion
		  (set-buffer buffer)
		  (save-buffer)
		)
	    )
	)
    )
  )
)
	      

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; initially functions
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(opal-mode-set-keymap)           ; overwrite old keymap
;(opal-mode-set-menu)
(opal-mode-append-alist)
;; set up font-lock-mode
(add-hook 'find-file-not-found-hooks 'opal-init-file)
(add-hook 'find-file-not-found-hooks 'opal-add-buffer-to-load-path)
(add-hook 'find-file-hooks 'opal-add-buffer-to-load-path)
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the OPAL-MODE
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opal-mode ()
  "Major mode for editing OPAL code.
\\[backward-opal-ide] = 'backward-opal-ide
	Move point backward one identifier
\\[forward-opal-ide] = 'forward-opal-ide
	Move point forward one identifier

\\[browser-identify-function] = 'browser-identify-function
	Report about the pointed object
\\[browser-identify-function-circa] = 'browser-identify-function-circa
	Report about the objects near point
\\[browser-identify-function-by-name] = 'browser-identify-function-by-name
	Report about the pointed name
\\[browser-show-imports] = 'browser-show-imports
	Generate full import-list
\\[browser-report-import] = 'browser-report-import
	Report about the imported objects of the pointed structure
\\[browser-list-applications] = 'browser-list-applications
	Report about all applied objects of the current structure
\\[browser-list-objects] = 'browser-list-objects
	Report about the imported and defined objects
\\[browser-help] = 'browser-help
	Show the help-message of the browser

\\[browser-level-complete] = 'browser-level-complete
	Set the level to complete
\\[browser-level-names] = 'browser-level-names
	Set the level to names
\\[browser-level-inst] = 'browser-level-inst
	Set the level to inst
\\[browser-level-optimize] = 'browser-level-optimize
	Set the level to optimize
\\[browser-level-fct] = 'browser-level-fct
	Set the level to fct
\\[browser-show-level] = 'browser-show-level
	Show the actual level



M-c   \\[opal-compile-call] = 'opal-compile-call
	Start compiling of project.
\\[opal-compile-call-ask] = 'opal-compile-call-ask
	Start compiling with asking for project.
\\[opal-compile-project-set] = 'opal-compile-project-set
	Set the Variable opal-compile-project.
\\[opal-compile-help] = 'opal-compile-help
	Show help-message of ocs.
\\[opal-compile-clean] = 'opal-compile-clean
	Cleaning the OCS directory.
\\[opal-compile-delete-frame] = 'opal-compile-delete-frame
	Delete the opal-compile frame and the opal-compile buffer.
\\[opal-compile-kill] = 'opal-compile-kill
	Kill the opal compile process.



\\[opal-opalfile-error] = 'opal-opalfile-error
	Load the error file and the corresponding diagnostic file.
\\[opal-opalfile-all] = 'opal-opalfile-all
	Load the implementation, signature, ext. properties and int. properties of the pointed structure.
\\[opal-opalfile-sign] = 'opal-opalfile-sign
	Load the signature of the pointed structure.
\\[opal-opalfile-impl] = 'opal-opalfile-impl
	Load the implementation of the pointed structure.
\\[opal-opalfile-intp] = 'opal-opalfile-intp
	Load the internal properties of the pointed structure.
\\[opal-opalfile-extp] = 'opal-opalfile-extp
	Load the external properties of the pointed structure.



\\[diag-next-main-error] = 'diag-next-main-error
	Visit next compilation error and corresponding source code, skipping sub errors.
\\[diag-prev-main-error] = 'diag-prev-main-error
	Visit previous compilation error and corresponding source code, skipping sub errors.
\\[diag-update] = diag-update'
	Update diagnostics buffer.
\\[diag-show] = 'diag-show
	Show current error.
\\[diag-kill-error] = 'diag-kill-error
	Kill current error.
\\[diag-next-error] = 'diag-next-error
	Visit next compilation error and corresponding source code.
\\[diag-prev-error] = 'diag-prev-error
	Visit previous compilation error and corresponding source code.
\\[diag-show-errors] = 'diag-show-errors
	Show only errors.
\\[diag-show-errors-and-warns] = 'diag-show-errors-and-warns
	Show only errors and warnings.
\\[diag-show-all] = 'diag-show-all
	Show all diagnostics.


Special commands:
\\{opal-mode-map}

Turning on opal-mode runs the hook 'opal-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (opal-mode-set-nonexist-keymap)
  (use-local-map opal-mode-map)         ; This provides the local keymap
  (setq mode-name "Opal(v4)")
  (setq major-mode 'opal-mode)
;  (setq indent-tabs-mode nil)  ;; no TABs
  (line-number-mode 1)  ;; show line numbers
;  Syntax-Entries for comments
  (modify-syntax-entry ?/ "w-14")
  (modify-syntax-entry ?* "w-23")
  (setq comment-start "/* ")
  (setq comment-end " */")
;  (auto-fill-mode)
;  (setq auto-fill-function 'opal-mode-auto-fill)
  (setq opal-compile-projectdefsfile (getenv "OCSPROJECT"))
  (opal-toolbar-install)
  (if opal-running-xemacs
      nil
    (opal-misc-hilit-all)
  )
  (setq selective-display t)
  (add-hook 'first-change-hook 'opal-toolbar-mark-change)
  (add-hook 'after-save-hook 'opal-toolbar-save-necessary)
  (run-hooks 'opal-mode-hook)
  (run-hooks 'opal-mode-hooks)  ; for backwards compatibility
  )


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Miscallaneous menu

(defun opal-mode-misc-keymap ()
  "add misc menu to opal mode"
  (interactive)

  (if opal-running-xemacs
      (opal-misc-menu-xemacs)
      (opal-misc-menu-fsfemacs)
  )
)



(defun opal-misc-menu-fsfemacs ()
  "opal-mode misc menu for FSF emacs"

  (interactive)
  (if opal-novice
      ();
    (define-key opal-mode-map [menu-bar opal misc]
      (cons "Misc" (make-sparse-keymap "Misc")))
    (define-key opal-mode-map [menu-bar opal misc opal-misc-indent]
      '("Standard Indentation" . opal-misc-indent ))
    (define-key opal-mode-map [menu-bar opal misc opal-misc-indent-on]
      '("Opal Indentation" . opal-misc-indent-on ))
    (define-key opal-mode-map [menu-bar opal misc opal-misc-hilit-all]
      '("Highlight" . opal-misc-hilit-all))

    (put 'opal-misc-indent-on 'menu-enable '(not opal-indent-flag))
    (put 'opal-misc-indent  'menu-enable 'opal-indent-flag)
    )
)

(defun opal-misc-std-indent-q ()
  (interactive)
  (not opal-indent-flag)
)

(defun opal-misc-opal-indent-q ()
  (interactive)
  opal-indent-flag
)

(defun opal-misc-hilit-all ()
  (interactive)
  ;;(set (make-local-variable 'font-lock-defaults) '(opal-font-lock-keywords-simple))
  (set (make-local-variable 'font-lock-defaults) '(opal-font-lock-keywords-extended))
)

(defvar opal-indent-flag nil
  "nil, iff opal-indentation is off; old indent-line-function otherwise. Do not set this variable directly, use function opal-misc-indent for this."
)

(defun opal-misc-indent ()
  (interactive)
  (if opal-indent-flag
      (progn
	(setq indent-line-function opal-indent-flag)
	(setq opal-indent-flag nil)
	)
    (progn
      (setq opal-indent-flag indent-line-function)
      (opal-mode-indent-init)
      )
    )
  )
      
(defun opal-misc-indent-on ()
  "switch on opal-identation"
  (interactive)
  (if opal-indent-flag
      (opal-mode-indent-init)
    (progn
      (setq opal-indent-flag indent-line-function)
      (opal-mode-indent-init)
      )
    )
  )
 

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; only for test and debug
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opal-mode-load ()
  "Loads the opal-mode File."
  (interactive)
  (load "opal-mode")
  )

(defun opal-mode-print-alist ()
  "."
  (interactive)
  (print auto-mode-alist (get-buffer "*scratch*"))
  )





;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; XEMACS
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opal-misc-menu-xemacs ()
  "opal-mode misc menu for Xemacs"

  (interactive)
  (setq opal-misc-menu
	(list "Misc"
	      ["Font lock simple" opal-misc-font-lock-simple font-lock-mode]
	      ["Font lock extended" opal-misc-font-lock-extended 
	       font-lock-mode]
	      ["Opal Indentation" opal-misc-indent :active t 
	       :style toggle :selected opal-indent-flag]
	      ["Install toolbar" opal-toolbar-install t]
	      )
	)
)

(defun opal-mode-set-menu-xemacs () 
  (progn
	(defun opal-add-menus ()
	  ""
	  (set-buffer-menubar (copy-sequence current-menubar))
	  (if opal-novice
	      (progn
		(add-submenu nil
			     (list "Opal"
				   ["Save all Opal files" 
				    opal-ask-save-opal-buffers t]
				   ["Show SIGNATURE and IMPLEMENTATION" 
				    opal-switch-2 t]
				   ["Switch to SIGNATURE part"  
				    opal-switch-to-sign 
				    :style radio :selected (opal-in-sign)]
				   ["Switch to IMPLEMENTATION part"
				    opal-switch-to-impl 
				    :style radio :selected (opal-in-impl)]
			      )
		  )
		)
	    ; else
	    (add-submenu nil (list "OPAL"))
	    (add-submenu (list "OPAL") opal-opalfile-menu)
	    (add-submenu (list "OPAL") opal-switch-menu)
	    (add-submenu (list "OPAL") opal-import-menu)
	    (add-submenu (list "OPAL") opal-compile-menu)
	    (add-submenu (list "OPAL") opal-dosfop-menu)
	    ;;(add-submenu (list "OPAL") opal-browser-menu)
	    (add-submenu (list "OPAL") opal-oasys-menu) 
	    (add-submenu (list "OPAL") opal-outline-menu)
	    (if opal-pchecker
		(add-submenu (list "OPAL") opal-certify-menu)
	      )
	    (add-submenu (list "OPAL") opal-misc-menu)
	    )
	  (add-submenu nil opal-diag-menu)
	  )
	(add-hook 'opal-mode-hook 'opal-add-menus)
	)
)




;; Local Variables:
;; coding: latin-1-unix
;; End:
