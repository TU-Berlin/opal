;; emacs-el-file for browsing opal files
;; $_Header$

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; necessary files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq browser-el "opal-browser.el")
(setq browser-exe "browser")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local key-bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opal-browser-menu-xemacs ()
  "Set opal-mode browser menu for XEmacs"
  (setq opal-browser-menu 
	(list "Browser"
	      ["Show Functionality by name" 
	       browser-identify-function-by-name t]
	      ["Show Functionality by position" 
	       browser-identify-function-circa t]	     
	      ["Generate Importlist"  browser-show-imports t]
	      "---"
	      ["Help"  browser-help t]
	      "---"
;		  ["Show Displaylevel" browser-show-level t]
	      ["Set Displaylevel To Optimize" browser-level-optimize  
	       :style radio :selected (null (browser-level-optimize-enable))]
	      ["Set Displaylevel To Names"  browser-level-names  
	       :style radio :selected (null (browser-level-names-enable))]
	      ["Set Displaylevel To Inst"  browser-level-inst  
	       :style radio :selected (null (browser-level-inst-enable))]
	      ["Set Displaylevel To Fct" browser-level-fct  
	       :style radio :selected (null (browser-level-fct-enable))]
	      ["Set Displaylevel To Complete" browser-level-complete 
	       :style radio :selected (null (browser-level-complete-enable))]

	      ))
)

(defun opal-browser-menu-fsfemacs ()
  "Set opal-mode browser menu for FSF Emacs"
  (interactive)

  (if (not opal-novice)
      (progn
	(define-key opal-mode-map [menu-bar opal browser]
	  (cons "Browser" (make-sparse-keymap "Browser")))
	
	(define-key opal-mode-map [menu-bar opal browser browser-level-complete]
	  '("Set Displaylevel To Complete" . browser-level-complete))
	(define-key opal-mode-map [menu-bar opal browser browser-level-fct]
	  '("Set Displaylevel To Fct" . browser-level-fct))
	(define-key opal-mode-map [menu-bar opal browser browser-level-inst]
	  '("Set Displaylevel To Inst" . browser-level-inst))
	(define-key opal-mode-map [menu-bar opal browser browser-level-names]
	  '("Set Displaylevel To Names" . browser-level-names))
	(define-key opal-mode-map [menu-bar opal browser browser-level-optimize]
	  '("Set Displaylevel To Optimize" . browser-level-optimize))
					;  (define-key opal-mode-map [menu-bar opal browser browser-show-level]
					;    '("Show Displaylevel" . browser-show-level))
	(define-key opal-mode-map [menu-bar opal browser t1]
	  '("" . nil))
	(define-key opal-mode-map [menu-bar opal browser browser-help]
	  '("Help" . browser-help))
	(define-key opal-mode-map [menu-bar opal browser t2]
	  '("" . nil))
	(define-key opal-mode-map [menu-bar opal browser browser-show-imports]
	  '("Generate Importlist" . browser-show-imports))
	(define-key opal-mode-map [menu-bar opal browser browser-identify-function]
	  '("Show Functionality by position" . 
	    browser-identify-function-circa))
	(define-key opal-mode-map [menu-bar opal browser 
					    browser-identify-function-by-name]
	  '("Show Functionality by name" . 
	    browser-identify-function-by-name))
      )
  )
)


(defun opal-mode-browser-keymap ()
  "Set the opal-mode browser keymap."
;;shortkeys
  (define-key opal-mode-map "\M-i" 'browser-show-imports)
  (define-key opal-mode-map "\M-z" 'browser-identify-function)
  (define-key opal-mode-map "\M-\C-z" 'browser-identify-function-by-name)
;;longkeys
  (define-key opal-mode-map "\C-c\C-b\C-t" 'browser-paramtest)
  (define-key opal-mode-map "\C-c\C-b\C-d" 'browser-dump)
  (define-key opal-mode-map "\C-c\C-b\C-h" 'browser-help)
  (define-key opal-mode-map "\C-c\C-b\C-l" 'browser-load)

  (define-key opal-mode-map "\C-c\C-bi" 'browser-report-import)
  (define-key opal-mode-map "\C-c\C-b\C-i" 'browser-show-imports)
  (define-key opal-mode-map "\C-c\C-b\C-a" 'browser-list-applications)
  (define-key opal-mode-map "\C-c\C-b\C-p" 'browser-identify-function)
  (define-key opal-mode-map "\C-c\C-bp" 'browser-identify-function-circa)
  (define-key opal-mode-map "\C-c\C-b\C-n" 'browser-identify-function-by-name)
  (define-key opal-mode-map "\C-c\C-b\C-o" 'browser-list-objects)

  (define-key opal-mode-map "\C-c\C-b\C-vc" 'browser-level-complete)
  (define-key opal-mode-map "\C-c\C-b\C-vn" 'browser-level-names)
  (define-key opal-mode-map "\C-c\C-b\C-vi" 'browser-level-inst)
  (define-key opal-mode-map "\C-c\C-b\C-vo" 'browser-level-optimize)
  (define-key opal-mode-map "\C-c\C-b\C-vf" 'browser-level-fct)
  (define-key opal-mode-map "\C-c\C-b\C-vs" 'browser-show-level)

  (if opal-running-xemacs
      (opal-browser-menu-xemacs)
      (opal-browser-menu-fsfemacs)
  )

  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun browser-test-interOpal ()
  "Check the existence of the interOpal file."
  (let* ((fn (file-name-nondirectory buffer-file-name))
	(interfn (concat (concat "OCS/" fn) ".inter")))
    (if (and fn
	     (string-match ".*\\.\\(sign\\|impl\\|extp\\|intp\\)$" fn))
	(if (file-readable-p interfn)
	    t
;	  (princ (concat "No corresponding diagnostics " interfn))
	  nil
	  )
;      (princ "No Opal file.")
      nil
;      (error "No Opal file.")
      )))

(defun browser-level-optimize-enable ()
  (if (string= browse-level "optimize") nil t))
(defun browser-level-fct-enable ()
  (if (string= browse-level "fct") nil t))
(defun browser-level-inst-enable ()
  (if (string= browse-level "inst") nil t))
(defun browser-level-names-enable ()
  (if (string= browse-level "names") nil t))
(defun browser-level-complete-enable ()
  (if (string= browse-level "complete") nil t))



(put 'browser-identify-function-by-name 'menu-enable '(browser-test-interOpal))
(put 'browser-identify-function-circa 'menu-enable '(browser-test-interOpal))
(put 'browser-show-imports 'menu-enable '(browser-test-interOpal))
;(put ' 'menu-enable '())
(put 'browser-level-complete 'menu-enable '(browser-level-complete-enable))
(put 'browser-level-fct 'menu-enable '(browser-level-fct-enable))
(put 'browser-level-inst 'menu-enable '(browser-level-inst-enable))
(put 'browser-level-names 'menu-enable '(browser-level-names-enable))
(put 'browser-level-optimize 'menu-enable '(browser-level-optimize-enable))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; globale Variablen -------- buffer-local machen??????????
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq browse-level "optimize")
(setq browser-use-filename nil)
(setq browser-use-position nil)
(setq browser-use-option1 nil)
(setq browser-use-option2 nil)
(setq browser-use-option3 nil)
(setq browser-use-option4 nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; browser functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun browser-help ()
  "Show the help-text."
  (interactive)
  (setq com-nr "1")
  (browser-browse)
  )

(defun browser-dump ()
  "dump the interOpal-file."
  (interactive)
  (setq com-nr "10")
  (setq browser-use-filename t)
  (setq browser-use-option1 t)
  (setq browser-option1 "v1")
  (setq browser-use-option2 t)
  (setq browser-option2 "-zprintNumbers")
  (browser-browse)
  )

(defun browser-identify-function ()
  "Reports about the functionality of the pointed function."
  (interactive)
  (setq com-nr "11")
  (setq browser-use-filename t)
  (setq browser-use-position t)
  (setq browser-use-option1 t)
  (setq browser-option1 "v1")
  (browser-browse)
  )

(defun browser-identify-function-circa ()
  "Reports about the functionality of the pointed function."
  (interactive)
  (setq com-nr "11")
  (setq browser-use-filename t)
  (setq browser-use-position t)
  (setq browser-use-option1 t)
  (setq browser-option1 "v1")
  (setq browser-use-option2 t)
  (setq browser-option2 "ca")
  (browser-browse)
  )

(defun browser-report-import ()
  "Reports about the necessary IMPORT's of the pointed structure."
  (interactive)
  (setq com-nr "12")
  (setq browser-use-filename t)
  (setq browser-use-position t)
  (setq browser-use-option1 t)
  (setq browser-option1 "v1")
  (setq browser-use-option2 t)
  (setq browser-option2 "allpos")
  (setq browser-use-option3 t)
  (setq browser-option3 "chkUniqueImportInst")
  (browser-browse)
  )

(defun browser-show-imports ()
  "show all IMPORT's of the current structure."
  (interactive)
  (setq com-nr "13")
  (setq browser-use-filename t)
  (setq browser-use-option1 t)
  (setq browser-option1 "v1")
  (setq browser-use-option2 t)
  (setq browser-option2 "allpos")
  (setq browser-use-option3 t)
  (setq browser-option3 "chkUniqueImportInst")
  (browser-browse)
  )

(defun browser-list-objects ()
  "list all objects of all structures."
  (interactive)
  (setq com-nr "14")
  (setq browser-use-filename t)
  (setq browser-use-option1 t)
  (setq browser-option1 "v1")
  (setq browser-use-option2 t)
  (setq browser-option2 "allpos")
  (browser-browse)
  )

(defun browser-list-applications ()
  "List all Applications of functions."
  (interactive)
  (setq com-nr "15")
  (setq browser-use-filename t)
  (setq browser-use-option1 t)
  (setq browser-option1 "v1")
  (setq browser-use-option2 t)
  (setq browser-option2 "allpos")
  (setq browser-use-option3 t)
  (setq browser-option3 "showType")
  (browser-browse)
  )

(defun browser-identify-function-by-name ()
  "Reports about the pointed name."
  (interactive)
  (setq com-nr "11")
  (setq browser-use-filename t)
  (setq browser-use-position t)
  (setq browser-use-option1 t)
  (setq browser-option1 "v1")
  (setq browser-use-option2 t)
  (setq browser-option2 "nameNotPos")
  (browser-browse)
  )

(defun browser-paramtest ()
  "testfunctions of the browser."
  (interactive)
  (setq com-nr "0")
  (setq browser-use-filename t)
  (setq browser-use-position t)
  (setq browser-use-option1 t)
  (setq browser-option1 "v3")
  (browser-browse)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hauptfunktion browser-browse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun browser-browse ()
;; test Positionsübergabe
;; Point darf nicht auf dem letzten Zeichen stehen!!
  "main function of the browser."
  (if browser-use-position (set-browser-position-and-name))
					; set ide-name and browser-pos-...
  ;;    (princ "browsing ..." t)
  (start-process "browsing" "*browse*"
		 browser-exe
		 "Command" com-nr
		 "Level" browse-level
		 (cond (browser-use-filename "Filename") (t "Option"))
		 (cond (browser-use-filename (set-browser-filename))(t "n"))
		 (cond (browser-use-position "Name") (t "Option"))
		 (cond (browser-use-position ide-name) (t "n"))
		 (cond (browser-use-position "Begin") (t "Option"))
		 (cond (browser-use-position browser-pos-begin-line)
		       (t "n"))
		 (cond (browser-use-position browser-pos-begin-col)
		       (t "Option"))
		 (cond (browser-use-position "End") (t "n"))
		 (cond (browser-use-position browser-pos-end-line)
		       (t "Option"))
		 (cond (browser-use-position browser-pos-end-col)
		       (t "n"))
		 "Option"
		 (cond (browser-use-option1 browser-option1) (t "n"))
		 "Option"
		 (cond (browser-use-option2 browser-option2) (t "n"))
		 "Option"
		 (cond (browser-use-option3 browser-option3) (t "n"))
		 "Option"
		 (cond (browser-use-option4 browser-option4) (t "n"))
		 )
  (save-window-excursion
    (with-output-to-temp-buffer "*browse*"
      (switch-to-buffer "*browse*")
      (toggle-read-only)))
;;  (opal-open-window "*browse*")
  (pop-to-buffer "*browse*")
  (setq browser-use-filename nil)
  (setq browser-use-position nil)
  (setq browser-use-option1 nil)
  (setq browser-use-option2 nil)
  (setq browser-use-option3 nil)
  (setq browser-use-option4 nil)
  (browser-show-level)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun browser-load ()
  "load browser.el ."
  (interactive)
  (load browser-el t t )
  (browser-show-level)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setzen von globalen Variablen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun browser-level-complete ()
  "Set the browse-level to complete."
  (interactive)
  (setq browse-level "complete")
  (browser-show-level))

(defun browser-level-names ()
  "Set the browse-level to names."
  (interactive)
  (setq browse-level "names")
  (browser-show-level))

(defun browser-level-inst ()
  "Set the browse-level to inst."
  (interactive)
  (setq browse-level "inst")
  (browser-show-level))

(defun browser-level-fct ()
  "Set the browse-level to fct."
  (interactive)
  (setq browse-level "fct")
  (browser-show-level))

(defun browser-level-optimize ()
  "Set the browse-level to optimize."
  (interactive)
  (setq browse-level "optimize")
  (browser-show-level))

(defun browser-show-level ()
  "Show the actual browse-level in the minibuffer."
  (interactive)
  (princ "actual browse-level: " t)
  (princ browse-level t))

(defun set-browser-position-and-name ()
  "Set the variables browser-pos-... and browser-ide-name."
  (save-excursion
;    (opal-end-ide)
    (begin-opal-ide)
    (setq ide-begin (point))
    (setq browser-pos-begin-line (get-line))
    (setq browser-pos-begin-col (get-col))
;    (opal-begin-ide)
    (end-opal-ide)
    (setq ide-end (point))
    (setq browser-pos-end-line (get-line))
    (setq browser-pos-end-col (get-col))
    (setq ide-name (if (> ide-end (point-max))
		       ""
		     (buffer-substring ide-begin ide-end))
	  )
    )
  )


(defun get-line ()
  "line of point."
  (let ((line (what-line)))
    (substring line 5 (length line)))
  )

(defun get-col ()
  "column of point."
  (let ((col (what-cursor-position))
	(start -1))
    (get-col-start start col))
  )

(defun get-col-start (Start Text)
  "column of point."
  (cond ((string= (substring Text (- Start 1) Start) " ")
	 (substring Text Start -1))
	(t (get-col-start (- Start 1) Text)))
  )


(defun set-browser-filename ()
  "Return the browser-filename. ** ist set by error and browser-filename is set to nil."
  (cond ((eq nil (buffer-file-name)) "")
	(t (buffer-file-name)))
  )

(defun opal-get-frame-p(list String)
  (cond ((null list) '(nil))
	((string= String (get-frame-name (frame-parameters (car list))))
	 (cons t (car list)))
	(t (opal-get-frame-p(cdr list) String)))
  )

(defun get-frame-name(list)
  "Arg: List of frame's properties, Ret: frame's name"
    (cond ((null list) 'nil)
	  ((string= "name" (car (car list))) (cdr(car list)))
	  (t (get-frame-name(cdr list))))
  )

(provide 'opal-browser)
