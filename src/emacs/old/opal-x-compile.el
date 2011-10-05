;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Opal Mode
;;; Compile Part
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This version has been designed to work with XEmacs
;;; and may not work with other Emacsen. Use the standard opal-compile.el
;;; in this case.


;; use -w2 so that all diagnostics are printed in the buffer
(setq opal-compile-project "ocs -w2" )

(defvar opal-compile-errorlevel nil
  "nil or 0: no errors; 1: hints, 2: warnings, 3: error")

(defvar opal-compile-show-messages
  "^\\(Generating\\|Checking\\|Archiving\\)"
  "*regexp to determine which compiler messages are to be displayed")

(defvar opal-compile-process-buffer "*opal-compile-output*"
  "name of buffer to display ocs output / 1")

(defvar opal-compile-diag-buffer "*opal-compile-diagnostics*"
  "name of buffer to display ocs output / 2")

(defvar opal-compile-silent nil)

(defvar opal-compile-projectdefsfile nil
  "name of current opal projectdefs file (initialised from OCSPROJECT variable)")

(defun opal-mode-compile-xemacs-menu ()
  "set up opal-mode compile menu for XEmacs"
  (setq opal-compile-menu 
	(list "Compile"
	      ["Compile current project"  opal-compile-call t]
	      ["Compile project ..." opal-compile-call-ask t]
	      ["Set project ..." opal-compile-project-set t]
	      "---"
	      ["ocs help"  opal-compile-help t]
	      ["ocs clean"  opal-compile-clean t]
	      "---"
	      ["Show ocs output" opal-compile-view t]
	      ["Clear ocs output" opal-compile-clear t]
	      ["Kill compiling process" opal-compile-kill (opal-compile-running)]
	      "---"
	      ["Load SysDefs" opal-compile-sysdefs t]
	      ["Load ProjectDefs" opal-compile-projectdefs 
	       opal-compile-projectdefsfile]
	      ["Set ProjectDefs ..." opal-compile-set-projectdefs t]
	      ))
)


(defun opal-mode-compile-keymap ()
  "Set the opal-mode compile keymap."
;; Tastenbelegungen von Compile
;;shortkeys
;  (define-key opal-mode-map "\M-c" 'opal-compile-call)
  (define-key opal-mode-map "\M-a" 'opal-compile-call-ask)
  (define-key opal-mode-map "\M-s" 'opal-compile-project-set)
;;longkeys
  (define-key opal-mode-map "\C-cc" 'opal-compile-call)
  (define-key opal-mode-map "\C-c\C-c\C-a" 'opal-compile-call-ask)
  (define-key opal-mode-map "\C-c\C-c\C-s" 'opal-compile-project-set)
  (define-key opal-mode-map "\C-c\C-c\C-h" 'opal-compile-help)
  (define-key opal-mode-map "\C-c\C-c\C-c" 'opal-compile-clean)
  (define-key opal-mode-map "\C-c\C-c\C-k" 'opal-compile-kill)
  (define-key opal-mode-map "\C-c\C-c\C-v" 'opal-compile-view)
  (define-key opal-mode-map "\C-c\C-cs" 'opal-compile-sysdefs)
  (define-key opal-mode-map "\C-c\C-cp" 'opal-compile-projectdefs)
  (define-key opal-mode-map "\C-c\C-c\C-p" 'opal-compile-set-projectdefs)
  (define-key opal-mode-map "\C-c\C-c0" 'opal-compile-clear)

;; Menuepunkt Compile:
  (opal-mode-compile-xemacs-menu)
  )
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opal-compile-running()
  (if (get-process "compiling") t nil))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opal-compile-kill ()
  "Kill the opal compile process."
  (interactive)
  (cond ((get-process "compiling") (delete-process "compiling"))
	(t (princ "No compile process exists"))
	)
  )

(defun opal-compile-project-set ()
  "Set the Variable opal-compile-project."
  (interactive)
  (setq opal-compile-project (read-string "Project: " opal-compile-project))
  )
	
(defun opal-compile-help ()
  "Show help-message of ocs."
  (interactive)
  (setq opal-compile-silent t)
  (opal-compile "ocs help")
  (opal-compile-view)
  (save-excursion
    (switch-to-buffer opal-compile-diag-buffer)
    (if (get-buffer-window opal-compile-diag-buffer)
	(set-window-start
	 (get-buffer-window opal-compile-diag-buffer)
	 (point-min))
      )
    )
  )
	
(defun opal-compile-call ()
  "Start compiling of project."
  (interactive)
  (opal-compile opal-compile-project)
  )
	
(defun opal-compile-call-ask ()
  "Start compiling with asking for project."
  (interactive)
  (opal-compile-project-set)
  (opal-compile-call)
  )

(defun opal-compile-clean ()
  "Cleaning the OCS directory."
  (interactive)
  (start-process-shell-command "cleaning" 
			       opal-compile-process-buffer "rm" "-f" "OCS/*" )
  )
	
(defun opal-compile (&optional Param)
  "Start compiling."
  (interactive)

  (opal-ask-save-opal-buffers)
  (get-buffer-create opal-compile-process-buffer)
  (setq opal-compile-directory (file-name-directory (buffer-file-name)))
  (cond ((not Param)(setq opal-start-process 
	     '(lambda () (start-process "compiling" 
					opal-compile-process-buffer "ocs"))))
	((string= Param "")(setq opal-start-process 
	     '(lambda () (start-process "compiling" 
					opal-compile-process-buffer "ocs"))))
	(t (setq opal-start-process 
	     '(lambda () (start-process-shell-command 
			  "compiling" opal-compile-process-buffer Param))))
	)
  (cond 
   ((get-process "compiling") (princ "opal compilation process is running !"))
    (t (message "Opal compilation started... (type C-c C-c C-v to watch)")
       (setq opal-compile-errorlevel 0)
       (opal-compile-clear)
       (funcall opal-start-process)
;       (set-process-filter (get-process "compiling") 
;			   'opal-compile-output-filter)
       (set-process-sentinel (get-process "compiling") 
			     'opal-compile-sentinel)
   ))
)


(defun opal-compile-view ()
  "pop-to-buffer to view output of the opal compiler"
  (interactive)
  (pop-to-buffer opal-compile-process-buffer)
;  (setq buffer-read-only t)
)

(defun opal-compile-clear ()
  "clear *opal-compile* buffer"
  (interactive)
  (save-excursion
    (set-buffer opal-compile-process-buffer)
    (erase-buffer)
  )
)

(defun opal-compile-sysdefs ()
  "load SysDefs file from current directory"
  (interactive)
  (if (file-exists-p "SysDefs")
      (find-file "SysDefs")
      (message "%s" "no SysDefs file in current directory")
  )
)

(defun opal-compile-projectdefs ()
  "load current ProjectDefs file"
  (interactive)
  (if opal-compile-projectdefsfile
      (find-file opal-compile-projectdefsfile)
      (message "%s" "no ProjectDefs file set")
  )
)

(defun opal-compile-set-projectdefs ()
  "set name of ProjectDefs file"
  (interactive)
  (if (not opal-compile-projectdefsfile)
      (setq opal-compile-projectdefsfile "")
  )
  (setq opal-compile-projectdefsfile
	(read-file-name "ProjectDefs (empty string to clear):" 
			(file-name-directory opal-compile-projectdefsfile)
			nil
			nil
			(file-name-nondirectory opal-compile-projectdefsfile)
        )
  )
  (if (string= opal-compile-projectdefsfile "")
	(setq opal-compile-projectdefsfile nil)
  )
  (setenv "OCSPROJECT" opal-compile-projectdefsfile)
)


(defun opal-compile-sentinel (proc ev)
  "sentinel for OPAL compile process - switch to buffer and call diag, if errors occurred"
;  (setq mode-line-process nil)
  (beep)
  (if t ;; should be (string= ev "finished\n") but doesn't work at  TUB
      (progn
	(let (b)
	  (setq b (get-buffer-create opal-compile-diag-buffer))
	  (set-buffer b)
	  (delete-region (point-min) (point-max))
	  (insert-buffer opal-compile-process-buffer)
	  (setq opal-diag-source nil)
	  (setq opal-diag-buffer b)
	  (opal-diag-parse)
	  (if opal-diag-errors
	      (progn
		(opal-diag-next-main-error)
		(opal-diag-show-error)
		)
	    (if (not opal-compile-silent)
		(message "Opal compilation ended without diagnostics")
	      )
	    )
	  )
	)
    (message "Abnormal end: %s" ev)
    (opal-compile-view)
    )
)

(provide 'opal-x-compile)
