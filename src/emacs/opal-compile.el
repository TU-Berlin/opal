;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Opal Mode
;;; Compile Part
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq opal-compile-project "ocs" )

;; control creation of frames
(defvar opal-use-frames window-system
  "*control whether opal-compile should use frames" )

(defvar opal-compile-last-buffer nil
  "name of buffer of currently compiled OPAL structure part")

(defvar opal-compile-errorlevel nil
  "nil or 0: no errors; 1: hints, 2: warnings, 3: error")

(defvar opal-compile-show-messages
  "^\\(Generating\\|Checking\\|Archiving\\)"
  "*regexp to determine which compiler messages are to be displayed")

(defvar opal-compile-process-buffer "*opal-compile*"
  "name of buffer to display ocs output")

(defvar opal-compile-projectdefsfile nil
  "name of current opal projectdefs file (initialised from OCSPROJECT variable)")

(defvar opal-compile-save-mode-name nil
  "mode name of major mode where opal-compile was started")

(defvar opal-compile-directory nil
  "directory where to find opal files")

(defun opal-mode-compile-xemacs-menu ()
  "set up opal-mode compile menu for XEmacs"
  (setq opal-compile-menu 
	(list "Compile"
	      ["Compile Project"  opal-compile-call t]
	      ["Compile Project interactive" opal-compile-call-ask t]
	      ["Set project" opal-compile-project-set t]
	      ["Show project-help"  opal-compile-help t]
	      "---"
	      ["ocs clean"  opal-compile-clean t]
	      "---"
	      ["Show ocs output" opal-compile-view t]
	      ["Clear ocs output" opal-compile-clear t]
	      ["Kill compiling process" opal-compile-kill t]
	      "---"
	      ["Load SysDefs" opal-compile-sysdefs t]
	      ["Load ProjectDefs" opal-compile-projectdefs 
	       opal-compile-projectdefsfile]
	      ["Set ProjectDefs" opal-compile-set-projectdefs t]
	      ))
)

(defun opal-mode-compile-fsfemacs-menu ()
  "set up opal-mode compile menu for FSF Emacs"

  (if (not opal-novice)
      (progn
	(define-key opal-mode-map [menu-bar opal compile]
	  (cons "Compile" (make-sparse-keymap "Compile")))
	
	(define-key opal-mode-map 
	  [menu-bar opal  compile opal-compile-set-projectdefs]
	  '("Set ProjectDefs" . opal-compile-set-projectdefs))
	(define-key opal-mode-map [menu-bar opal  compile
					    opal-compile-projectdefs]
	  '("Load ProjectDefs" . opal-compile-projectdefs))
	(define-key opal-mode-map [menu-bar opal  compile opal-compile-sysdefs]
	  '("Load SysDefs" . opal-compile-sysdefs))
	(define-key opal-mode-map [menu-bar opal  compile t3]
	  '("" . nil))
	(define-key opal-mode-map [menu-bar opal  compile opal-compile-kill]
	  '("Kill compiling process" . opal-compile-kill))
	(define-key opal-mode-map [menu-bar opal  compile opal-compile-clear]
	  '("Clear ocs output" . opal-compile-clear))
	(define-key opal-mode-map [menu-bar opal  compile opal-compile-view]
	  '("Show ocs output" . opal-compile-view))
	(define-key opal-mode-map [menu-bar opal  compile t1]
	  '("" . nil))
	(define-key opal-mode-map [menu-bar opal  compile opal-compile-clean]
	  '("ocs clean" . opal-compile-clean))
	(define-key opal-mode-map [menu-bar opal  compile t2]
	  '("" . nil))
	(define-key opal-mode-map [menu-bar opal  compile opal-compile-help]
	  '("Show project-help" . opal-compile-help))
	(define-key opal-mode-map [menu-bar opal  compile opal-compile-project-set]
	  '("Set project" . opal-compile-project-set))
					;  (define-key opal-mode-map [menu-bar opal  compile opal-compile-call-ask]
					;    '("Compile Project interactive" . opal-compile-call-ask))
	(define-key opal-mode-map [menu-bar opal  compile opal-compile-call]
	  '("Compile Project" . opal-compile-call))
      )
  )
)

(put 'opal-compile-projectdefs 'menu-enable 'opal-compile-projectdefsfile)

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
;  (define-key opal-mode-map "\C-c\C-c\C-d" 'opal-compile-delete-frame)
  (define-key opal-mode-map "\C-c\C-c\C-k" 'opal-compile-kill)
  (define-key opal-mode-map "\C-c\C-c\C-v" 'opal-compile-view)
  (define-key opal-mode-map "\C-c\C-cs" 'opal-compile-sysdefs)
  (define-key opal-mode-map "\C-c\C-cp" 'opal-compile-projectdefs)
  (define-key opal-mode-map "\C-c\C-c\C-p" 'opal-compile-set-projectdefs)
  (define-key opal-mode-map "\C-c\C-c0" 'opal-compile-clear)

;; Menuepunkt Compile:
  (if opal-running-xemacs 
      (opal-mode-compile-xemacs-menu)
      (opal-mode-compile-fsfemacs-menu)
  )
)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opal-compile-running()
  (if (get-process "compiling") t nil))
(defun opal-compile-delete-frame-enable()
  (and (not (opal-compile-running)) (get-buffer opal-compile-process-buffer)))

(put 'opal-compile-call 'menu-enable '(not (opal-compile-running)))
(put 'opal-compile-call-ask 'menu-enable '(not (opal-compile-running)))
(put 'opal-compile-help 'menu-enable '(not (opal-compile-running)))
(put 'opal-compile-clean 'menu-enable '(not (opal-compile-running)))
(put 'opal-compile-delete-frame 'menu-enable
     '(opal-compile-delete-frame-enable))
(put 'opal-compile-kill 'menu-enable '(opal-compile-running))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opal-compile-kill ()
  "Kill the opal compile process."
  (interactive)
  (cond ((get-process "compiling") (delete-process "compiling"))
	(t (princ "No compile process exist"))
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
  (opal-compile "ocs help")
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
			       opal-compile-process-buffer "rm" "-fR" "OCS/*" )
  )
	
(defun opal-compile (&optional Param)
  "Start compiling."
  (interactive)

  (opal-ask-save-opal-buffers)
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
    (t (princ "opal compiling started... (type C-c C-c C-v to watch)")
       (funcall opal-start-process)
       (set-process-filter (get-process "compiling") 
			   'opal-compile-output-filter)
       (set-process-sentinel (get-process "compiling") 
			     'opal-compile-sentinel)
       (setq opal-compile-errorlevel 0)
   ))
;  (setq opal-compile-save-mode-name mode-name)
;  (setq mode-name (concat mode-name ": compiling"))
;  (setq mode-line-process " ocs compiling")
;  (opal-open-window "*opal-compile*")
;  (opal-open-window "*opal-compile-output*")
;  (save-excursion
;    (set-buffer "*opal-compile-output*")
;    (erase-buffer)
;  )
)

(defun opal-open-window (name)
  (let ((erg (opal-get-frame-p(frame-list) name))
	(curr-buff (buffer-name)))
    (cond ((car erg)
	   (make-frame-visible (cdr erg))
	   (raise-frame(cdr erg)))
	  (opal-use-frames 
	   (switch-to-buffer name)
	   (if (and
		  (string= name opal-compile-process-buffer)
	          (not (get-buffer name))
	       )
	       (progn
		 (set-frame-size (new-frame) 70 24)
		 (new-frame)
	       )
	   )
	   (switch-to-buffer curr-buff))
	  (t (delete-other-windows)
	     (split-window-vertically)
	     (other-window 1)
	     (switch-to-buffer name)
	     (other-window 1))
	  )))  

(defun opal-compile-delete-frame ()
  "Delete the opal-compile frame and the opal-compile buffer."
  (interactive)
  (let ((erg (opal-get-frame-p(frame-list) opal-compile-process-buffer)))
    (cond ((get-process "compiling") (princ "Process is running"))
	  ((car erg) (delete-frame(cdr erg))(kill-buffer opal-compile-process-buffer))
	  ((get-buffer opal-compile-process-buffer)
	   (kill-buffer opal-compile-process-buffer)
	   (if window-system
	       (princ "Frame \"*opal-compile*\" does not exist. Only buffer \"*opal-compile*\" was killed.")
	     (delete-other-windows)))
	  (t (princ "Frame and buffer \"*opal-compile*\" do not exist."))))
  )

(defun opal-compile-opt-save ()
  "If current buffer is modified, offer to save it"
  (interactive)
  (if (buffer-modified-p)
      (if (y-or-n-p "Opal file changed. Save it? ")
	  (save-buffer)
       )
  )
)

(defun opal-compile-view ()
  "pop-to-buffer to view output of the opal compiler"
  (interactive)
  (pop-to-buffer opal-compile-process-buffer)
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

(defun opal-compile-output-filter (proc string)
  "Filter to handle output from the ocs compilation"

(save-match-data
;; check the text for messages
  (if (string-match opal-compile-show-messages string)
      (message "%s" string)
  )
;; watch currently compiled structure part
  (if (string-match "^Checking Signature of \\(.+\\) ..." string)
      (progn
	(setq opal-compile-last-buffer 
	    (concat (substring string (match-beginning 1) (match-end 1)) 
		    ".sign"))
;	(message "%s%s" "now handling file " opal-compile-last-buffer)
      )
  )
  (if (string-match "^Checking Implementation of \\(.+\\) ..." string)
      (progn
	(setq opal-compile-last-buffer 
	    (concat (substring string (match-beginning 1) (match-end 1)) 
		    ".impl"))
;	(message "%s%s" "now handling file " opal-compile-last-buffer)
      )
  )
;; watch for errors, warnings or hints
  (if (string-match "ERROR:" string) (setq opal-compile-errorlevel 3))
  (if (and
        (<= opal-compile-errorlevel 1)
	(string-match "WARNING:" string)
      )
      (setq opal-compile-errorlevel 2)
  )
  (if (and
        (<= opal-compile-errorlevel 0)
	(string-match "HINT:" string)
      )
      (setq opal-compile-errorlevel 1)
  )
  (if (string-match "^\\(sh:\\|RUNTIME ERROR\\)" string)
      (setq opal-compile-errorlevel -1)
  )
  	

;; insert the text
  (opal-compile-output-insert string)

  )
)

(defun opal-compile-output-insert (string)
  "insert string in opal-compile output buffer"
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(set-buffer opal-compile-process-buffer)
;         (save-excursion
;	   (goto-char (process-mark proc))
	   (goto-char (point-max))
	   (insert string)
	   (set-mark (point-max))
;	   (set-marker (process-mark proc) (point))
;	   (goto-char (process-mark proc))
;	 )
	 (set-buffer old-buffer)
      )
  )
)
  

(defun opal-compile-sentinel (proc ev)
  "sentinel for OPAL compile process - switch to buffer and call diag, if errors occurred"
;  (setq mode-line-process nil)
  (beep)
  (cond
   ((string-match "exited abnormally" ev)
    (cond ((> opal-compile-errorlevel 0)
	   (progn
	     (message "%s" "diagnostics occurred, switching to buffer")
	     (pop-to-buffer 
	      (find-file-noselect 
	       (concat opal-compile-directory opal-compile-last-buffer)))
	     (delete-other-windows)
	     (raise-frame (window-frame (get-buffer-window (current-buffer))))
	     (opal-diag-update)
	     ))
	  (t
	   (message "%s" "serious errors in compilation")
	   (display-buffer opal-compile-process-buffer)
	  )
     )
    )
   ((string-match "finished" ev)
    (message "%s" "opal compilation ended successfully")
    (opal-compile-output-insert ev)
   )
   (t
    (message "%s%s" "opal compilation ended with " ev)
    (opal-compile-output-insert ev)
   )
  )
)


(provide 'opal-compile)
