;; $_Header$
;;; calling the interpreter from within emacs

(provide 'oasys-mode)
(require 'comint)
(require 'easymenu)

;; 0) keymap and menu setup

;; 1) variables

(defvar oasys-dir "/home/uebb/kd/ocs/experimental/oasys/"
  "*directory for the oasys program")

(defvar oasys-prog (concat oasys-dir "bin/oasys")
  "*complete file name of the OPAL interpreter"
)

(defvar oasys-client (concat oasys-dir "bin/opalClient")
  "*complete file name of opal client")

(defvar oasys-LD-PATH (concat "OCS:" oasys-dir "lib/opalica")
  "*setting of LD_LIBRARY_PATH for opal client")

(defconst oasys-init-string (concat
"oasys-path add library " oasys-dir "lib/opalica/
oasys-register NatConv
oasys-register BoolConv
oasys-register IntConv
oasys-register RealConv
oasys-register CharConv
oasys-register Denotation
oasys-print-method nat'Nat `'NatConv
oasys-print-method bool'BOOL `'BoolConv
oasys-print-method int'Int `'IntConv
oasys-print-method real'Real RealConv.sign/glob1
oasys-print-method char'Char `'CharConv
oasys-print-method denotation'DENOTATION Denotation.sign/glob20
oasys-channel-open"
)
  "*string to send oasys at the very beginning")

(defvar oasys-current-opal-buffer nil
  "buffer of current OPAL file")

(defvar oasys-buffer-name "*oasys*"
  "name of oasys buffer")

(defvar oasys-client-buffer-name "*oasys-client*"
  "name of buffer associated with opal client")

(defvar oasys-mode-hook '()
  "*Hook for customising OPAL/oasys mode")

;; 3) the mode for controlling the interpreter

(defvar oasys-mode-map nil
  "keymap for oasys-mode")
(cond ((not oasys-mode-map)
       (setq oasys-mode-map (copy-keymap comint-mode-map))
       (define-key oasys-mode-map "\C-q" 'oasys-quit)
       (define-key oasys-mode-map "\M-r" 'oasys-restart)
       (define-key oasys-mode-map "\C-b" 'oasys-return)
      )
)


(defun oasys-mode-set-menu ()
  "set the menu of OPAL/oasys mode"

  (interactive)
  (easy-menu-define 
   oasys-mode-menu
   (list oasys-mode-map)
   "menu of oasys mode"
   (list "OASYS"
	 ["Restart oasys" oasys-restart t]
	 ["Restart opal-client" oasys-client-restart t]
	 ["Quit oasys" oasys-quit t]
	 ["Back to current opal file" oasys-return t]
	 )
  )
  (easy-menu-add oasys-mode-menu)
)

  
(defun oasys-mode ()
  "major mode for running oasys.
Derived from comint-mode."

  (interactive)
  (comint-mode)
  (setq major-mode 'opal-oasys)
  (setq mode-name "oasys 1.00")
  (use-local-map oasys-mode-map)
  (setq comint-prompt-regexp "^>")
  (setq oasys-current-opal-buffer nil)
  (add-hook 'comint-output-filter-functions 'oasys-eval-filter)
  (add-hook 'comint-output-filter-functions 'oasys-command-pipeline-filter)
  (oasys-mode-set-menu)
  (run-hooks 'oasys-mode-hook)
  
)


;; 4) oasys program start

(defun oasys-restart ()
  "start oasys or restart oasys if terminated"

  (interactive)
  (if (not (comint-check-proc oasys-buffer-name))
      (progn
;	(setenv "OASYSDIR" oasys-dir)
	(setenv "LD_LIBRARY_PATH" oasys-LD-PATH)
	(save-excursion
	  (set-buffer 
	   (make-comint "oasys" oasys-prog nil "default.repo"))
	  (process-kill-without-query (get-buffer-process oasys-buffer-name))
	  (oasys-mode)
	  (oasys-insert "-----------------------------------------------")
	  (oasys-perform oasys-init-string)
;	  (sleep-for 30)
	)
      )
  )
)

(defun oasys-restart-check ()
  "t, iff *oasys* buffer is terminated"

  (interactive)
  (not (comint-check-proc oasys-buffer-name))
)

(defun oasys-client-restart ()
  "start opal client or restart opal client if terminated"

  (interactive)
  (if (not (comint-check-proc oasys-client-buffer-name))
      (progn
;	(setenv "OCS_DLDDEBUG" "yes")
	(save-excursion
	  (set-buffer (make-comint "oasys-client" oasys-client))
	  (process-kill-without-query 
	   (get-buffer-process oasys-client-buffer-name))
	)
      )
  )
)

(defun oasys-close ()
  "quit oasys, kill all oasys-buffers"
  (interactive)
  (if (not (oasys-restart-check))
      (oasys-quit)
  )
  (if (get-buffer oasys-buffer-name) (kill-buffer oasys-buffer-name))
  (if (get-buffer oasys-client-buffer-name) 
      (kill-buffer oasys-client-buffer-name))
  (if (get-buffer oasys-eval-buffer) (kill-buffer oasys-eval-buffer))
  (if (get-buffer oasys-check-diagnostics-buffer) 
      (kill-buffer oasys-check-diagnostics-buffer))
)

(defun oasys (&optional display)
  "start oasys and opal client if necessary, switch to buffer *oasys*, or just display, iff display is t"

  (interactive)
  (oasys-client-restart)
  (oasys-restart)
  (if display
      (display-buffer oasys-buffer-name)
    (pop-to-buffer oasys-buffer-name)
  )
)
  
;; 5) filtering output

(defconst oasys-command-pipeline '()
  "list of commands (strings) to send to oasys until some error occurs. 
last argument must be 0")

(defvar oasys-command-pipeline-error nil
  "t if some error occurred"
)


(defvar oasys-command-pipeline-filter nil
  "if non-nil, function to call for pipeline output")

(defun oasys-command-pipeline-filter (text)
  "monitor text for error messages and prompts. send next command to oasys if no error occurred and pipeline isn't empty."

 (if (null oasys-command-pipeline)
     (); do nothing
   (let ((a (string-match comint-prompt-regexp text))
	 (e (string-match "^\\(FAILURE\\)" text))
	 )
     (setq oasys-command-pipeline-error (or oasys-command-pipeline-error e))
     (if oasys-command-pipeline-filter
	 (funcall oasys-command-pipeline-filter text)
     )
     (if a
	 (if oasys-command-pipeline-error
	     (progn
	       (setq oasys-command-pipeline '())
	     )
	   ; no error
	   (if (car-safe oasys-command-pipeline)
	       (progn ; start next command
		 (let (c)
		   (setq c (car oasys-command-pipeline))
		   (setq oasys-command-pipeline (cdr oasys-command-pipeline))
		   (if (numberp c)
		       (); nothing
		     (message "pipeline: %s" c)
		     (oasys-perform (concat "! " c))
		   )
		 )
	       )
	     ; no next command
	   )
	 )
     )
   ) 
 )
)

(defun oasys-pipeline-init (cmdlist &optional filter)
  "set up oasys command pipeline to perform commands in list"

  (let (c r)
    (setq c (car cmdlist))
    (setq oasys-command-pipeline (append (cdr cmdlist) 0 nil))
    (setq oasys-command-pipeline-filter filter)
    (setq oasys-command-pipeline-error nil)
    (oasys-perform (concat "! " c))
  )
)

(defun oasys-pipeline-wait ()
  "wait until command-pipeline is empty"
  (while oasys-command-pipeline
    (accept-process-output)
  )
)
    
;; 6) OASYS menu items
(defun oasys-quit ()
  "quit oasys"

  (interactive)
  (oasys-perform "oasys-quit")
)

(defun oasys-help ()
  "show oasys help"

  (interactive)
  (oasys-perform "oasys-help")
)

(defun oasys-return ()
  "return to current OPAL file, if defined"

  (interactive)
  (if oasys-current-opal-buffer
      (progn
	(pop-to-buffer oasys-current-opal-buffer)
	(raise-frame (window-frame (get-buffer-window (current-buffer))))
      )
   )
)

;; 5) 
(defun oasys-set-current (buffer)
  "set current OPAL file"

  (setq oasys-current-opal-buffer buffer)
)

(defvar oasys-known-paths '()
  "list of paths already sent to oasys")

(defun oasys-insert-path (p)
  "add path to oasys-known-paths"
  (setq oasys-known-paths (cons p oasys-known-paths))
)

(defun oasys-prepare-current-opal ()
  "prepare current opal buffer for checking"
  (interactive)
  (let*  ((fn (buffer-file-name oasys-current-opal-buffer))
	  (pathCmd (concat "oasys-path add ocs "
			   (file-name-directory fn)))
	  (registerCmd (concat "oasys-register " (oasys-structure-of fn)))
	  (reloadCmd (concat "oasys-reload " (oasys-unit-of fn)))
	  (focusCmd  (concat "oasys-focus " (oasys-unit-of fn)))
         )
    (message "%s" "ensuring presence of oasys and opalClient ...")
    (oasys-client-restart)
    (oasys-restart)
    (message "preparing %s ..." (oasys-unit-of fn))
    (if (member (file-name-directory fn) oasys-known-paths )
	(oasys-pipeline-init (list registerCmd reloadCmd focusCmd))
      (oasys-insert-path (file-name-directory fn))
      (oasys-pipeline-init   (list pathCmd registerCmd reloadCmd focusCmd))
    )
    (oasys-pipeline-wait)
    (if oasys-command-pipeline-error
	(progn
	  (error "serious oasys error!")
	  (display-buffer oasys-buffer-name)
	  )
					; else - weitermachen
      (message "checking %s ..." (oasys-unit-of fn))
      (setq oasys-check-errors nil)
      (oasys-check-diag-init)
      (oasys-pipeline-init (list "oasys-check") 'oasys-check-filter)
      (oasys-pipeline-wait)
      (if oasys-command-pipeline-error
	  (progn
	    (error "serious oasys error during check!")
	    (display-buffer oasys-buffer-name)
	    )
	(message "checking %s finished!" (oasys-unit-of fn))
	(oasys-check-preparse-diagnostics oasys-check-diagnostics-buffer)
	(oasys-check-eval-message oasys-check-diagnostics-buffer)
      )
      )
    )
)

(defvar oasys-check-diagnostics-buffer "*oasys-check-diagnostics*"
  "buffer name in which checking diagnostics are assembled")

(defun oasys-check-diag-init ()
  "clear and create buffer oasys-check-diagnostics-buffer"
  (interactive)
  (get-buffer-create oasys-check-diagnostics-buffer)
  (save-excursion
    (set-buffer oasys-check-diagnostics-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (opal-diag-mode)
    (setq buffer-read-only nil)
    (if opal-running-xemacs (font-lock-mode 1))
    )
  )


(defun oasys-check-filter (text)
  "filter function for check command"
  
  (save-excursion
    (set-buffer oasys-check-diagnostics-buffer)
    (goto-char (point-max))
    (insert text)
  )
)

(defun oasys-check-preparse-diagnostics (&optional buffer)
  "prepare check-diagnostics-buffer for parsing by opal-diag functions"
  (interactive)
  (if (not buffer) (setq buffer (current-buffer)))
  (save-excursion
    (let (current-file (case-fold-search nil) (case-replace nil))
      (set-buffer buffer)
      (goto-char (point-min))
      (catch 'finished
	(while t
	  (cond 
	   ((looking-at "checking \\(.*\\)") ;; current file, delete but save
	    (setq current-file 
		  (buffer-substring (match-beginning 1) (match-end 1)))
	    (kill-line)
	    (kill-line))
	   ((looking-at ;; ERROR , insert current file
	     "\\(\\(ERROR\\|WARNING\\|HINT\\) \\[\\)\\(#[0-9]+\\) ")
	    (replace-match (concat "\\1" (regexp-quote current-file) " ") t)
	    (forward-line))
	   ((looking-at "\\.\\. updated absy of")  ;; ignore
	    (kill-line)
	    (kill-line))
	   ((looking-at "ERROR \\[check\\]") ;; ignore
	    (kill-line)
	    (kill-line))
	   ((looking-at "OUTPUT:")  ;; finished
	    (delete-region (point) (point-max))
	    (throw 'finished t))
	   (t                  ;non recognized lines belong to diagnostics
	    (forward-line))
	  )
	)
      )
      ;; einheitliches Layout aller Fehlermeldungen wg. Hilfestellung
      (goto-char (point-min))
      (while (re-search-forward "\\(]: \\)\\(.\\)" nil t)
	(replace-match "\\1\n\\2" t)
      )
    )
    (setq buffer-read-only t)
  )
)
	    
(defun oasys-check-eval-message (check-diag-buffer)
  "insert message about success or failure of checking in eval buffer.
if errors occurred, inhibit evaluation, otherwise allow it."
  (let ((current-unit (oasys-unit-of 
		       (buffer-file-name oasys-current-opal-buffer)))
       )
    (save-excursion
      (set-buffer check-diag-buffer)
      (goto-char (point-min))
      (cond
       ((re-search-forward "^ERROR" nil t)
	(beep)
	(oasys-eval-message (concat "found errors while checking " 
				    current-unit))
	(setq oasys-eval-allowed nil))
       ((re-search-forward "^WARNING" nil t)
	(beep)
	(oasys-eval-message (concat "encountered warnings while checking "
				    current-unit
				    "; expect runtime errors"))
	(setq oasys-eval-allowed t))
       ((re-search-forward "^HINT" nil t)
	(oasys-eval-message (concat "encountered hintss while checking "
				    current-unit
				    "; check diagnostics in case of "
				    "unexpected behaviour"))
	(setq oasys-eval-allowed t))
       (t
	(setq oasys-eval-allowed t))
      )
    )
  )
)
    
(defvar oasys-check-errors nil
  "errors encountered in this check")

;; oo) Auxiliary functions
(defun oasys-perform (str)
  "send and insert str ++ \"\\n\" to oasys"
  (interactive "sString to send and insert:")

  (oasys-send str)
  (oasys-insert str)
)

(defun oasys-send (str)
  "add \\n to string and send it to oasys"

  (oasys-send-i (concat str "\n"))
)

(defun oasys-send-i (str)
  "send string to the oasys process in *oasys* buffer"

  (interactive "sString to send:")
  (comint-send-string (get-buffer-process oasys-buffer-name) str)
)

(defun oasys-insert (str)
  "insert string ++ \"\\n\" in process buffer (without sending it to oasys!!)"

  (oasys-insert-i (concat str "\n"))
)

(defun oasys-insert-i (str)
  "insert string in process buffer (without sending it to oasys!!)"

  (interactive "sString to insert:")
  (let ((old-buffer (current-buffer))
	(proc (get-buffer-process oasys-buffer-name)))
    (unwind-protect
      (set-buffer oasys-buffer-name)
;      (save-excursion
	(goto-char (process-mark proc))
	(insert str)
	(set-marker (process-mark proc) (point))
;	(comint-show-maximum-output)
;      )
    (set-buffer old-buffer)
)))

(defun oasys-structure-of (fname)
  "return the trunc of the filename (usually the structure name)"
  (let ((bn (file-name-nondirectory fname)))
    (string-match "\\([^.]*\\)\\..*" bn)
    (substring bn (match-beginning 1) (match-end 1))
  )
)

(defun oasys-unit-of (fname)
  "return the trunc of the filename (usually the structure name)"
  (file-name-nondirectory fname)
)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; oasys-eval-mode
;;

(defconst oasys-eval-prompt ">"
  "prompt for oasys-eval buffer"
)

(defvar oasys-eval-buffer "*oasys-eval*"
  "name of the oasys-eval buffer")

(defconst oasys-eval-allowed nil
  "t iff last check was successful, and oasys can evaluate expressions"
)

(defvar oasys-eval-mode-map nil
  "local map for oasys-eval-mode")

(defvar oasys-eval-mode-name "oasys-eval 1.00"
  "mode name for oasys-eval-mode")

(defun oasys-eval-mode-set-menu ()
  "set the menu of OPAL/oasys mode"

  (interactive)
  (easy-menu-define 
   oasys-eval-mode-menu
   (list oasys-eval-mode-map)
   "menu of oasys-eval mode"
   (list "oasys-eval"
	 ["Restart" oasys-eval-restart t]
	 ["Restart oasys only" oasys-restart t]
	 ["Restart opal-client only" oasys-client-restart t]
	 ["Back to current opal file" oasys-return t]
	 ["Switch focus" oasys-eval-focus t]
	 ["Set print method" oasys-eval-print-method t]
	 "---"
	 ["Quit eval buffer" oasys-eval-quit t]
	 )
  )
  (easy-menu-add oasys-eval-mode-menu)
)

(defun oasys-eval-mode ()
  "mode for sending expressions to oasys"
  (interactive)
  (use-local-map oasys-eval-mode-map)
  (setq mode-name oasys-eval-mode-name)
  (setq major-mode 'oasys-eval-mode)
  (oasys-eval-mode-set-menu)
  (run-hooks 'oasys-eval-mode-hook)
)

(if oasys-eval-mode-map
    (); do not change existing map
  (setq oasys-eval-mode-map (make-sparse-keymap))
  (define-key oasys-eval-mode-map "\C-r" 'oasys-eval-restart)
  (define-key oasys-eval-mode-map "\C-m" 'oasys-eval-send)
  (define-key oasys-eval-mode-map "\C-f" 'oasys-eval-focus)
  (define-key oasys-eval-mode-map "\C-b" 'oasys-return)
  (define-key oasys-eval-mode-map "\C-q" 'oasys-eval-quit)
  (define-key oasys-eval-mode-map "\C-p" 'oasys-eval-print-method)
  (define-key oasys-eval-mode-map "\M-p" 'oasys-eval-prev-history)
  (define-key oasys-eval-mode-map "\M-n" 'oasys-eval-next-history)
)

(defun oasys-eval-send ()
  "send text of the current line minus prompt for evaluation to oasys and show output in current buffer"
  (interactive)
  (if (oasys-eval-check)
      (if (not oasys-eval-allowed)
	  (error "cannot evaluate; remove errors first")
	(let (a)
	  (if (string-match (concat "^\\(" (regexp-quote oasys-eval-prompt) 
				    "\\)\\(.*\\)$" )(opal-current-line))
	      (progn
		(setq a (substring (opal-current-line) 
				   (match-beginning 2) (match-end 2)))
		(goto-char (point-max))
		(insert "\n")
		(setq oasys-eval-copy-buffer (current-buffer))
		(oasys-perform (concat "! oasys-eval {" a "}"))
		)
	    (error "oasys-eval: not at input line")
	    )
	  )
	)  
    )
)

  
(defun oasys-eval-check ()
  "return t, iff both oasys and opal client run"
  (interactive)
  (if (comint-check-proc oasys-buffer-name)
      (if (comint-check-proc oasys-client-buffer-name)
	  t
	(error "opal client not active!")
      )
    (error "oasys not active!")
  )
)

(defun oasys-eval (&optional focus nopop)
  "establish buffer *oasys-eval* if necessary and select it."

  (interactive)
  (if (not (get-buffer oasys-eval-buffer))
      (progn
	(set-buffer (get-buffer-create oasys-eval-buffer))
	(oasys-eval-mode)
	(insert oasys-eval-prompt)
	(setq oasys-eval-history 0)
      )
  )
  (if (not nopop)
      (pop-to-buffer oasys-eval-buffer)
  )
  (if focus
      (oasys-eval-focus focus)
    )
)

(defun oasys-eval-message (text)
  "insert a message into oasys-eval-buffer"
  (save-excursion
    (oasys-eval nil t)
    (set-buffer oasys-eval-buffer)
    (goto-char (point-max))
    (insert "\n" "[" text "]" "\n" oasys-eval-prompt)
  )
  (message "%s" text)
)

(defun oasys-eval-quit ()
  "kill buffer *oasys-eval*"

  (interactive)
  (kill-buffer oasys-eval-buffer)
  (delete-window)
)

(defconst oasys-eval-copy-buffer nil
  "non-nil means to copy output from oasys into that buffer"
)
  
(defun oasys-eval-filter (text)
  "filter to copy output from oasys in buffer oasys-eval-copy-buffer, if that variable is set. resets that variable, if output matches comint-prompt-regexp."
  (interactive)

  (if oasys-eval-copy-buffer
      (let ((a (string-match comint-prompt-regexp text))
	    )
	(progn
	  (cond
	   ((not a) (oasys-eval-filter-dispatch text))
	   ((> a 0) (oasys-eval-filter-insert text a)
	            (oasys-eval-filter-end))
	   ((= a 0) (oasys-eval-filter-end))
	   (t       (oasys-eval-filter-dispatch text))
	  )
	)
    )
  )
)

;; (defun oasys-eval-filter (text)
;;   "filter to copy output from oasys in buffer oasys-eval-copy-buffer, if that variable is set. resets that variable, if output matches comint-prompt-regexp."
;;   (interactive)
;; 
;;   (if oasys-eval-copy-buffer
;;       (progn
;; 	(if (or oasys-eval-errors
;; 		(string-match (regexp-quote "ERROR [") text))
;; 	    (progn
;; 	      (setq oasys-eval-errors t)
;; 	      (

(defun oasys-eval-filter-insert (text &optional upto)
  "insert text into oasys-eval-copy-buffer; if upto is non-nil then insert substring from 0 to (upto - 1)"
  (save-excursion
    (set-buffer oasys-eval-copy-buffer)
    (goto-char (point-max))
    (if upto
	(insert (substring text 0 (- upto 1)))
      (insert text)
    )
  )
)
    

(defun oasys-eval-filter-end ()
  "reset oasys-tee-buffer and insert prompt"
  (interactive)
  (pop-to-buffer oasys-eval-copy-buffer)
  (goto-char (point-max))
  (insert "\n")
  (insert oasys-eval-prompt)
  (setq oasys-eval-copy-buffer nil)
  (setq oasys-eval-history 0)
)

(defun oasys-eval-filter-dispatch (text)
  "check text for appearing in oasys-eval-buffer or message line"

  (cond
   ((string-match "^OUTPUT:\\(.*\\)$" text)
    (oasys-eval-filter-insert 
     (substring text (match-beginning 1) (match-end 1)))
   )
   (t ; (string-match "^ERROR" text)
    (oasys-eval-filter-insert text)
   )
   (t
    (message "%s" text)
   )
  )
;  (save-excursion
;    (set-buffer "*scratch*")
;    (goto-char (point-max))
;    (insert "---\n" text "\n")
;  )
)

(defun oasys-eval-focus (unit)
  "set focus to unit (structure name + .sign or .impl)"
  (interactive "sSet focus to unit:")
  (if (string-match "\\.\\(sign\\|impl\\)$" unit)
      (progn
	(oasys-pipeline-init 
	 (list (concat "oasys-eval-interpreted " unit) 
	       (concat "oasys-focus " unit)
	       "oasys-save"))
	(oasys-pipeline-wait)
	(if oasys-command-pipeline-error
	    (error "could not set focus to %s" unit)
	  (save-excursion
	    (set-buffer oasys-eval-buffer)
	    (setq mode-name (concat oasys-eval-mode-name ": " unit))
	  )
	)
       )
    ; else
    (error "units must end in \".sign\" or \".impl\"!")
  )
)

(defun oasys-eval-print-method (sort fun)
  "set print-method for sort to fun"

  (interactive "sSort:\nsFunction:")
  (if (oasys-eval-check)
      (progn
	(goto-char (point-max))
	(insert "\n")
	(setq oasys-eval-copy-buffer (current-buffer))
	(oasys-perform (concat "! oasys-print-method " sort " " fun))
      )
    ;; else
    (error "oasys and/or opalClient not found!")
  )
)

(defun oasys-eval-restart ()
  "restart oasys and opalClient if necessary, reset focus if oasys-current-opal-buffer defined"
  (interactive)
  (let ((a oasys-current-opal-buffer))
    (message "restarting opal interpreter ...")
    (oasys-restart)
    (oasys-client-restart)
    (message "resetting focus ...")
    (if a
	(oasys-eval-focus (oasys-unit-of (buffer-file-name a)))
    )
    (setq oasys-current-opal-buffer a)
  )
)

(defvar oasys-eval-history 0
  "current number of history item")

(defun oasys-eval-insert-history (no)
  "insert no history item at point"
  
  (let (ok hitem)
    (save-excursion
      (setq ok (re-search-backward 
		(concat "^" (regexp-quote oasys-eval-prompt) "\\(.*\\)")
		nil t (+ no 1)))
      (if ok
	  (setq hitem (buffer-substring (match-beginning 1) (match-end 1)))
      )
    )
    (if ok
	(progn
;	  (kill-line)
	  (insert hitem)
	)
      ; else
      (setq oasys-eval-history (- oasys-eval-history 1))
      (error "too far back!")
    )
  )
)    

(defun oasys-eval-next-history ()
  "if at input line, go forward in history"
  (interactive)
  (let (a)
    (setq a (opal-current-line))
    (if (string-match (concat "^\\(" (regexp-quote oasys-eval-prompt) 
				    "\\)\\(.*\\)$" ) a)
	(progn
	  (beginning-of-line)
	  (kill-line)
	  (insert oasys-eval-prompt)
	  (if (> oasys-eval-history 0)
	      (progn
		(setq oasys-eval-history (- oasys-eval-history 1))
		(oasys-eval-insert-history oasys-eval-history)
	      )
	    (error "already at current input!")
	   )
	)
      (error "not at input line!")
    )
   )
)

(defun oasys-eval-prev-history ()
  "if at input line, go forward in history"
  (interactive)
  (let (a)
    (setq a (opal-current-line))
    (if (string-match (concat "^\\(" (regexp-quote oasys-eval-prompt) 
				    "\\)\\(.*\\)$" ) a)
	(progn
	  (beginning-of-line)
	  (kill-line)
	  (insert oasys-eval-prompt)
	  (setq oasys-eval-history (+ oasys-eval-history 1))
	  (oasys-eval-insert-history oasys-eval-history)
	)
      (error "not at input line!")
    )
   )
)
        
