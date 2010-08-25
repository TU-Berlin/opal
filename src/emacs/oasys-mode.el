;; $_Header$
;;; calling the interpreter from within emacs

(provide 'oasys-mode)
(require 'oasys-expr-mode)
(require 'easymenu)
(require 'tq)


;;; some local variables

(defvar oasys-raw-output "*oasys-raw-output*"
  "buffer connected with the oasys process")

(defvar oasys-output "*oasys*"
  "buffer, in which the user sees output of oasys")

(defvar oasys-process nil
  "process object for oasys process")

(defvar oasys-tq nil
  "transaction queue for communicating with oasys")

(defvar oasys-prompt "^\\([a-zA-Z0-9]+\\.\\(sign\\|impl\\)\\)?>"
  "prompt of oasys, match 1 delivers current unit")

(defvar oasys-binary "oasys"
  "name of the oasys binary to start")

;; general functions

(defun oasys-start (&optional silent)
  "start oasys, if not already started"

  (interactive)
  (if oasys-process
      (if (not silent)
	  (message "oasys is running!")
	)
    (message "Starting oasys ...")
    (setq oasys-process
	  (start-process "oasys"
			 (get-buffer-create oasys-raw-output) oasys-binary))
    (set-process-sentinel oasys-process 'oasys-sentinel)
    (process-kill-without-query oasys-process)
    (get-buffer-create oasys-output)
    (get-buffer-create oasys-answer-buffer)
    (get-buffer-create oasys-expr-buffer)
    (save-excursion
      (set-buffer oasys-output)
      (delete-region (point-min) (point-max))
      (oasys-mode)
      (set-buffer oasys-raw-output)
      (oasys-mode)
      (set-buffer oasys-answer-buffer)
      (opal-diag-mode)
      (setq buffer-read-only nil)
      (set-buffer oasys-expr-buffer)
      (opal-mode)
      (set-buffer oasys-expr-buffer)
      (oasys-expr-mode)
      )
    (setq oasys-tq (tq-create oasys-process))
    (oasys-enqueue-i "") ; wait for first prompt
    )
  )

(defun oasys-enqueue (command &optional expr)
  "send command to oasys process"

  (interactive "sCommand:")
  (oasys-enqueue-i (concat command "\n") expr)
)

(defun oasys-enqueue-i (command &optional expr)
  "send command to oasys process"

  (if oasys-process
      (progn 
	(save-excursion
;	  (if expr
;	      (set-buffer oasys-output)
	    (set-buffer oasys-raw-output)
;	    )
	  (goto-char (point-max))
	  (insert command)
	  )
	(tq-enqueue oasys-tq command oasys-prompt expr 'oasys-process-answer)
	)
    (message "oasys is not running!")
    )
  )

;;; handling the process

(defvar oasys-answer-buffer "*oasys-answer*"
  "contains output of last transaction to oasys")

(defun oasys-process-answer (closure answer)
  "function to handle oasys transactions"

  (save-excursion
    (if closure
	(set-buffer oasys-output)
      (set-buffer oasys-raw-output) 
      )
    (goto-char (point-max))
    (insert answer)
    )
  (if closure 
      (progn 
	(pop-to-buffer oasys-output)
	(beginning-of-line)
	(kill-line)
	)
    )
  (save-excursion
    (set-buffer oasys-answer-buffer)
    (delete-region (point-min) (point-max))
    (insert answer "\n")
    (let (unit)
      (re-search-backward oasys-prompt)
      (setq unit (buffer-substring (match-beginning 1) (match-end 1)
				    oasys-answer-buffer))
;      (replace-match "")
      (set-buffer oasys-output)
      (setq mode-name (concat "oasys(v2): " unit))
      )
    (setq opal-diag-hide oasys-output)
    (setq opal-diag-source nil)
    (setq opal-diag-buffer oasys-answer-buffer)
    (setq opal-diag-buffer-may-kill nil)
    (setq ok (opal-diag-parse t (get-buffer oasys-expr-buffer)))
    )
  (if (car ok)
      (progn
	(opal-diag-next-main-error)
	(opal-diag-show-error)
	(message "correct expression and type C-c c")
	)
    )
)
  
(defun oasys-sentinel (process event)
  
  (setq oasys-process nil)
  (message "oasys %s" event)
)

(defun oasys-kill ()
  "kill oasys-process, if active"
  (interactive)
  (if oasys-process
      (oasys-quit)
    )
  )

;;; oasys-mode

(defvar oasys-mode-map nil "keymap for oasys mode")
(defvar oasys-mode-menu nil "oasys menu")

(defvar oasys-mode-hook '() "hook for oasys mode")

(defun oasys-mode-map ()
  "define keymap for oasys-mode"
  (setq oasys-mode-map (make-sparse-keymap))
  (define-key oasys-mode-map "\M-e" 'oasys-eval)
  (define-key oasys-mode-map "\M-c" 'oasys-check)
  (define-key oasys-mode-map "\M-f" 'oasys-focus)
  (define-key oasys-mode-map "\M-q" 'oasys-quit)
  (define-key oasys-mode-map "\M-b" 'oasys-back)
  (define-key oasys-mode-map "\M-k" 'oasys-cmd)
  ; similar to Opal
  (define-key oasys-mode-map "\M-n" 'opal-diag-next-main-error)
  (define-key oasys-mode-map "\M-p" 'opal-diag-prev-main-error)
  (define-key oasys-mode-map "\C-c\C-a\C-r" 'opal-oasys-raw)
  (if opal-running-xemacs
      (nil)
    (define-key oasys-mode-map [menu-bar oasys] 
      (cons "OASYS" (make-sparse-keymap "OASYS")))
    (define-key oasys-mode-map [menu-bar oasys oasys-quit]
      '("Quit" . oasys-quit))
    (define-key oasys-mode-map [menu-bar oasys line1]
      '(menu-item "--single-line"))
    (define-key oasys-mode-map [menu-bar oasys oasys-cmd]
      '("Command" . oasys-cmd))
    (define-key oasys-mode-map [menu-bar oasys line2]
      '(menu-item "--single-line"))
    (define-key oasys-mode-map [menu-bar oasys oasys-eval]
      '("Eval ..." . oasys-eval))
    (define-key oasys-mode-map [menu-bar oasys oasys-check]
      '("Check" . oasys-check))
    (define-key oasys-mode-map [menu-bar oasys oasys-focus]
      '("Focus" . oasys-focus))
     
      
    )
  
)


(defun oasys-mode-menu ()
  "Set the menu spec. of the opal mode"
  (interactive)
  (if opal-running-xemacs
					; ------ XEmacs related
      (opal-oasys-menu-xemaxs)
					; --- FSF Emacs related
      (opal-oasys-menu-fsfemacs)
    )
)

(defun opal-oasys-menu-fsfemacs ()
  "Set oasys-mode menu for FSF Emacs"
  (interactive)
  (progn (define-key oasys-mode-map [menu-bar oasys] 
	(cons "OASYS" (make-sparse-keymap "OASYS")))
      (define-key oasys-mode-map [menu-bar oasys oasys-eval]
       '("Eval ..." . oasys-eval))
      (define-key oasys-mode-map [menu-bar oasys oasys-check]
       '("Check" . oasys-check))
     (define-key oasys-mode-map [menu-bar oasys oasys-view]
       '("View" . oasys-view))
     (define-key oasys-mode-map [menu-bar oasys oasys-hide]
       '("Hide" . oasys-hide))
     (define-key oasys-mode-map [menu-bar oasys oasys-load]
       '("Load" . oasys-load)))
    
  )

(defun oasys-mode-menu-xemacs ()
  "define and set oasys-mode menu."

  (interactive)
  (setq oasys-mode-menu
	(list "Oasys"
	      ["Eval ..." oasys-eval t]
	      ["Check" oasys-check t]
	      "---"
	      ["Focus ..." oasys-focus t]
	      ["Quit" oasys-quit t]
	      ["Command ..." oasys-cmd t]
	      "---"
	      ["Back to Opal" oasys-back t]
	      "---"
	      (list "Expert"
		    ["Raw output" opal-oasys-raw t])
	      ))
  ;;(set-buffer-menubar (copy-sequence current-menubar))
  (add-submenu nil oasys-mode-menu)
)

(defun oasys-mode ()
  "major mode for oasys output buffer"

  (interactive)
  (setq major-mode 'oasys-mode)
  (setq mode-name "oasys(v2)")
  (oasys-mode-map)
  (use-local-map oasys-mode-map)
  (oasys-mode-menu)
  (opal-toolbar-install)
  (run-hooks 'oasys-mode-hook)
)
  
;; 


(defun oasys-eval (expr)
  "evaluate expression in current oasys focus"
  (interactive "sExpression:")
  (oasys-start t)
  (let (escexpr)
    (setq escexpr expr)
    (get-buffer-create oasys-expr-buffer)
    (save-excursion
      (set-buffer oasys-expr-buffer)
      (delete-region (point-min) (point-max))
      (insert expr)
      (set-buffer oasys-output)
      (let (a b)
	(setq a (point))
	(insert expr)
	(setq b (point))
	(insert "\n")
;	(set-extent-face (make-extent a b (get-buffer oasys-output))
;			 'oasys-input-face)
	)
      ) 
    (while (string-match "[{}]" escexpr)
      (replace-match "\\\\\\&")
      )
    (while (string-match "\n" escexpr)
      (replace-match "")
      )
    (setq opal-diag-source oasys-expr-buffer)
    (oasys-enqueue (concat "e {" escexpr "}") t)
    )
  )

(make-face 'oasys-input-face)
(set-face-background 'oasys-input-face "lightblue")

(defun oasys-check ()
  "check units"
  (interactive)
  (oasys-start t)
  (oasys-enqueue "c")
)

(defun oasys-focus (unit)
  "focus on given unit"
  (interactive "sUnit:")
  (oasys-start t)
  (oasys-enqueue (concat "f " unit))
  )

(defun oasys-quit ()
  "quit oasys"
  (interactive)
  (oasys-start t)
  (oasys-enqueue "q")
  )

(defun oasys-cmd (cmd)
  "send cmd to oasys"
  (interactive "sCommand:")
  (oasys-start t)
  (oasys-enqueue cmd)
  (pop-to-buffer oasys-raw-output)
)

(defun oasys-add-path (path)
  "add path to oasys internal path"
  (oasys-start t)
  (oasys-enqueue (concat "oasys-path add ocs " path))
)

(defun oasys-back ()
  "back to Opal source"
  (interactive)
  (oasys-start t)
  (let (source)
    (save-excursion
      (set-buffer oasys-answer-buffer)
      (goto-char (point-max))
      (re-search-backward oasys-prompt)
      (setq source (buffer-substring (match-beginning 1) (match-end 1)))
      )
    (setq fullname (opal-find-structure source))
    (if fullname
	(progn
	  (opal-diag-clear-diags)
	  (find-file fullname)
	  )
      (message "Could not find %s" source)
      )
    )
  )
