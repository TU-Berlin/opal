;; $_Header$
;;; calling the interpreter from within emacs

(provide 'oasys-mode)
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
    (get-buffer-create oasys-output)
    (get-buffer-create oasys-answer-buffer)
    (save-excursion
      (set-buffer oasys-output)
      (delete-region (point-min) (point-max))
      (oasys-mode)
      )
    (setq oasys-tq (tq-create oasys-process))
    (oasys-enqueue-i "")
    )
  )

(defun oasys-enqueue (command)
  "send command to oasys process"

  (interactive "sCommand:")
  (oasys-enqueue-i (concat command "\n"))
)

(defun oasys-enqueue-i (command)
  "send command to oasys process"

  (if oasys-process
      (progn 
	(save-excursion
	  (set-buffer oasys-output)
	  (goto-char (point-max))
	  (insert command)
	  )
	(tq-enqueue oasys-tq command oasys-prompt nil 'oasys-process-answer)
	)
    (message "oasys is not running!")
    )
  )

;;; handling the process

(defvar oasys-answer-buffer "*oasys-answer*"
  "contains output of last transaction to oasys")

(defun oasys-process-answer (closure answer)
  "function to handle oasys transactions"

  (pop-to-buffer oasys-output) 
  (goto-char (point-max))
  (insert answer)
;  (save-excursion
    (set-buffer oasys-answer-buffer)
    (delete-region (point-min) (point-max))
    (insert answer "\n")
    (setq opal-diag-buffer oasys-answer-buffer)
    (setq ok (opal-diag-parse t))
;    )
  (if (car ok)
      (progn
	(opal-diag-next-main-error)
	(opal-diag-show-error)
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

(add-hook 'kill-emacs-hook 'oasys-kill)

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
  (define-key oasys-mode-map "\M-x" 'oasys-cmd)
)

(defun oasys-mode-menu ()
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
	      ["back to Opal" oasys-back t]))
  (set-buffer-menubar (copy-sequence current-menubar))
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
  (run-hooks 'oasys-mode-hook)
)
  
;; 

(defvar oasys-expr-buffer "*oasys-expr*" 
  "Buffer used for temporary storing of input expression")

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
      ) 
    (while (string-match "[{}]" escexpr)
      (replace-match "\\\\\\&")
      )
    (setq opal-diag-source oasys-expr-buffer)
    (oasys-enqueue (concat "e {" escexpr "}"))
    )
  )

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
)

(defun oasys-back ()
  "back to Opal source"
  (interactive)
  (oasys-start t)
  (let (source)
    (save-excursion
      (set-buffer oasys-answer-buffer)
      (goto-char (point-min))
      (search-forward oasys-prompt)
      (setq source (buffer-substring (match-beginning 1) (match-end 1)))
      )
    (opal-opalfile "" source)
    )
  )
