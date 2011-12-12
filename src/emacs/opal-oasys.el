;;; opal-oasys.el --- An Oasys interaction mode

;; Purpose:
;;
;; To send an Opal buffer to another buffer running an Oasys
;; interpreter.
;;
;;
;;
;; Customisation:
;;
;; The name of the Opal interpreter is in oasys-program-name.
;;
;; Arguments can be sent to the Opal interpreter when it is started by
;; setting oasys-program-args (empty by default) to a list of
;; string args to pass it.  This value can be set interactively by
;; calling C-c C-s with an argument (i.e. C-u C-c C-s).
;;
;; `opal-oasys-hook' is invoked in the *oasys* buffer once Oasys is
;; started.
;;
;; All functions/variables start with `turn-{on,off}-opal-oasys' or
;; `opal-oasys-'.



(defgroup opal-oasys nil
  "Major mode for interacting with an inferior Oasys session."
  :group 'opal
  :prefix "opal-oasys-")

(defun turn-on-opal-oasys ()
  "Turn on Opal interaction mode with an Oasys interpreter running in an
another Emacs buffer named *oasys*.
Maps the following commands in the opal keymap:
    \\<opal-mode-map>\\[opal-oasys-start-process] to create the Oasys buffer and start a Oasys process in it.
    \\[opal-oasys-load-file] to save the current buffer and load and focus it in Oasys.
    \\[opal-oasys-reload-file] to send the :eload command to Oasys without saving the buffer.
    \\[opal-oasys-show-oasys-buffer] to show the Oasys buffer and go to it."
  (local-set-key "\C-c\C-s" 'opal-oasys-start-process)
  (local-set-key "\C-c\C-l" 'opal-oasys-load-file)
  (local-set-key "\C-c\C-b" 'opal-oasys-show-oasys-buffer))

(defun turn-off-opal-oasys ()
  "Turn off Opal interaction mode with an oasys interpreter within a buffer."
  (local-unset-key  "\C-c\C-s")
  (local-unset-key  "\C-c\C-l")
  (local-unset-key  "\C-c\C-b"))

(define-derived-mode opal-oasys-mode comint-mode "Opal Oasys"
  "Major mode for interacting with an inferior Oasys session.

The commands available from within an Opal program are:
    \\<opal-mode-map>\\[opal-oasys-start-process] to create the Oasys buffer and start an Oasys process in it.
    \\[opal-oasys-load-file] to save the current buffer and load and focus it in Oasys.
    \\[opal-oasys-show-oasys-buffer] to show the Oasys buffer and go to it.

\\<opal-oasys-mode-map>Commands:
\\[comint-send-input] after end of Oasys output sends line as input to Oasys.
\\[comint-send-input] before end of Oasys output copies rest of line and sends it to Oasys as input.
\\[comint-kill-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[comint-interrupt-subjob] interrupts the comint or its current subjob if any.
\\[comint-stop-subjob] stops, likewise. \\[comint-quit-subjob] sends quit signal.")


;; Oasys interface:

(require 'comint)
(require 'shell)

(defvar opal-oasys-process nil
  "The active Oasys subprocess corresponding to current buffer.")

(defvar opal-oasys-process-buffer nil
  "*Buffer used for communication with Oasys subprocess for current buffer.")

(defcustom opal-oasys-program-name "oasys"
  "*The name of the Oasys interpreter program."
  :type 'string
  :group 'opal-oasys)

(defcustom opal-oasys-program-args nil
  "*A list of string args to pass when starting the Oasys interpreter."
  :type '(repeat string)
  :group 'opal-oasys)

(defvar opal-oasys-load-end nil
  "Position of the end of the last load command.")

(defvar opal-oasys-error-pos nil
  "Position of the end of the last load command.")

(defvar opal-oasys-send-end (point-min)
  "Position of the end of the last send command.")



(defun opal-oasys-start-process (arg)
  "Start an Oasys process and invoke `opal-oasys-hook' if not nil.
Prompt for a list of args if called with an argument."
  (interactive "P")
  (if arg
      ;; XXX [CDW] Fix to use more natural 'string' version of the
      ;; XXX arguments rather than a sexp.
      (setq opal-oasys-program-args
            (read-minibuffer (format "List of args for %s:"
                                     opal-oasys-program-name)
                             (prin1-to-string opal-oasys-program-args))))

  ;; Start the Oasys process in a new comint buffer.
  (message "Starting Oasys process `%s'." opal-oasys-program-name)
  (setq opal-oasys-process-buffer
        (apply 'make-comint
               "oasys" opal-oasys-program-name nil
               opal-oasys-program-args))
  (setq opal-oasys-process
        (get-buffer-process opal-oasys-process-buffer))

  ;; Select Oasys buffer temporarily.
  (set-buffer opal-oasys-process-buffer)
  (opal-oasys-mode)
  (make-local-variable 'shell-cd-regexp)
  (make-local-variable 'shell-dirtrackp)

  ;; Track directory changes using the `:cd' command.
  ;;(setq shell-cd-regexp ":cd")
  ;;(setq shell-dirtrackp t)
  ;;(add-hook 'comint-input-filter-functions 'shell-directory-tracker nil 'local)

  ;; Oasys prompt should be of the form `ModuleName> '.
  (setq comint-prompt-regexp "^\\(.*\\(\\.impl\\|.sign\\)\\)?>")

  ;; History syntax of comint conflicts with Haskell, e.g. !!, so better
  ;; turn it off.
  ;;(setq comint-input-autoexpand nil)
  ;;(setq comint-process-echoes nil)
  (run-hooks 'opal-oasys-hook)
  
  ;; Clear message area.
  (message ""))


(defun opal-oasys-wait-for-output ()
  "Wait until output arrives and go to the last input."
  (while (progn			
	   (goto-char opal-oasys-send-end)
	   (not (re-search-forward comint-prompt-regexp nil t)))
    (accept-process-output opal-oasys-process)))

(defun opal-oasys-send (&rest string)
  "Send `opal-oasys-process' the arguments (one or more strings).
A newline is sent after the strings and they are inserted into the
current buffer after the last output."
 ;; (opal-oasys-wait-for-output)          ; wait for prompt
  (goto-char (point-max))               ; position for this input
  (apply 'insert string)
  (comint-send-input)
  (setq opal-oasys-send-end (marker-position comint-last-input-end))
  (opal-oasys-wait-for-output))

(defun opal-oasys-start-load ()
  (switch-to-oasys)
  (opal-oasys-wait-for-output)
)

(defun opal-oasys-go-load (cd)
  "Save the current buffer and load its file into the Oasys process.

If the second argument CD is non-nil, change directory in the Oasys
process to the current buffer's directory before loading the file.

If the variable `opal-oasys-command' is set then its value will be
sent to the Oasys process after the load command. This can be used for a
top-level expression to evaluate."
  (hack-local-variables)		; in case they've changed
  (save-buffer)
  (let ((file (concat "\"" buffer-file-name "\""))	
	(dir (expand-file-name default-directory))
	(cmd (and (boundp 'opal-oasys-command)
		  opal-oasys-command
		  (if (stringp opal-oasys-command)
		      opal-oasys-command
		    (symbol-name opal-oasys-command)))))
    (if (and opal-oasys-process-buffer
	     (eq (process-status opal-oasys-process) 'run))
	;; Ensure the Oasys buffer is selected.
	(set-buffer opal-oasys-process-buffer) 
      ;; Start Oasys process.
      (opal-oasys-start-load))

    ;; Wait until output arrives and go to the last input.
    ;; load
    (opal-oasys-send "a " file)
    (opal-oasys-wait-for-output)
    (let ((focus (concat "\"" (car (reverse (split-string file "/"))))))
    (opal-oasys-send "f " focus))
    ;; ;; Error message search starts from last load command.
    (setq opal-oasys-load-end (marker-position comint-last-input-end))
    (setq opal-oasys-error-pos opal-oasys-load-end)
    ;; Wait until output arrives and go to the last input.
    (opal-oasys-wait-for-output)))


(defun opal-oasys-load-file (cd)
  "Save an oasys buffer file and load its file.
If CD (prefix argument if interactive) is non-nil, change directory in
the Oasys process to the current buffer's directory before loading the
file. If there is an error, set the cursor at the error line otherwise
show the *oasys* buffer."
  (interactive "P")
  (opal-oasys-gen-load-file cd)
  )



(defun opal-oasys-gen-load-file (cd)
  "Save an oasys buffer file and load its file or reload depending on CMD.
If CD is non-nil, change the process to the current buffer's directory
before loading the file. If there is an error, set the cursor at the
error line otherwise show the *oasys* buffer."

  ;; Execute (re)load command.
  (save-excursion (opal-oasys-go-load cd))

  ;; Show *oasys* buffer.
  (pop-to-buffer opal-oasys-process-buffer)
  (goto-char (point-max))

  ;; Something went wrong. If possible, be helpful and pinpoint the
    ;; first error in the file whilst leaving the error visible in the
    ;; *oasys* buffer.
  (if (re-search-forward
       "^ERROR" nil t)
       (goto-char opal-oasys-load-end)
       ;;(opal-oasys-locate-next-error)
    ))

 

(defun opal-oasys-show-oasys-buffer ()
  "Go to the *oasys* buffer."
  (interactive)
  (if (or (not opal-oasys-process-buffer)
          (not (buffer-live-p opal-oasys-process-buffer)))
      (opal-oasys-start-process nil))
  (pop-to-buffer  opal-oasys-process-buffer))



(defun switch-to-oasys (&optional arg)
  "Show the inferior-opal buffer.  Start the process if needed."
  (interactive "P")
  (opal-oasys-start-process arg)
  (turn-on-opal-oasys)
  (opal-oasys-show-oasys-buffer))


(provide 'opal-oasys)