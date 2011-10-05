;;; opal-oasys.el --- An Oasys interaction mode

;; Purpose:
;;
;; To send an Opal buffer to another buffer running an Oasys
;; interpreter.
;;
;;
;; Installation:
;;
;; To use with opal-mode.el add this to .emacs:
;;
;;   (add-hook 'opal-mode-hook 'turn-on-opal-oasys)
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
  (local-set-key "\C-c\C-r" 'opal-oasys-reload-file)
  (local-set-key "\C-c\C-n" 'opal-oasys-locate-next-error)
  (local-set-key "\C-c\C-b" 'opal-oasys-show-oasys-buffer))

(defun turn-off-opal-oasys ()
  "Turn off Opal interaction mode with an oasys interpreter within a buffer."
  (local-unset-key  "\C-c\C-s")
  (local-unset-key  "\C-c\C-l")
  (local-unset-key  "\C-c\C-r")
  (local-unset-key  "\C-c\C-b"))

(define-derived-mode opal-oasys-mode comint-mode "Opal Oasys"
  "Major mode for interacting with an inferior Oasys session.

The commands available from within an Opal program are:
    \\<opal-mode-map>\\[opal-oasys-start-process] to create the Oasys buffer and start an Oasys process in it.
    \\[opal-oasys-load-file] to save the current buffer and load and focus it in Oasys.
    \\[opal-oasys-reload-file] to send the reload command to Oasys without saving the buffer.
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

(defvar opal-oasys-send-end nil
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
  (setq comint-prompt-regexp
	"^\\*?[[:upper:]][\\._[:alnum:]]*\\( \\*?[[:upper:]][\\._[:alnum:]]*\\)*> ")

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
	   (goto-char comint-last-input-end)
	   (not (re-search-forward comint-prompt-regexp nil t)))
    (accept-process-output opal-oasys-process)))

(defun opal-oasys-send (&rest string)
  "Send `opal-oasys-process' the arguments (one or more strings).
A newline is sent after the strings and they are inserted into the
current buffer after the last output."
  (opal-oasys-wait-for-output)          ; wait for prompt
  (goto-char (point-max))               ; position for this input
  (apply 'insert string)
  (comint-send-input)
  (setq opal-oasys-send-end (marker-position comint-last-input-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun opal-oasys-go (load-command cd)
  "Save the current buffer and load its file into the Oasys process.
The first argument LOAD-COMMAND specifies how the file should be
loaded: as a new file (\":load \") or as a reload (\":reload \").

If the second argument CD is non-nil, change directory in the GHCi
process to the current buffer's directory before loading the file.

If the variable `haskell-ghci-command' is set then its value will be
sent to the GHCi process after the load command. This can be used for a
top-level expression to evaluate."
  (hack-local-variables)		; in case they've changed
  (save-buffer)
  (let ((file (if (string-equal load-command ":load ")
		  (concat "\"" buffer-file-name "\"")
		""))
	(dir (expand-file-name default-directory))
	(cmd (and (boundp 'haskell-ghci-command)
		  haskell-ghci-command
		  (if (stringp haskell-ghci-command)
		      haskell-ghci-command
		    (symbol-name haskell-ghci-command)))))
    (if (and haskell-ghci-process-buffer
	     (eq (process-status haskell-ghci-process) 'run))
	;; Ensure the GHCi buffer is selected.
	(set-buffer haskell-ghci-process-buffer) 
      ;; Start Haskell-GHCi process.
      (haskell-ghci-start-process nil))

    (if cd (haskell-ghci-send (concat ":cd " dir)))
    ;; Wait until output arrives and go to the last input.
    (haskell-ghci-wait-for-output)
    (haskell-ghci-send load-command file)
    ;; Error message search starts from last load command.
    (setq haskell-ghci-load-end (marker-position comint-last-input-end))
    (setq haskell-ghci-error-pos haskell-ghci-load-end)
    (if cmd (haskell-ghci-send cmd))
    ;; Wait until output arrives and go to the last input.
    (haskell-ghci-wait-for-output)))

(defun haskell-ghci-load-file (cd)
  "Save a ghci buffer file and load its file.
If CD (prefix argument if interactive) is non-nil, change directory in
the GHCi process to the current buffer's directory before loading the
file. If there is an error, set the cursor at the error line otherwise
show the *ghci* buffer."
  (interactive "P")
  (haskell-ghci-gen-load-file ":load " cd))

(defun haskell-ghci-reload-file (cd)
  "Save a ghci buffer file and load its file.
If CD (prefix argument if interactive) is non-nil, change the GHCi
process to the current buffer's directory before loading the file.
If there is an error, set the cursor at the error line otherwise show
the *ghci* buffer."
  (interactive "P")
  (haskell-ghci-gen-load-file ":reload " cd))

(defun haskell-ghci-gen-load-file (cmd cd)
  "Save a ghci buffer file and load its file or reload depending on CMD.
If CD is non-nil, change the process to the current buffer's directory
before loading the file. If there is an error, set the cursor at the
error line otherwise show the *ghci* buffer."

  ;; Execute (re)load command.
  (save-excursion (haskell-ghci-go cmd cd))

  ;; Show *ghci* buffer.
  (pop-to-buffer haskell-ghci-process-buffer)
  (goto-char haskell-ghci-load-end)

  ;; Did we finish loading without error?
  (if (re-search-forward
       "^Ok, modules loaded" nil t)
      (progn (goto-char (point-max))
             (recenter 2)
             (message "There were no errors."))

    ;; Something went wrong. If possible, be helpful and pinpoint the
    ;; first error in the file whilst leaving the error visible in the
    ;; *ghci* buffer.
    (goto-char haskell-ghci-load-end)
    (haskell-ghci-locate-next-error)))


(defun haskell-ghci-locate-next-error () 
  "Go to the next error shown in the *ghci* buffer."
  (interactive)
  (if (buffer-live-p haskell-ghci-process-buffer)
      (progn (pop-to-buffer haskell-ghci-process-buffer)
	     (goto-char haskell-ghci-error-pos)
	     (if (re-search-forward
		  "^[^\/]*\\([^:\n]+\\):\\([0-9]+\\)" nil t)
		 (let ((efile (buffer-substring (match-beginning 1)
						(match-end 1)))
		       (eline (string-to-int 
			       (buffer-substring (match-beginning 2)
						 (match-end 2)))))

		   (recenter 2)
		   (setq haskell-ghci-error-pos (point))
		   (message "GHCi error on line %d of %s."
                   eline (file-name-nondirectory efile))
		   (if (file-exists-p efile)
		       (progn (find-file-other-window efile)
			      (goto-line eline)
			      (recenter))))

      ;; We got an error without a file and line number, so put the
      ;; point at end of the *ghci* buffer ready to deal with it.
               (goto-char (point-max))
               (recenter -2)
	       (message "No more errors found.")))
    (message "No *ghci* buffer found.")))     

(defun haskell-ghci-show-ghci-buffer ()
  "Go to the *ghci* buffer."
  (interactive)
  (if (or (not haskell-ghci-process-buffer)
          (not (buffer-live-p haskell-ghci-process-buffer)))
      (haskell-ghci-start-process nil))
  (pop-to-buffer  haskell-ghci-process-buffer))

(provide 'haskell-ghci)			

;; arch-tag: f0bade4b-288d-4329-9791-98c1e24167ac
;;; haskell-ghci.el ends here
