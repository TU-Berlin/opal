;;; opal-mode.el --- An Opal editing mode    -*-coding: iso-8859-1;-*-

;;; Code:

(defgroup opal nil
  "Major mode for editing Opal programs."
  :group 'languages
  :prefix "opal-")


;; Set load-path
;;;###autoload
(add-to-list 'load-path
   (or (file-name-directory load-file-name) (car load-path)))


;; Mode maps.
(defvar opal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-z] 'switch-to-oasys)
    (define-key map [?\C-c ?\C-l] 'oasys-load-file)

    ;;(define-key map (kbd "C-c C-t") 'inferior-haskell-type)
    ;;(define-key map (kbd "C-c C-i") 'inferior-haskell-info)
    ;;(define-key map (kbd "C-c M-.") 'inferior-haskell-find-definition)
    ;;(define-key map (kbd "C-c C-d") 'inferior-haskell-find-haddock)

    (define-key map [remap delete-indentation] 'opal-delete-indentation)
    map)
  "Keymap used in Opal mode.")


;; Opal mode menu.
(easy-menu-define opal-mode-menu opal-mode-map
  "Menu for the Opal major mode."
  `("Opal"
    ["Indent line" indent-according-to-mode]
    ["Indent region" indent-region mark-active]
    ["(Un)Comment region" comment-region mark-active]
    "---"
    ["Start interpreter" switch-to-oasys]
    ["Load file" oasys-load-file]
    "---"
    ["Customize" (customize-group 'opal)]
    ))

;; Syntax table.
(defvar opal-mode-syntax-table
  ()
  "Syntax table used in Opal mode.")


;; Delete indentation.
(defun opal-delete-indentation (&optional arg)
  (delete-indentation arg))


;; Opal mode hook
(defcustom opal-mode-hook nil
  "Hook run after entering Opal mode."
  :type 'hook
  :group 'opal
  :options `(turn-on-haskell-indent
	     turn-on-font-lock))



;; The main mode functions
;;;###autoload
(define-derived-mode opal-mode fundamental-mode "Opal"
  "Major mode for editing Haskell programs.
Blank lines separate paragraphs, comments start with `-- '.
\\<opal-mode-map>

Modules can hook in via `opal-mode-hook'.  The following modules
are supported with an `autoload' command:

   `opal-indent', Semi-automatic indentation.

Module X is activated using the command `turn-on-X'.  For example,
`opal-indent' is activated using `turn-on-opal-indent'.
For more information on a module, see the help for its `X-mode'
function.  Some modules can be deactivated using `turn-off-X'.

Invokes `opal-mode-hook'."
  (set (make-local-variable 'paragraph-start) (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  ;;(set (make-local-variable 'fill-paragraph-function) 'opal-fill-paragraph)
  (set (make-local-variable 'adaptive-fill-mode) nil)
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-padding) 0)
  ;;(set (make-local-variable 'comment-start-skip) "[-{]-[ \t]*")
  (set (make-local-variable 'comment-end) "")
  ;;(set (make-local-variable 'comment-end-skip) "[ \t]*\\(-}\\|\\s>\\)")
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  
  ;; Set things up for font-lock.
  ;;(set (make-local-variable 'font-lock-defaults)
       ;;'(haskell-font-lock-choose-keywords
	 ;;nil nil ((?\' . "w") (?_  . "w")) nil
	 ;;(font-lock-syntactic-keywords
	  ;;. haskell-font-lock-choose-syntactic-keywords)
	 ;;(font-lock-syntactic-face-function
	  ;;. haskell-syntactic-face-function)
	 ;; Get help from font-lock-syntactic-keywords.
	 ;;(parse-sexp-lookup-properties . t)))
  (set (make-local-variable 'tab-width) 4))


;; Provide ourselves:

(provide 'opal-mode)
