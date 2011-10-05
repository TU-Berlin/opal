;;; opal-font-lock.el --- Font locking module for Opal Mode


;;;
;; Installation:
;; 
;; To turn font locking on for all Opal buffers under the Opal
;; mode, add this to .emacs:
;;
;;    (add-hook 'opal-mode-hook 'turn-on-opal-font-lock)
;;
;; Otherwise, call `turn-on-opal-font-lock'.
;;
;;
;; Customisation:
;;
;; The colours and level of font locking may be customised.  See the
;; documentation on `turn-on-opal-font-lock' for more details.
;;

;; All functions/variables start with
;; `(turn-(on/off)-)opal-font-lock' or `opal-fl-'.



(eval-when-compile
  (require 'opal-mode)
  (require 'cl))
(require 'font-lock)

(defcustom opal-font-lock-symbols nil
  "Display \\ and -> and such using symbols in fonts.
This may sound like a neat trick, but be extra careful: it changes the
alignment and can thus lead to nasty surprises w.r.t layout.
If t, try to use whichever font is available.  Otherwise you can
set it to a particular font of your preference among `japanese-jisx0208'
and `unicode'."
  :group 'haskell
  :type '(choice (const nil)
	         (const t)
	         (const unicode)
	         (const japanese-jisx0208)))

(defconst opal-font-lock-symbols-alist
  (append
   ;; Prefer single-width Unicode font for lambda.
   (and (fboundp 'decode-char)
	(memq opal-font-lock-symbols '(t unicode))
	(list (cons "\\" (decode-char 'ucs 955))))
   ;; The symbols can come from a JIS0208 font.
   (and (fboundp 'make-char) (fboundp 'charsetp) (charsetp 'japanese-jisx0208)
	(memq opal-font-lock-symbols '(t japanese-jisx0208))
	(list (cons "not" (make-char 'japanese-jisx0208 34 76))
	      (cons "\\" (make-char 'japanese-jisx0208 38 75))
	      (cons "->" (make-char 'japanese-jisx0208 34 42))
   ;; Or a unicode font.
   (and (fboundp 'decode-char)
	(memq opal-font-lock-symbols '(t unicode))
	(list (cons "not" (decode-char 'ucs 172))
              (cons "->" (decode-char 'ucs 8594))
	      (cons "<-" (decode-char 'ucs 8592))
	      (cons "=>" (decode-char 'ucs 8658))
              (cons "()" (decode-char 'ucs #X2205))
              (cons "==" (decode-char 'ucs #X2261))
              (cons "/=" (decode-char 'ucs #X2262))
              (cons ">=" (decode-char 'ucs #X2265))
              (cons "<=" (decode-char 'ucs #X2264))
              (cons "!!" (decode-char 'ucs #X203C))
              (cons "&&" (decode-char 'ucs #X2227))
              (cons "||" (decode-char 'ucs #X2228))
              (cons "sqrt" (decode-char 'ucs #X221A))
              (cons "pi" (decode-char 'ucs #X3C0))
	      ;;(cons "::" (decode-char 'ucs 8759))
	      (const "." (decode-char 'ucs 8728))))
  "Alist mapping Opal symbols to chars.
Each element has the form (STRING . CHAR). STRING is the Opal symbol.
CHAR is the character with which to represent this symbol.")


;; Use new vars for the font-lock faces. The indirection allows people to
;; use different faces than in other modes, as before.
(defvar opal-keyword-face 'font-lock-keyword-face)
(defvar opal-constructor-face 'font-lock-type-face)
(defvar opal-definition-face 'font-lock-function-name-face)
(defvar opal-operator-face 'font-lock-variable-name-face)
(defvar opal-default-face nil)



(defun haskell-font-lock-symbols-keywords ()
  ())


;; The font lock regular expressions.
(defun haskell-font-lock-keywords-create (literate)
  "Create fontification definitions for Opal programs.
Returns keywords suitable for `font-lock-keywords'."
  ())



(defcustom opal-font-lock-dosfop (boundp 'font-lock-doc-face)
  "If non-nil try to highlight DOSFOP comments specially."
  :type 'boolean
  :group 'opal)

(defvar opal-font-lock-seen-dosfop nil)
(make-variable-buffer-local 'opal-font-lock-seen-dosfop)

(defun opal-syntactic-face-function (state)
  "`font-lock-syntactic-face-function' for Opal."
  ())

(defconst opal-font-lock-keywords
  (opal-font-lock-keywords-create nil)
  "Font lock definitions for Opal.")


(defun opal-font-lock-defaults-create ()
  "Locally set `font-lock-defaults' for Opal."
  ())



;; The main functions.
(defun turn-on-haskell-font-lock ()
  "Turns on font locking in current buffer for Opal programs.

Changes the current buffer's `font-lock-defaults', and adds the
following variables:

   `opal-keyword-face'      for reserved keywords and syntax,
   `opal-constructor-face'  for data- and type-constructors, class names,
                               and module names,
   `opal-operator-face'     for symbolic and alphanumeric operators,
   `opal-default-face'      for ordinary code.

The variables are initialised to the following font lock default faces:

   `opal-keyword-face'      `font-lock-keyword-face'
   `opal-constructor-face'  `font-lock-type-face'
   `opal-operator-face'     `font-lock-function-name-face'
   `opal-default-face'      <default face>

Two levels of fontification are defined: level one (the default)
and level two (more colour).  The former does not colour operators.
Use the variable `font-lock-maximum-decoration' to choose
non-default levels of fontification.  For example, adding this to
.emacs:

  (setq font-lock-maximum-decoration '((opal-mode . 2) (t . 0)))

uses level two fontification for `opal-mode' and default level for
all other modes.  See documentation on this variable for further
details.

To alter an attribute of a face, add a hook.  For example, to change
the foreground colour of comments to brown, add the following line to
.emacs:

  (add-hook 'opal-font-lock-hook
      (lambda ()
          (set-face-foreground 'opal-comment-face \"brown\")))

Note that the colours available vary from system to system.  To see
what colours are available on your system, call
`list-colors-display' from emacs.

To turn font locking on for all Opal buffers, add this to .emacs:

  (add-hook 'opal-mode-hook 'turn-on-opal-font-lock)

To turn font locking on for the current buffer, call
`turn-on-opal-font-lock'.  To turn font locking off in the current
buffer, call `turn-off-opal-font-lock'.

Invokes `opal-font-lock-hook' if not nil."
  (opal-font-lock-defaults-create)
  (run-hooks 'opal-font-lock-hook)
  (turn-on-font-lock))

(defun turn-off-opal-font-lock ()
  "Turns off font locking in current buffer."
  (font-lock-mode -1))



(provide 'haskell-font-lock)
