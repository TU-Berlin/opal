;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; opal-switch.el
;; shortcuts to switch from one OPAL structure part to a corresponding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst opal-switch-popup nil)

(defun opal-switch-menu-xemacs ()
  "set the opal-mode switch menu for XEmacs"

  (setq opal-switch-menu
	
	(list "Files/Buffers" :filter 'opal-switch-filter-curr-struct
	      ["Show both parts of " opal-switch-2 t]
	      ["Signature part of " opal-switch-to-sign
	       :included (not (opal-in-sign))]
	      ["Implementation part of " opal-switch-to-impl
	       :included (not (opal-in-impl))]
	      "-----"
	      )
   )
)

(defun opal-switch-filter-curr-struct (items)
  "add structure name to menu items which end in \"of \""

  (nconc
   ;; Menu-Eintr"age verschoenern
   (mapcar (function opal-switch-curr-struct) items)
   ;; Submenu mit allen Opal-Buffern
   (list (opal-switch-opal-buffers-menu))
   (if opal-switch-popup
       nil
     ;; Submenu mit Opal-Dateien in akt. Verzeichnis
     (list (list "Load Opal file" :filter 'opal-switch-opal-files-menu))
     )
   ;; Rest
   (list
    ["Save all Opal buffers ..." opal-ask-save-opal-buffers t]
    ["New Opal structure ..." opal-switch-new-file t]
    "-----"
    )
    ;; Opal-Defs
   (list (opal-switch-opaldefs-buffers-menu))
   )
)

(defun opal-switch-curr-struct (item)
  "add structure name to item if it is vector whose first element is a string ending in \"of \""

  (if (and (vectorp item)
	   (stringp (aref item 0))
	   (string-match "^\\(.*of \\).*$" (aref item 0)))
      (progn
	(aset item 0 (concat (substring (aref item 0) 
					(match-beginning 1) (match-end 1))
			     (opal-buffer-to-structure)))
	item
	)
    item
    )
  )

(defun opal-switch-opal-buffers-menu ()
   (cons "Switch to Opal buffer" 
	 (opal-switch-opal-buffers (buffer-list) '(major-mode . opal-mode) 'buffer-name)
	 )
)

(defun opal-switch-opaldefs-buffers-menu ()
   (cons "Switch to SysDefs / ProjectDefs buffer" 
	 (opal-switch-opal-buffers (buffer-list) 
				   '(major-mode . opal-defs-mode)
				   'buffer-file-name)
	 )
)

(defun opal-switch-opal-buffers (rest-buffers search prn-fn)
  "return a menu item list for every opal buffer"
 (if rest-buffers
     (let ((blv (buffer-local-variables (car rest-buffers)))
	    (buf (car rest-buffers))
	    )
	(if (equal (assoc 'major-mode blv) search)
	    (cons
	     (vector 
	       (concat
		(if (equal (assoc 'buffer-read-only blv) 
			   '(buffer-read-only . t))
		    "%" " ")
		(if (buffer-modified-p buf) "*" " ")
		(apply prn-fn buf nil)
		)
	       (list (function switch-to-buffer) buf)
	       t
	      )
	     (opal-switch-opal-buffers (cdr rest-buffers) search prn-fn)
	     )
	  (opal-switch-opal-buffers (cdr rest-buffers) search prn-fn)
	  )
      )
   nil
   )
)


(defun opal-switch-opal-files-menu (x)
  (if (buffer-file-name)
      (append
       (list (concat "Current dir: " (file-name-directory (buffer-file-name))))
       (mapcar (function opal-switch-opal-file) 
	       (directory-files "." t "\\(\\.sign\\|\\.impl\\)$" nil t)
	       )
       (list "----")
       (mapcar (function opal-switch-opal-dir) opal-hook-path)
       )
    ; else - no buffer-file-name
    (append
     (list "Opal files in:")
     (mapcar (function opal-switch-opal-dir) opal-hook-path)
     )
  )
)

(defun opal-switch-opal-file (fname)
  "make file name to menu item"
;  (message "adding %s to menu" fname)
  (vector (file-name-nondirectory fname) (list (function find-file) fname) t)
)

(defun opal-switch-opal-dir (dirname)
  "make submenu of dirname and its opalfiles"
  (let ((com (list 'lambda (list 'x)(list 'opal-switch-opal-dir-m dirname))))
    (list dirname :filter com)
    )
  )

(defun opal-switch-opal-dir-m (dirname)
  "make submenu of dirname and its opalfiles"
  (mapcar (function opal-switch-opal-file)
	  (directory-files dirname t "\\(\\.sign\\|\\.impl\\)$" nil t)
	  )
  )

(defun opal-switch-popup-fsfemacs ()
  "set popup-menu for changing to other structure parts / FSF Emacs."
  (interactive)
  (setq switch-keymap (make-sparse-keymap "Switch to other structure parts"))
  
  (if (not opal-novice)
      (progn
	(define-key switch-keymap [switch-to-intp] 
	  '("INTERNAL PROPERTIES part" . opal-switch-to-intp))
	(define-key switch-keymap [switch-to-extp] 
	  '("EXTERNAL PROPERTIES part" . opal-switch-to-extp))
      )
  )
  (define-key switch-keymap [switch-to-impl] 
    '("IMPLEMENTATION part" . opal-switch-to-impl))
  (define-key switch-keymap [switch-to-sign] 
    '("SIGNATURE part" . opal-switch-to-sign))
  (define-key switch-keymap [xxx] 
    '("" . nil))
  (define-key switch-keymap [switch-2]
    '("show SIGNATURE and IMPLEMENTATION" . opal-switch-2))

  (fset 'switch-menu switch-keymap)
  (define-key opal-mode-map [A-down-mouse-1] 'switch-menu)
)


(defun opal-switch-menu-fsfemacs ()
  "set the opal-mode switch menu for FSF Emacs"

  (if opal-novice
      () ; see opal-mode-set-menu for opal-novices
    (define-key opal-mode-map [menu-bar opal switch]
      (cons "Switch" (make-sparse-keymap "Switch")))

    (define-key opal-mode-map [menu-bar opal switch opal-switch-to-intp]
      '("INTERNAL PROPERTIES part" . opal-switch-to-intp))
    (define-key opal-mode-map [menu-bar opal switch opal-switch-to-extp]
      '("EXTERNAL PROPERTIES part" . opal-switch-to-extp))
    (define-key opal-mode-map [menu-bar opal switch opal-switch-to-impl]
      '("IMPLEMENTATION part" . opal-switch-to-impl))
    (define-key opal-mode-map [menu-bar opal switch opal-switch-to-sign]
      '("SIGNATURE part" . opal-switch-to-sign))
    (define-key opal-mode-map [menu-bar opal switch t]
      '("" . nil))
    (define-key opal-mode-map [menu-bar opal switch opal-switch-to-all]
      '("show SIGNATURE and IMPLEMENTATION" . opal-switch-2))
    )
)

(defun opal-switch-popup-xemacs ()
  "popup switch menu for XEmacs"
  (interactive)

  (setq opal-switch-popup t)
  (popup-menu opal-switch-menu)
  (setq opal-switch-popup nil)
)

(defun opal-mode-switch-keymap ()
  "set top opal mode switch keymap"

  (define-key opal-mode-map "\C-c\C-s\C-s" 'opal-switch-to-sign)
  (define-key opal-mode-map "\C-c\C-s\C-i" 'opal-switch-to-impl)
  (define-key opal-mode-map "\C-c\C-se" 'opal-switch-to-extp)
  (define-key opal-mode-map "\C-c\C-si" 'opal-switch-to-intp)
  (define-key opal-mode-map "\C-c\C-s\C-b" 'opal-switch-2)

  (if opal-running-xemacs
      (progn
	(opal-switch-menu-xemacs)
	(define-key opal-mode-map [(alt button1)] 'opal-switch-popup-xemacs)
	(define-key opal-mode-map [(button3)] 'opal-switch-popup-xemacs)
      )
      (opal-switch-menu-fsfemacs)
      (opal-switch-popup-fsfemacs)
  )
)

(defun opal-switch-to-sign ()
   "switches to buffer with signature of the current structure."
   (interactive)
   (find-file (concat (opal-buffer-to-structure) ".sign"))
)

(defun opal-switch-to-impl ()
   "switches to buffer with implementation of the current structure."
   (interactive)
   (find-file (concat (opal-buffer-to-structure) ".impl"))
)

(defun opal-switch-to-extp ()
   "switches to buffer with external properties of the current structure."
   (interactive)
   (find-file (concat (opal-buffer-to-structure) ".extp"))
)

(defun opal-switch-to-intp ()
   "switches to buffer with internal properties of the current structure."
   (interactive)
   (find-file (concat (opal-buffer-to-structure) ".intp"))
)

(defun opal-switch-2 ()
  "show SIGNATURE and IMPLEMENTATION part of a structure"

  (interactive)
  (find-file (concat (opal-buffer-to-structure) ".sign"))
  (find-file-other-window (concat (opal-buffer-to-structure) ".impl"))
)

(defun opal-switch-new-file (struct)
  "create new Opal file, show both parts"

  (interactive "sName of new structure:")
  (if (and (file-exists-p (concat struct ".sign"))
	   (not (y-or-n-p "Structure exists, load it? ")))
      nil
    (find-file (concat struct ".impl"))
    (find-file-other-window (concat struct ".sign"))
    )
  )

(put 'opal-switch-to-sign   'menu-enable '(null (opal-in-sign)))
(put 'opal-switch-to-impl   'menu-enable '(null (opal-in-impl)))
(put 'opal-switch-to-extp   'menu-enable '(null (opal-in-extp)))
(put 'opal-switch-to-intp   'menu-enable '(null (opal-in-intp)))

(provide 'opal-switch)

