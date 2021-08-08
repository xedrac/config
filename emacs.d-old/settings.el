;; settigns.el

; start fullscreen
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)

; remove some GUI elements
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-screen t)  ; don't show splash screen
(setq require-final-newline t)   ; add newline to end of file if necessary
(setq-default indent-tabs-mode nil)
(setq show-trailing-whitespace t); show trailing whitespace
(set-default 'truncate-lines t)  ; Don't wrap text
(setq scroll-margin 1)
(setq scroll-conservatively 9999)
(setq auto-window-vscroll nil)
(setq linum-format "%4d" column-number-mode t) ; Line number format
;(setq global-linum-mode t)                     ; Show line numbers in the buffers
(setq gc-cons-threshold 100000000)             ; make garbage collection stalls much less frequent
(setq read-process-output-max (* 1024 1024))   ; increase process read limits for better lsp performance
;(setq compilation-scroll-output 'first-error)  ; scroll until the first error


(defun my-c-setup ()
  (progn
    (c-add-style "shawn"
		'("bsd"
		  (c-basic-offset . 4)
		  (c-indent-level . 4)
		  (c-backspace-function . delete-backward-char)
		  (c-syntactic-indentation-in-macros . nil)
		  (c-tab-always-indent . t)
		  (c-hanging-braces-alist
		   (block-close . c-snug-do-while))
		  (c-offsets-alist
		   (arglist-cont-nonempty . *)
		   (statement-cont . *))
		  (indent-tabs-mode . nil)))
    (setq c-default-style "shawn")
    (c-set-style "shawn")))

(add-hook 'c-mode-hook 'my-c-setup)
(add-hook 'c++-mode-hook 'my-c-setup)

; Set autosave/backup behavior
;(defconst my-auto-save-dir (concat user-emacs-directory "backup/auto-save-tmp/"))
(defconst my-auto-save-dir "~/.emacs.d/backup/auto-save")
(setq
  ;backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/per-save/")))
  backup-directory-alist '(("" . "~/.emacs.d/backup/per-save/"))
  backup-by-copying t
  delete-old-versions t    ; delete excess backup files automatically
  delete-by-moving-to-trash t
  kept-new-versions 9      ; newest versions to keep when a new numbered backup is made (default: 2)
  kept-old-versions 6      ; oldest versions to keep when a new numbered backup is made (default: 2)
  version-control t        ; give backup files version numbers
  ;auto-save-file-name-transforms `((".*" ,my-auto-save-dir t))
  auto-save-file-name-transforms `((".*" ,my-auto-save-dir t))
  ;auto-save-list-file-prefix `((".*" ,my-auto-save-dir))
  auto-save-list-file-prefix (concat my-auto-save-dir "auto-save-")
  auto-save-default t      ; autosave every buffer that visits a file
  auto-save-timeout 20     ; default 30
  auto-save-interval 200   ; default 300
  vc-make-backup-files t)   ; backup versioned files
  ;create-lockfiles nil)    ; Don't create hidden .#<filename> lock files in source tree


; Perform both per-session saves and per-save saves
; This is useful if you leave emacs running for a long time
(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first of each emacs session
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    ;(let ((backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/per-session/")))
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session/")))
	  (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in both a per-session
  ;; and per-save backup, to keep the numbering of per-save backups consistent
  (let ((buffer-backup-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook 'force-backup-of-buffer)


; Save place in files when visiting
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")


; set exec path using $PATH
(defun set-exec-path-from-shell-PATH ()
  (let
      ((path-from-shell (replace-regexp-in-string
                            "[ \t\n]*$"
                            ""
                            (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq eshell-path-env path-from-shell) ; for eshell users
      (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))


; Taken from stack overflow for 'Save As' functionality
(defun my-write-copy-to-file ()
  "Write a copy of the current buffer or region to a file."
  (interactive)
  (let* ((curr (buffer-file-name))
         (new (read-file-name
               "Copy to file: " nil nil nil
               (and curr (file-name-nondirectory curr))))
         (mustbenew (if (and curr (file-equal-p new curr)) 'excl t)))
    (if (use-region-p)
        (write-region (region-beginning) (region-end) new nil nil nil mustbenew)
      (save-restriction
        (widen)
        (write-region (point-min) (point-max) new nil nil nil mustbenew)))))
