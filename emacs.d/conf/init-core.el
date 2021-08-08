;;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(setq package-quickstart t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(unless package--initialized (package-initialize))

;;; Install some fundamental packages
;(defvar my-packages '(use-package general))
;(defun install-missing-packages ()
;  (let ((missing (seq-remove (lambda (p) (package-installed-p p)) my-packages)))
;    (when missing
;      (package-refresh-contents)
;      (dolist (p missing)
;        (package-install p)))))
;
;(install-missing-packages)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;;; Benchmarking tool for testing purposes
;(use-package esup
;  :commands esup)

;;; Define SMB customization group
(defgroup smb nil "SMB Customization")

;;; Useful Defaults
;(setq-default cursor-type 'bar)           ; Line-style cursor similar to other text editors
(setq inhibit-startup-screen t)           ; Disable startup screen
(setq initial-scratch-message "")         ; Make *scratch* buffer blank
(setq-default frame-title-format '("%b")) ; Make window title the buffer name
(setq ring-bell-function 'ignore)         ; Disable bell sound
(fset 'yes-or-no-p 'y-or-n-p)             ; y-or-n-p makes answering questions faster
(show-paren-mode 1)                       ; Show closing parens by default
(setq linum-format "%4d ")                ; Prettify line number format
(add-hook 'prog-mode-hook                 ; Show line numbers in programming modes
        (if (fboundp 'display-line-numbers-mode)
            #'display-line-numbers-mode
            #'linum-mode))


;;; Offload the custom-set-variables to a separate file
;;; This keeps your init.el neater and you have the option
;;; to gitignore your custom.el if you see fit.
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
;;; Load custom file. Don't hide errors. Hide success message
(load custom-file nil t)

;;; Avoid littering the user's filesystem with backups
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '((".*" . "~/.emacs.d/saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;;; Lockfiles unfortunately cause more pain than benefit
(setq create-lockfiles nil)

;;; Load wheatgrass as the default theme if one is not loaded already

;(if (not custom-enabled-themes)
;    (load-theme 'wheatgrass t))

(use-package paradox      ; Nice list-packages interface
  :ensure t
  :init
  (paradox-enable))

(use-package general      ; Nicer keybinding support with evil
  :ensure t)

(use-package undo-tree    ; Sane undo/redo behavior
  :ensure t
  :init (global-undo-tree-mode))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  (which-key-setup-minibuffer)
  ;(which-key-setup-side-window-right)
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-max-description-length 27)
  (setq which-key-add-column-padding 0)
  ;(setq which-key-max-display-columns nil))
)


(provide 'init-core)
