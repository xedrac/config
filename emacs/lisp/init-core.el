;; -*- lexical-binding: t; -*-

;;; Define SMB customization group
(defgroup smb nil "SMB Customization")

;;; Useful Defaults
(setq inhibit-startup-screen t)           ; Disable startup screen
(setq initial-scratch-message "")         ; Make *scratch* buffer blank
(setq-default frame-title-format '("%b")) ; Make window title the buffer name
(setq ring-bell-function 'ignore)         ; Disable bell sound
(fset 'yes-or-no-p 'y-or-n-p)             ; y-or-n-p makes answering questions faster
(show-paren-mode 1)                       ; Show closing parens by default
(global-auto-revert-mode t)               ; Auto update buffers whose files are changed outside emacs

(setq display-line-numbers-width-start t)
(setq display-line-numbers-current-absolute -1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)

(global-auto-revert-mode)                 ; Automatically update buffer if file changes outside emacs

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))  ; remove some gui elements
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;(setq-default line-spacing 0.2)
(setq-default indent-tabs-mode nil)  ; indent with spaces by default

;;; Start fullscreen
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)
;(custom-set-variables
;  '(initial-frame-alist '((fullscreen . maximized))))

;;; Extra config
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq save-interprogram-paste-before-kill t)
(setq apropos-do-all t)
(setq mouse-yank-at-point t)
(setq inhibit-startup-screen t)      ; don't show splash screen
(setq ring-bell-function 'ignore)    ; disable that obnoxious noise
;(setq visible-bell nil)              ; don't flash the screen either
(setq scroll-margin 3)
(setq scroll-conservatively 9999)
(setq auto-window-vscroll nil)
;(setq compilation-scroll-output 'first-error)  ; scroll until the first error
(setq read-process-output-max (* 512 1024))     ; increase process read limits for better lsp performance
(setq display-line-numbers-type 'relative)
;(blink-cursor-mode t)
;(set-cursor-color "#cccccc")
(setq font-lock-maximum-decoration 2)


;;; Offload the custom-set-variables to a separate file
;;; This keeps your init.el neater and you have the option
;;; to gitignore your custom.el if you see fit.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;(unless (file-exists-p custom-file)
;  (write-region "" nil custom-file))
;;; Load custom file. Don't hide errors. Hide success message
(when (file-exists-p custom-file)
  (load custom-file))

(setq window-divider-default-right-width 1)  ; width in pixels of vertical divider
(setq window-divider-default-bottom-width 1) ; width in pixels of horizontal divider
(setq window-divider-default-places 'right-only)
(set-fringe-mode 0)
(set-face-foreground 'window-divider "#000000")
(set-face-background 'window-divider "#000000")
(window-divider-mode 1)

;;; Avoid littering the user's filesystem with backups
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist (expand-file-name "saves/" user-emacs-directory)
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;;; Lockfiles unfortunately cause more pain than benefit
;(setq create-lockfiles nil)

(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;;; Remember line when you revisit a file
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))
(setq delete-active-region t)

(setq require-final-newline t)       ; add newline to end of file if necessary
(setq show-trailing-whitespace t)    ; show trailing whitespace

;;; Delete trailing whitespace on save in programming modes
(add-hook 'prog-mode-hook
          (lambda () (add-hook 'before-save-hook
                               (lambda () (delete-trailing-whitespace)
                                nil 'local))))

;;; Save last session
;(desktop-save-mode 1)

;;; No text wrapping
(set-default 'truncate-lines t)

(setq c-default-style "k&r")
(setq c-ts-mode-indent-offset 4)


;(set-frame-font "Monospace-14" nil t)
;(set-frame-font "DejaVu Sans Mono-14" nil t)
;(set-frame-font "Liberation Mono-14" nil t)
;(set-frame-font "Source Code Pro-14" nil t)
;(set-frame-font "FiraCode NF-14" nil t)
(set-frame-font "FiraCode Nerd Font Mono" nil t)
;(set-frame-font "FiraCode Nerd Font Mono Light-14" nil t)

(provide 'init-core)
