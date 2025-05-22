;;; init.el   -*- lexical-binding: t; -*-

(setq gc-cons-threshold 20000000     ; Not too big, but not too small
      gc-cons-percentage 0.1)

;; Add conf folder to the load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Limit garbage collection to idle times (use gcmh-mode)
;;   NOTE:  This might be unnecessary once the igc branch is merged in emacs
;(require 'init-gcmh)
;(gcmh-mode 1)

(setq read-process-output-max (* 1024 1024 4)) ;; Allow reading larger process outputs (improves lsp performance)
(setq package-enable-at-startup nil)          ;; Use elpaca instead of built-in package manager
(require 'elpaca-bootstrap)
(require 'cl-lib)

(use-package emacs
  :ensure nil
  :custom
  (auto-save-default nil)                     ;; Disable automatic saving of buffers
  (create-lock-files nil)                     ;; Prevent the creation of lockfiles when editing
  (delete-by-moving-to-trash t)               ;; Move deleted files to trash instead of permanently deleting them
  ;(delete-selection-mode 1)                  ;; Enable replacing selected text with typed text
  (delete-active-region t)                    ;; Deleting region goes to the kill buffer?
  (column-number-mode t)                      ;; Display the column number in the modeline
  ;(display-line-numbers-width-start t)
  ;(display-line-numbers-current-absolute -1)
  (display-line-numbers-type 'relative)
  (history-length 25)                         ;; Set the length of the command history (default: 100)
  (inhibit-startup-screen t)                  ;; Disable startup screen
  (initial-scratch-message "")                ;; Make *scratch* buffer blank
  (ispell-dictionary "en_US")                 ;; Set the default dictionary for spell checking
  (make-backup-files nil)                     ;; Disable creation of backup files
  (pixel-scroll-precision-mode t)             ;; Enable precise pixel scrolling
  (pixel-scroll-precision-use-momentum nil)   ;; Disable momentum scrolling for pixel precision
  (ring-bell-function 'ignore)                ;; Disable bell sound
  (split-width-threshold 300)                 ;; Prevent automatic window splitting if the window width exceeds 300 pixels
  (switch-to-buffer-obey-display-actions t)   ;; Make buffer switching respect display actions
  (font-lock-maximum-decoration 4)            ;; for languages that don't use treesitter, make them colorful (e.g. Racket)
  (window-divider-default-right-width 1)      ;; width in pixels of vertical divider
  (window-divider-default-bottom-width 1)     ;; width in pixels of horizontal divider
  (window-divider-default-places 'right-only)
  (require-final-newline nil)                 ;; don't require a newline to end of file
  (show-trailing-whitespace t)                ;; show trailing whitespace
  (indent-tabs-mode nil)                      ;; indent with spaces by default
  (tab-always-indent 'complete)               ;; Make the TAB key complete text instead of just indenting.
  (tab-width 4)                               ;; Set the tab width to 4 spaces.
  (treesit-font-lock-level 4)                 ;; Use advanced font locking for Treesit mode.
  (truncate-lines t)                          ;; Enable line truncation to avoid wrapping long lines.
  (use-dialog-box nil)                        ;; Disable dialog boxes in favor of minibuffer prompts.
  (use-short-answers t)                       ;; Use short answers in prompts for quicker responses (y instead of yes)
  (warning-minimum-level :emergency)          ;; Set the minimum level of warnings to display
  (x-select-enable-clipboard t)
  (x-select-enable-primary t)
  (save-interprogram-paste-before-kill t)
  (apropos-do-all t)
  (mouse-yank-at-point t)
  (visible-bell nil)                          ;; don't flash the screen
  ;(scroll-margin 3)
  (scroll-conservatively 9999)
  (auto-window-vscroll nil)
  (compilation-scroll-output 'first-error)    ;; scroll until the first error
  ;(blink-cursor-mode t)
  ;(set-cursor-color "#cccccc")
  (c-default-style "k&r")
  (c-ts-mode-indent-offset 4)                 ;; Default to 4 space indentation in C/C++
  (garbage-collection-messages t)             ;; Print a message in the messages buffer every time gc happens
  (save-place t)                              ;; Remember line when revisiting a file
  (save-place-file (expand-file-name "places" user-emacs-directory))

  :init                        ;; Initialization settings that apply before the package is loaded.
  (tool-bar-mode -1)           ;; Disable the tool bar for a cleaner interface.
  (menu-bar-mode -1)           ;; Disable the menu bar for a more streamlined look.
  (set-fringe-mode 0)
  (window-divider-mode 1)
  (set-face-foreground 'window-divider "#000000")
  (set-face-background 'window-divider "#000000")
  (when scroll-bar-mode
    (scroll-bar-mode -1))      ;; Disable the scroll bar if it is active.
  (global-hl-line-mode 1)      ;; Enable highlight of the current line
  (global-auto-revert-mode 1)  ;; Enable global auto-revert mode to keep buffers up to date with their corresponding files.
  (global-font-lock-mode 1)
  (indent-tabs-mode -1)        ;; Disable the use of tabs for indentation (use spaces instead).
  (recentf-mode 1)             ;; Enable tracking of recently opened files.
  (savehist-mode 1)            ;; Enable saving of command history.
  (save-place-mode 1)          ;; Enable saving the place in files for easier return.
  (winner-mode 1)              ;; Enable winner mode to easily undo window configuration changes.
  (xterm-mouse-mode 1)         ;; Enable mouse support in terminal mode.
  (file-name-shadow-mode 1)    ;; Enable shadowing of filenames for clarity.
  (show-paren-mode 1)          ;; Show closing parens by default
  ;(tab-bar-mode 1)            ;; Workspace tabs at the top
  ;(desktop-save-mode 1)       ;; Save last session
  (modify-coding-system-alist 'file "" 'utf-8)  ;; Set default encoding for files to utf-8
  ;(prefer-coding-system 'utf-8)
  ;(set-language-environment "UTF-8")

  :hook
  ((text-mode . display-line-numbers-mode)
   (conf-mode . display-line-numbers-mode)
   (prog-mode . (lambda ()
                  (display-line-numbers-mode 1)
                  (add-hook 'before-save-hook
                            (lambda ()
                              (delete-trailing-whitespace) nil 'local))))
   (window-setup . #'toggle-frame-fullscreen)
   (after-init . (lambda ()
                   (message "Emacs has fully loaded. This code runs after startup.")
                   ;; Insert a welcome message in the *scratch* buffer displaying loading time and activated packages.
                   (with-current-buffer (get-buffer-create "*scratch*")
                     (insert (format ";; Initialized in: %s"
                               (emacs-init-time)))))))
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file 'noerror 'nomessage)

  ;; skip over non-file buffers when cycling through buffers
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)

  ;; Set default font
  (set-face-attribute 'default nil
            :family "Inconsolata Nerd Font Mono"
            ;:family "Inconsolata Nerd Font Mono-20"
            :height 200
            :weight 'normal
            :width 'normal)

  (when (eq system-type 'darwin)       ;; Check if the system is macOS
    (setq mac-command-modifier 'meta)  ;; Set the Command key to act as the Meta key
    (set-face-attribute 'default nil :family "Inconsolata Nerd Font Mono" :height 170))

  ;; Makes Emacs vertical divisor the symbol │ instead of |.
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│)))

(setq-default frame-title-format '("%b"))        ; Make window title the buffer name





;;; My modules
(require 'init-theme)
(require 'init-evil)
(require 'init-completions)
(require 'init-searching)
(require 'init-packages)
(require 'init-keybindings)
(require 'init-treesitter)
(require 'init-languages)
(require 'init-filetree)
(require 'init-git)


;; A big contributor to startup times is garbage collection.  We up the GC
;; threshold to temporarily prevent it from running, and then reset it later
;; using a hook.
;(setq gc-cons-threshold most-positive-fixnum
;      gc-cons-percentage 0.6)

;; Set the file-name-handler to nil (because regexing is cpu intensive)
;; But, keep a ref to it so we can hook it in later
;(defvar default-file-name-handler-alist file-name-handler-alist)
;(setq file-name-handler-alist nil)
;(add-hook 'emacs-startup-hook
;  (lambda ()
;    (setq gc-cons-threshold 16777216
;          gc-cons-percentage 0.1
;          file-name-handler-alist default-file-name-handler-alist)))

;; For native comp
;(when (fboundp 'native-compile-async)
;  (setq comp-async-jobs-number 15
;        comp-deferred-compilation t
;        comp-deferred-compilation-black-list '()))

;(load (expand-file-name "~/.roswell/helper.el"))

;;; Avoid littering the user's filesystem with backups
;(setq
;   backup-by-copying t      ; don't clobber symlinks
;   backup-directory-alist (expand-file-name "saves/" user-emacs-directory)
;   delete-old-versions t
;   kept-new-versions 6
;   kept-old-versions 2
;   version-control t)       ; use versioned backups

;(setenv "LANG" "en_US.UTF-8")
;(setenv "LC_ALL" "en_US.UTF-8")


(provide 'init)

;;; init ends here
