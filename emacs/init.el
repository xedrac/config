;;; init.el   -*- lexical-binding: t; -*-

; Configure garbage collection to run less frequently
(setq gc-cons-threshold 20000000     ; 20 MB - not too big, not too small
      gc-cons-percentage 0.1)        ; Or if 10% of RAM is being used, whichever hits first

;; Add "lisp" folder to the load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Bootstrap the elpaca package manager
(setq package-enable-at-startup nil)
(require 'elpaca-bootstrap)
(require 'cl-lib)
(require 'init-functions)


;;; ==================== BUILT-IN PACKAGES ====================

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
  (split-width-threshold 200)                 ;; Minimum width (pixels?) needed for new automatic vsplit windows
                                              ;; NOTE:  This threshold may need to change for different screen resolutions...
                                              ;;        otherwise it may create new vsplits too aggressively or conservatively
                                              ;; TODO:  Make this work on a percentage of screen width instead of pixels
  (switch-to-buffer-obey-display-actions t)   ;; Make buffer switching respect display actions
  (font-lock-maximum-decoration 4)            ;; for languages that don't use treesitter, make them colorful (e.g. Racket)
  (window-divider-default-right-width 1)      ;; width in pixels of vertical divider
  (window-divider-default-bottom-width 1)     ;; width in pixels of horizontal divider
  (window-divider-default-places 'right-only)
  (require-final-newline nil)                 ;; don't require a newline to end of file
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
  (tab-bar-show 1)             ;; Only show tab bar when there's more than one tab

  :init                        ;; Initialization settings that apply before the package is loaded.
  (tool-bar-mode -1)           ;; Disable the tool bar for a cleaner interface.
  (menu-bar-mode -1)           ;; Disable the menu bar for a more streamlined look.
  (set-fringe-mode 0)
  (window-divider-mode 1)
  (set-face-foreground 'window-divider "#000000")
  (set-face-background 'window-divider "#000000")
  (when scroll-bar-mode
    (scroll-bar-mode -1))      ;; Disable the scroll bar if it is active.
  (global-hl-line-mode 0)      ;; Set to 1 to highlight cursor line
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
  (tab-bar-mode t)             ;; Workspace tabs at the top
  ;(desktop-save-mode 1)        ;; Save last session  (when using elpaca, this needs to be set just barely before desktop-read)
  (modify-coding-system-alist 'file "" 'utf-8)  ;; Set default encoding for files to utf-8

  (prefer-coding-system 'utf-8)
  ;(set-language-environment "UTF-8")

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative, try `cape-dict'.
  ;(setq text-mode-ispell-word-completion nil)

  :hook
  ((text-mode . display-line-numbers-mode)
   (conf-mode . display-line-numbers-mode)
   (prog-mode . (lambda ()
                  (setq show-trailing-whitespace t)
                  (display-line-numbers-mode 1)
                  (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)))
   (window-setup . toggle-frame-fullscreen)
   ; after-init-hook  (elpaca uses its own after-init-hook because it runs asynchronously)
   (elpaca-after-init-hook . (lambda ()
                               (message "Emacs has fully loaded. This code runs after startup.")
                               ;; Insert a welcome message in the *scratch* buffer displaying loading time and activated packages.
                               (with-current-buffer (get-buffer-create "*scratch*")
                                 (insert (format ";; Initialized in: %s"
                                           (emacs-init-time)))))))
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file 'noerror 'nomessage)
  (setq read-process-output-max (* 1024 1024 4)) ;; Allow reading larger process outputs (improves lsp performance)
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)

  ;; skip over non-file buffers when cycling through buffers
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))

  ;; Avoid littering the user's filesystem with backups
  ;(setq backup-by-copying t      ; don't clobber symlinks
  ;      backup-directory-alist (expand-file-name "saves/" user-emacs-directory)
  ;      delete-old-versions t
  ;      kept-new-versions 6
  ;      kept-old-versions 2
  ;      version-control t)       ; use versioned backups

  ;; Set default font
  (set-face-attribute 'default nil
                      :family "Inconsolata Nerd Font Mono"
                      :height 150
                      :weight 'normal
                      :width 'normal)

  ;; Makes Emacs vertical divisor the symbol │ instead of |.
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
  (setq-default frame-title-format '("%b"))                 ;; Make window title the buffer name
  (setq project-vc-extra-root-markers '("early-init.el" "Cargo.toml")))  ;; Make project.el always recognise emacs dir as a root project


(use-package bufferlo
  :ensure t
  :init
  (setq bufferlo-menu-bar-show t)
  (setq bufferlo-menu-bar-list-buffers 'ibuffer)
  (setq bufferlo-prefer-local-buffers 'tabs)
  (setq bufferlo-ibuffer-bind-local-buffer-filter t)
  (setq bufferlo-ibuffer-bind-keys t)
  :config
  (setq bufferlo-delete-frame-kill-buffers-prompt t)
  (setq bufferlo-bookmark-tab-replace-policy 'new)
  (setq bufferlo-bookmark-tab-duplicate-policy 'prompt)
  (setq bufferlo-mode-line-tab-prefix "Ⓣ")
  (setq bufferlo-mode-line-left-prefix nil)
  (setq bufferlo-mode-line-right-suffix nil)
  (setq switch-to-prev-buffer-skip-regexp
        (concat "\\` *"
                "\\(\\*\\(" ; earmuffs
                (mapconcat #'identity
                           '("Messages"
                             "Buffer List"
                             "Ibuffer"
                             "Local Buffer List" ; bufferlo
                             "scratch"
                             "Occur"
                             "Completions"
                             "Help"
                             "Warnings"
                             "Apropos"
                             "Bookmark List"
                             "Async-native-compile-log"
                             "Flymake log"
                             "ruff-format errors"
                             "vc-diff")
                           "\\|")
                "\\)\\*\\)"
                "\\|" (rx "*" (1+ anything) " Ibuffer*")
                "\\|" (rx "*helpful " (1+ anything) "*")
                "\\|" (rx "*tramp" (1+ anything) "*")
                "\\|" (rx "magit" (* anything) ": " (1+ anything))
                "\\'"))
  (setq bufferlo-kill-buffers-prompt t)
  (setq bufferlo-kill-modified-buffers-policy 'retain-modified-kill-without-file-name) ; nil 'retain-modified 'retain-modified-kill-without-file-name 'kill-modified
  (setq bufferlo-bookmark-inhibit-bookmark-point t)
  (setq bufferlo-delete-frame-kill-buffers-prompt t)
  (setq bufferlo-bookmark-frame-save-on-delete 'when-bookmarked)
  (setq bufferlo-bookmark-tab-save-on-close 'when-bookmarked)
  (setq bufferlo-close-tab-kill-buffers-prompt t)
  (setq bufferlo-bookmark-frame-load-make-frame 'restore-geometry)
  (setq bufferlo-bookmark-frame-load-policy 'prompt)
  (setq bufferlo-bookmark-frame-duplicate-policy 'prompt)
  (setq bufferlo-bookmark-tab-replace-policy 'new)
  (setq bufferlo-bookmark-tab-duplicate-policy 'prompt)
  (setq bufferlo-bookmark-tab-in-bookmarked-frame-policy 'prompt)
  (setq bufferlo-bookmark-tab-failed-buffer-policy 'placeholder)
  (setq bufferlo-bookmarks-save-duplicates-policy 'prompt)
  (setq bufferlo-bookmarks-save-frame-policy 'all)
  (setq bufferlo-bookmarks-load-tabs-make-frame t)
  (setq bufferlo-bookmarks-save-at-emacs-exit 'all)
  (setq bufferlo-bookmarks-load-at-emacs-startup 'pred)
  (setq bufferlo-bookmarks-load-at-emacs-startup-tabs-make-frame nil)
  (setopt bufferlo-bookmarks-auto-save-interval (* 60 5)) ; 5 minutes
  (setq bufferlo-bookmarks-auto-save-messages 'saved)
  (setq bufferlo-set-restore-geometry-policy 'all)
  (setq bufferlo-set-restore-tabs-reuse-init-frame 'reuse) ; nil 'reuse 'reuse-reset-geometry
  (setq bufferlo-set-restore-ignore-already-active 'prompt) ; nil 'prompt 'ignore
  (setq bufferlo-frameset-restore-geometry 'bufferlo)
  (setq bufferlo-frame-geometry-function #'bufferlo-frame-geometry-default)
  (setq bufferlo-frame-sleep-for 0.3)

  (setq bookmark-bmenu-type-column-width 12) ; supported in Emacs 31 (innocuous on earlier versions)

  (setq bufferlo-bookmark-buffers-exclude-filters
        (list
         (rx bos " " (1+ anything)) ; ignores "invisible" buffers; e.g., " *Minibuf...", " markdown-code-fontification:..."
         (rx bos "*" (1+ anything) "*") ; ignores "special" buffers; e.g;, "*Messages*", "*scratch*", "*occur*"
         ))

  (setq bufferlo-bookmark-buffers-include-filters
        (list
         (rx bos "*shell*") ; comment out shells if you do not have bookmark support
         (rx bos "*" (1+ anything) "-shell*") ; project.el shell buffers
         (rx bos "*eshell*")
         (rx bos "*" (1+ anything) "-eshell*") ; project.el eshell buffers
         ))

  (defun my/bufferlo-bookmarks-save-p (bookmark-name)
    (string-match-p (rx "=as") bookmark-name))
  (setq bufferlo-bookmarks-save-predicate-functions nil) ; clear the save-all predicate
  (add-hook 'bufferlo-bookmarks-save-predicate-functions #'my/bufferlo-bookmarks-save-p)

  (defun my/bufferlo-bookmarks-load-p (bookmark-name)
    (string-match-p (rx "=al") bookmark-name))
  (add-hook 'bufferlo-bookmarks-load-predicate-functions #'my/bufferlo-bookmarks-load-p)

  (defvar my:bufferlo-consult--source-local-buffers
    (list :name "Bufferlo Local Buffers"
          :narrow   ?l
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                                :predicate #'bufferlo-local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))
    "Local Bufferlo buffer candidate source for `consult-buffer'.")

  (defvar my:bufferlo-consult--source-other-buffers
    (list :name "Bufferlo Other Buffers"
          :narrow   ?o
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items    (lambda () (consult--buffer-query
                                :predicate #'bufferlo-non-local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))
    "Non-local Bufferlo buffer candidate source for `consult-buffer'.")

  (defvar my:bufferlo-consult--source-all-buffers
    (list :name "Bufferlo All Buffers"
          :narrow   ?a
          :hidden   t
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items    (lambda () (consult--buffer-query
                                :sort 'visibility
                                :as #'buffer-name)))
    "All Bufferlo buffer candidate source for `consult-buffer'.")

  ;; add in the reverse order of display preference
  (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-all-buffers)
  (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-other-buffers)
  (add-to-list 'consult-buffer-sources 'my:bufferlo-consult--source-local-buffers)

  (bufferlo-mode)
  (bufferlo-anywhere-mode))



;; Fix desktop save mode restore when using elpaca package manager
;(use-package desktop
;  :ensure nil
;  :demand t
;  :hook ((elpaca-after-init-hook . (lambda ()
;                                    (desktop-save-mode 1) ; Enable the mode right before
;                                    (let ((key "--no-desktop"))
;                                      (when (member key command-line-args)
;                                        (setq command-line-args (delete key command-line-args))
;                                        (desktop-save-mode 0)))
;                                    (when desktop-save-mode
;                                      (desktop-read)
;                                      (setq inhibit-startup-screen t))))))

;; Make emacsclient start fullscreen
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-frame-parameter frame 'fullscreen 'fullboth)))


;;; WINDOW
;; This section configures window management in Emacs, enhancing the way buffers
;; are displayed for a more efficient workflow. The `window' use-package helps
;; streamline how various buffers are shown, especially those related to help,
;; diagnostics, and completion.
(use-package window
  :ensure nil       ;; This is built-in, no need to fetch it.
  :custom
  (display-buffer-alist
   '(
      ("\\*.*e?shell\\*|*terminal*"
       (display-buffer-in-side-window)
       (window-height . 0.35)
       (side . bottom)
       (slot . -1))

     ;("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
     ; (display-buffer-in-side-window)
     ; (window-height . 0.25)
     ; (side . bottom)
     ; (slot . 0))

     ;; Example configuration for the LSP help buffer,
     ;; keeps it always on bottom using 25% of the available space:
     ;("\\*\\(lsp-help\\)\\*"
     ; (display-buffer-in-side-window)
     ; (window-height . 0.25)
     ; (side . bottom)
     ; (slot . 0))

     ;; Configuration for displaying various diagnostic buffers on
     ;; bottom 25%:
     ;("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
     ; (display-buffer-in-side-window)
     ; (window-height . 0.25)
     ; (side . bottom)
     ; (slot . 1))
      )))

;; Make tabs into project-specific workspaces
;(use-package tabspaces
;  :ensure (:host github :repo "mclear-tools/tabspaces")
;  :hook (elpaca-after-init-hook . tabspaces-mode)
;  :commands (tabspaces-switch-or-create-workspace
;             tabspaces-open-or-create-project-and-workspace)
;  :custom
;  (tabspaces-use-filtered-buffers-as-default t)
;  (tabspaces-default-tab "Default")
;  (tabspaces-remove-to-default t)
;  (tabspaces-include-buffers '("*scratch*"))
;  (tabspaces-initialize-project-with-todo t)
;  (tabspaces-todo-file-name "project-todo.org")
;  ;; sessions
;  (tabspaces-session t)
;  (tabspaces-session-auto-restore t)
;  (tab-bar-new-tab-choice "*scratch*"))


;; Built-in file manager
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-lah --group-directories-first")  ;; Display files in a human-readable format and group directories first.
  (dired-dwim-target t)                                      ;; Enable "do what I mean" for target directories.
  (dired-guess-shell-alist-user
   '(;("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open") ;; Open image files with `feh' or the default viewer.
     ;("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open") ;; Open audio and video files with `mpv'.
     (".*" "open" "xdg-open")))                              ;; Default opening command for other files.
  (dired-kill-when-opening-new-dired-buffer t))               ;; Close the previous buffer when opening a new `dired' instance.
  ;:config
  ;(when (eq system-type 'darwin)
  ;  (let ((gls (executable-find "gls")))
  ;    (when gls
  ;      (setq insert-directory-program gls)))))

;; Incremental search
;; TODO:  How is this different that evil's / search?
;(use-package isearch
;  :ensure nil
;  :config
;  (setq isearch-lazy-count t)                  ;; Enable lazy counting to show current match information.
;  (setq lazy-count-prefix-format "(%s/%s) ")   ;; Format for displaying current match count.
;  (setq lazy-count-suffix-format nil)          ;; Disable suffix formatting for match count.
;  (setq search-whitespace-regexp ".*?")        ;; Allow searching across whitespace.
;  :bind (("C-s" . isearch-forward)             ;; Bind C-s to forward isearch.
;         ("C-r" . isearch-backward)))          ;; Bind C-r to backward isearch.


;; Eldoc provides helpful inline documentation for functions and variables
;; in the minibuffer, enhancing the development experience. It can be particularly useful
;; in programming modes, as it helps you understand the context of functions as you type.
;; This package is built-in, so there's no need to fetch it separately.
;; The following line enables Eldoc globally for all buffers.
;; TODO:  Does this conflict with devdocs?
(use-package eldoc
  :ensure nil
  :init
  (global-eldoc-mode))


;;; Live feedback on warnings/errors in your code
;(use-package flymake
;  :ensure nil
;  :hook (rust-mode rust-ts-mode python-mode python-ts-mode haskell-mode haskell-ts-mode c-mode c-ts-mode c++-mode c++-ts-mode)
;  :custom
;  (flymake-margin-indicators-string
;   '((error "!»" compilation-error)
;     (warning "»" compilation-warning)
;     (note "»" compilation-info)))
;  (use-package flymake-bashate :ensure t)
;  (use-package FlymakeHaskell :ensure t)
;  (use-package FlymakeLua :ensure t)
;  (use-package FlymakeElisp :ensure t)
;)


;; Note organizer
;(use-package org
;  :ensure nil
;  :defer t)


;; Popup contextual keybindings
(use-package which-key
  :ensure nil
  :defer t
  :hook
  (elpaca-after-init-hook . which-key-mode))


;; Show recent commands/files at the top
(require 'savehist)
(savehist-mode)


;; ERC (Emacs Relay Chat) is a built-in IRC client
(use-package erc
  :ensure nil
  :defer t
  :custom
  (erc-join-buffer 'window)                                        ;; Open a new window for joining channels.
  (erc-hide-list '("JOIN" "PART" "QUIT"))                          ;; Hide messages for joins, parts, and quits to reduce clutter.
  (erc-timestamp-format "[%H:%M]")                                 ;; Format for timestamps in messages.
  (erc-autojoin-channels-alist '((".*\\.libera\\.chat" "#emacs"))));; Automatically join the #emacs channel on Libera.Chat.





;;; ==================== EXTERNAL PACKAGES ====================


;; Limit garbage collection to idle times (use gcmh-mode)
;;   NOTE:  This might be unnecessary once the igc branch is merged in emacs
;(require 'init-gcmh)
;(gcmh-mode 1)


(use-package atom-one-dark-theme
  ;:ensure (:host github :repo "jonathanchu/atom-one-dark-theme")
  :ensure (:host github :repo "xedrac/atom-one-dark-theme")  ; warmer version of the above theme
  :config
  (load-theme 'atom-one-dark t))


(use-package nerd-icons
  :ensure t)
  ;:init
  ;(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)


;; Nice modeline to match my neovim one
(use-package telephone-line
  :ensure (:host github :repo "dbordak/telephone-line")
  :config
    (telephone-line-mode 1))


;; Automatically enable <lang>-ts-mode for appropriate modes
(use-package treesit-auto
  :ensure (:host github :repo "renzmann/treesit-auto")
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


(use-package eglot
  :ensure nil
  :init
  (setq eglot-ignored-modes '(lisp-interaction-mode))
  :config
  (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pyright" "--stdio")))
  (add-to-list 'eglot-server-programs '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd" "--background-index")))
  ;(add-to-list 'eglot-server-programs '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd" "--compile-commands-dir=\"build\")))
  (add-to-list 'eglot-server-programs '((haskell-mode haskell-ts-mode) . ("haskell-language-server-wrapper" "--lsp")))
  ;(add-to-list 'eglot-server-programs '((js-mode typescript-mode ts-mode) . ("typescript-language-server" "--stdio")))
  ;(add-to-list 'eglot-server-programs '((java-mode java-ts-mode) . ("jdtls")))
  ;(add-to-list 'eglot-server-programs '((go-mode) . ("gopls")))
  ;(add-to-list 'eglot-server-programs '((web-mode html-mode css-mode) . ("vscode-html-language-server" "--stdio")))
  :hook
  ((rust-ts-mode python-ts-mode c-ts-mode c++-ts-mode haskell-ts-mode) . eglot-ensure))

;;; Completion ui in minibuffer
(use-package vertico
  :ensure t
  :bind (:map minibuffer-local-map
          ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  ;(vertico-resize t)
  (setq vertico-count 25)
  (setq vertico-scroll-margin 0)
  :init
  (vertico-mode)
  :config
  (setq vertico-count 25))


;;; Add context to results in the margins (file permissions, dates, help info, etc...)
(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))


;;; Completion at point popups in buffers
(use-package corfu
  :ensure t
  :init
  ;(global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  :hook (prog-mode . corfu-mode) ;shell-mode eshell-mode)
  :custom
  (eglot-stay-out-of '(completion-at-point))
  (corfu-auto t)                ;; Enable auto completion
  (corfu-auto-delay 0.2)        ;; seconds before completion popup is shown
  (corfu-auto-prefix 1)         ;; num chars required to show completions
  (corfu-cycle t))              ;; Enable cycling for `corfu-next/previous'

;;; Much nicer icons for corfu completions
(use-package kind-icon
  :ensure t
  :after corfu
  ;:custom
  ; (kind-icon-blend-background t)
  ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode))


;;; vim emulation
(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-keybinding nil)
  ;(setq evil-want-C-u-scroll t)
  ;(setq evil-shift-width 4)
  ;(setq evil-search-module 'evil-search)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-motion-state-cursor '("orange" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("magenta" box))
  (setq evil-insert-state-cursor '("blue" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))
    ;; Center active search highlight
  (advice-add 'evil-search-next :after
    (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
  (advice-add 'evil-search-previous :after
    (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos)))))


;;; evil keybindings for other parts of emacs
(use-package evil-collection
  :ensure
  :after evil
  :config
  (evil-collection-init))


;;; Use escape (or custom escape sequence e.g. "jk"), to escape from everything
(use-package evil-escape
  :ensure
  :after evil)

;;; Quick way to move or target something with 2-char sequence
(use-package evil-snipe
  :ensure
  :after evil
  :config
  (evil-snipe-override-mode 1))

;;; Quickly change surrounding tags/quotes etc...
(use-package evil-surround
  :ensure
  :after evil
  :config
  (global-evil-surround-mode 1))

;;; Align things like a list of key=value lines to have the = signs all line up on the same column
(use-package evil-lion
  :ensure
  :config
  (evil-lion-mode))

(use-package vimish-fold
  :ensure
  :after evil)

;;; Provides ripgrep and file finding
(use-package consult
  :ensure t
  :demand t
  :hook
  ((completion-list-mode . consult-preview-at-point-mode)
   (consult-after-jump . font-lock-mode))
  :init
  (setq register-preview-delay 0.01
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        read-file-name-completion-ignore-case t  ; ignore case when searching filenames
        read-buffer-completion-ignore-case t     ; ignore case when grepping buffers
        completion-ignore-case t)                ; ignore case on completions
  (setq consult-fd-args "fd --color=never --type f --hidden --exclude \\#*\\#")  ; Don't show temp files in search find results
  :config
  (setq consult-buffer-sources '(consult--source-file))  ; only show file-backed buffers in the list
  ;(setq consult-ripgrep-args "rg --null --line-buffered --color=always --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip")

  (defun consult-project-files-with-preview ()
    "Show all project files immediately"
    (interactive)
    (consult--read (project-files (project-current t))
                   :prompt "Project file: "
                   :category 'file
                   :state (consult--file-state)
                   :require-match t))

  ;; Emulate telescope.vim by showing file live previews
  ; TODO  Showing relative filenames doesn't work because consult can't open/preview the files...
  ;       Can I add the project root back on when I go to open the file?
  ;(defun consult-list-all-project-files ()
  ;  "Show all project files immediately with live preview"
  ;  (interactive)
  ;  (let* ((prj (project-current t))
  ;         (files (project-files prj))
  ;         (root (expand-file-name (project-root prj)))
  ;         (relativefiles (mapcar (lambda (file)
  ;                                  (string-remove-prefix root file))
  ;                                files)))
  ;    ;(consult--read (project-files (project-current t))
  ;    (consult--read relativefiles
  ;                   :prompt "Project file: "
  ;                   :category 'file
  ;                   :state (consult--file-state)
  ;                   :require-match t)))

  (consult-customize
    consult-theme :preview-key '(:debunce 0.1 any)
    consult-ripgrep consult-git-grep consult-grep
    consult-bookmark consult-recent-file consult-xref
    consult--source-bookmark consult--source-file-register
    consult--source-recent-file consult--source-project-recent-file
    :preview-key '(:debounce 0.0 any)))


;;; Allow specifying substrings instead of having to tab complete from the beginning in Vertico
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;; Contextual actions for vertico (delete, rename, etc...)
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-," . embark-dwim)
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


;;; Contextual actions for consult stuff too
(use-package embark-consult
  :ensure t   ; only need to install it, embark loads it after consult if found
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package org
  :ensure t)

(use-package org-modern
  :ensure t
  :after 'org
  :config
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-agenda-tags-column 0
        org-ellipsis "…"))

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;;
;;; git changes indicators per line in the gutter
(use-package diff-hl
  :defer t
  :straight t
  :ensure t
  :hook
  (find-file . (lambda ()
                 (global-diff-hl-mode)           ;; Enable Diff-HL mode for all files.
                 (diff-hl-flydiff-mode)          ;; Automatically refresh diffs.
                 (diff-hl-margin-mode)))         ;; Show diff indicators in the margin.
  :custom
  (diff-hl-side 'left)                           ;; Set the side for diff indicators.
  (diff-hl-margin-symbols-alist '((insert . "│") ;; Customize symbols for each change type.
                                  (delete . "-")
                                  (change . "│")
                                  (unknown . "?")
                                  (ignored . "i"))))

(use-package transient
  :ensure (:host github :repo "magit/transient"))

(use-package magit
  :ensure (:host github :repo "magit/magit"))


(use-package haskell-mode
  :ensure t)

;; Scheme REPL integration
(use-package geiser-guile
  :ensure t
  :config (setq geiser-guile-binary "guile3.0"))

(use-package racket-mode
  :ensure t
  :hook (racket-mode . (lambda ()
                         (racket-xp-mode)
                         (font-lock-mode)))
  :bind (:map racket-mode-map
              ("<f5>" . racket-run))
  :config
  ;(setq racket-completion-min-chars 1)
  ;(setq racket-xp-completion-min-chars 1)
  (setq tab-always-indent 'complete))


;(use-package clojure-mode
;  :ensure t)
;
;(use-package cider
;  :ensure)

;(use-package protobuf-mode
;  :ensure t
;  :hook (protobuf-mode-hook . (lambda () (protobuf-mode))))
(use-package protobuf-ts-mode
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package cargo
  :ensure t)

;; Format on save for rust files
;(add-hook 'rust-ts-mode-hook
;          (lambda ()
;             (add-hook 'before-save-hook 'cargo-process-fmt nil 'local))) ; local save hook for rust


;; Make inactive windows slightly dimmer
(use-package dimmer
  :ensure t
  :config (dimmer-mode t))

;; Show each matching paren pair as a different color to make them easier to parse visually
;; TODO:  Still not sure if I like this mode
(use-package rainbow-delimiters
  :ensure t
  :hook ((racket-mode . rainbow-delimiters-mode)
         (lisp-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)
         (scheme-mode . rainbow-delimiters-mode)))

(use-package diredc
  :defer t)

;;; Project tree viewer
;;; TODO:  Just use dired instead...?
(use-package treemacs
  :ensure t
  ;:defer t
  :bind
  ;(kbd "C-0") 'neotree-toggle
  ("C-0" . treemacs)
  :config
  (setq treemacs-filewatch-mode t
        treemacs-follow-mode t
        treemacs-tab-bar t
        ;treemacs-add-and-display-current-directory t
        ;treemacs-display-current-directory-exclusively t
        treemacs-display-current-project-exclusively t
        treemacs-project-follow-mode t
        treemacs-file-follow-delay 0.0
        treemacs-follow-after-init t
        treemacs-expand-after-init t
        treemacs-litter-directories '("/.venv" "/build" "/.cache" "/eln-cache")
        treemacs-show-cursor nil
        treemacs-wide-toggle-width 70
        treemacs-width 40
        treemacs-move-files-by-mouse-dragging t
        treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-missing-project-action 'ask
        treemacs-find-workspace-method 'find-for-file-or-pick-first
        treemacs-collapse-dirs 3
        treemacs-project-follow-cleanup t
        treemacs-git-commit-diff-mode t
        treemacs-git-mode 'simple)
  (treemacs-resize-icons 18))
  ;:hook (treemacs-mode . (lambda () (set-face-background 'treemacs-window-background-face "#232326"))))

(use-package treemacs-evil
  :after treemacs
  :ensure t)

(use-package treemacs-magit
  :after '(treemacs magit)
  :ensure t)


(use-package buffer-move
  :ensure t)


;;; Visualize the undo tree (vundo)
(use-package vundo
  :ensure t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))


;; Indent code somewhat sanely when pasting
(use-package snap-indent
  :ensure t
  :hook (prog-mode . snap-indent-mode)
  :custom ((snap-indent-format '(untabify delete-trailing-whitespace))
           (snap-indent-on-save nil)))


(use-package parinfer-rust-mode
  :ensure (:host github :repo "justinbarclay/parinfer-rust-mode")
  :hook (emacs-lisp-mode lisp-mode))


(use-package gptel
  :ensure t
  :bind (("C-c C-a r" . gptel-add)
         ("C-c C-a f" . gptel-add-file))
  :config
  (setq gptel-default-mode 'org-mode
        gptel-model 'qwen3-coder:latest
        ;gptel-model 'mistral:latest
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "192.168.7.101:1234"
                        :stream t
                        ;:models '(codestral:latest)
                        ;:models '(mistral:latest)
                        :models '(qwen3-coder:latest)))
  :hook (gptel-mode . (lambda ()
                        (interactive)
                        (switch-to-buffer-other-window "*Ollama*")
                        (setq-local gptel-auto-scroll t))))

  ;;; Make sure it scrolls to bottom when content changes
  ;(add-hook 'gptel-response-hook
  ;        (lambda ()
  ;          (when (eq (current-buffer) (get-buffer "*Ollama*"))
  ;            (goto-char (point-max))
  ;            (recenter -1)))))

;(use-package aidermacs
;  :bind (("C-c a" . aidermacs-transient-menu))
;  :config
;  ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
;  (setenv "ANTHROPIC_API_KEY" "sk-...")
;  ; defun my-get-openrouter-api-key yourself elsewhere for security reasons
;  (setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
;  :custom
;  ; See the Configuration section below
;  (aidermacs-default-chat-mode 'architect)
;  (aidermacs-default-model "sonnet"))

;(use-package vterm
;  :ensure t)

;;; Benchmarking tool for testing purposes
;;(use-package esup
;;    :config
;;    ;; Workaround an issue with esup and byte-compiled cl-lib
;;    ;; but this also seems to make it less useful
;;    (setq esup-depth 0)

(defun cargo-process-build-and-focus ()
  "Run cargo-process-build and focus the buffer."
  (interactive)
  (call-interactively 'cargo-process-build)
  (when (get-buffer "*Cargo Build*")
    (pop-to-buffer "*Cargo Build*")))

(defun cargo-process-run-and-focus ()
  "Run cargo-process-run and focus the buffer."
  (interactive)
  (call-interactively 'cargo-process-run)
  (when (get-buffer "*Cargo Run*")
    (pop-to-buffer "*Cargo Run*")))

(defun cargo-process-test-and-focus ()
  "Run cargo-process-test and focus the buffer."
  (interactive)
  (call-interactively 'cargo-process-test)
  (when (get-buffer "*Cargo Test*")
    (pop-to-buffer "*Cargo Test*")))

(with-eval-after-load 'rust-ts-mode
  (define-key rust-ts-mode-map (kbd "C-c c") 'cargo-process-build-and-focus)
  (define-key rust-ts-mode-map (kbd "C-c r") 'cargo-process-run-and-focus)
  (define-key rust-ts-mode-map (kbd "C-c t") 'cargo-process-test-and-focus))


(with-eval-after-load 'evil
  (evil-set-leader 'normal (kbd "SPC"))

  (evil-define-key '(normal motion visual insert) 'global
    (kbd "C-s")  '(lambda () (interactive) (save-buffer t))
    (kbd "C-S")  '(lambda () (interactive) (save-some-buffers t))
    (kbd "C-+")  'text-scale-increase
    (kbd "C--")  'text-scale-decrease
    (kbd "<f8>") 'my/toggle-terminal-window)


  (evil-define-key '(normal motion visual) 'global
    ;"/" 'consult-line
    ";" 'evil-ex
    ":" 'evil-repeat-find-char)

  (evil-define-key 'normal 'global
    ; misc
    ;"SPC" 'execute-extended-command
    (kbd "<leader>.")   'project-find-file
    (kbd "<leader>,")   'consult-project-buffer
    ;(kbd "<leader>'")   '(lambda () (interactive) (term "/bin/bash"))
    ;(kbd "<leader>?")   'general-describe-keybindings

    ;; Switch to init.el buffer if it's open, or open the file if it's not
    (kbd "<leader>`")   '(lambda () (interactive)
                           (let ((buffer (get-buffer "init.el")))
                             (if buffer
                                 (switch-to-buffer buffer)
                               (find-file (expand-file-name "init.el" user-emacs-directory)))))


    ;; Jump to the term buffer or exec a new term
    (kbd "<leader>T")   '(lambda () (interactive)
                           (let ((buffer (get-buffer "*terminal*")))
                             (if buffer
                                 (switch-to-buffer-other-window buffer)
                               (term "/bin/bash"))))


                                        ;(switch-to-buffer "init.el"))  ;(dired user-emacs-directory))
    (kbd "<leader>R")   '(lambda() (interactive) (load-file user-init-file))
    (kbd "<leader>eb")  'eval-buffer
    (kbd "<leader>es")  'eval-last-sexp
    (kbd "<leader>ee")  'eval-expression
    (kbd "<leader>ef")  'eval-defun
    (kbd "<leader>c")   'comment-line
    (kbd "<leader>C")   'comment-region
    ;(kbd "<leader>0")   '(lambda () (interactive) (serial-term "/dev/ttyUSB0" 115200))
    ;(kbd "<leader>1")   '(lambda () (interactive) (serial-term "/dev/ttyUSB1" 115200))
    ;(kbd "<leader>2")   '(lambda () (interactive) (serial-term "/dev/ttyUSB2" 115200))

    ; file s
    (kbd "<leader>-") 'consult-locate
    ;(kbd "<leader>ou" 'project-find-file ;'consult-fd
    (kbd "<leader>of") 'find-file
    (kbd "<leader>ou") 'consult-project-files-with-preview  ;'consult-list-all-project-files ;'consult-fd
    (kbd "<leader>oi") 'consult-ripgrep
    (kbd "<leader>og") 'consult-git-grep
    (kbd "<leader>oe") 'consult-buffer

    ; buffers
    (kbd "<leader>ba") 'switch-to-buffer
    (kbd "<leader>bb") 'consult-project-buffer
    (kbd "<leader>bl") 'list-buffers
    (kbd "<leader>bN") 'evil-buffer-new
    (kbd "<leader>bd") '(lambda () (interactive) (kill-buffer (current-buffer)))  ; this works more reliably than 'kill-this-buffer
    (kbd "<leader>bq") '(lambda () (interactive) (kill-buffer))
    (kbd "<leader>bn") 'next-buffer  ;'evil-next-buffer
    (kbd "<leader>bp") 'previous-buffer  ;'evil-prev-buffer
    (kbd "<leader>br") 'mode-line-other-buffer   ; switch back to the most recently viewed buffer
    (kbd "<leader>bs") 'save-buffer
    (kbd "<leader>bS") '((lambda () (interactive) (save-some-buffers t))) ; :which-key "save all")
    (kbd "<leader>bH") 'buf-move-left
    (kbd "<leader>bJ") 'buf-move-down
    (kbd "<leader>bK") 'buf-move-up
    (kbd "<leader>bL") 'buf-move-right
    (kbd "<leader>bC") 'my-write-copy-to-file

    ; windows (prefix: w)
    (kbd "<leader>wv") 'evil-window-vsplit
    (kbd "<leader>ws") 'evil-window-split
    (kbd "<leader>wn") 'evil-window-next
    (kbd "<leader>wp") 'evil-window-prev
    (kbd "<leader>wq") 'delete-window
    (kbd "<leader>w>") '(lambda () (interactive) (evil-window-increase-width 10))
    (kbd "<leader>w<") '(lambda () (interactive) (evil-window-decrease-width 10))
    (kbd "<leader>w+") '(lambda () (interactive) (evil-window-increase-height 10))
    (kbd "<leader>w-") '(lambda () (interactive) (evil-window-decrease-height 10))

    ; frames/monitors (prefix: m)
    (kbd "<leader>mn") 'other-frame

    ; project
    (kbd "<leader>p") 'project-switch-project
    ;(kbd "<leader>P") 'tabspaces-open-or-create-project-and-workspace
    ;(kbd "<leader>P") 'tabspaces-switch-or-create-workspace
    ;(kbd "<leader>pf") 'counsel-projectile-find-file
    ;(kbd "<leader>pd") 'counsel-projectile-find-dir
    ;(kbd "<leader>pg") 'counsel-projectile-grep
    ;(kbd "<leader>pa") 'counsel-projectile-ag
    ;(kbd "<leader>pr") 'counsel-projectile-rg
    ;(kbd "<leader>ps") '(lambda () (interactive) (counsel-projectile-ag "-s"))
    ;(kbd "<leader>pb") 'counsel-projectile-switch-to-buffer

    ; tabs
    (kbd "<leader>tw") 'whitespace-mode
    (kbd "<leader>tn") 'tab-next
    (kbd "<leader>tp") 'tab-previous
    (kbd "<leader>tN") 'my/new-project-tab  ;tab-new
    (kbd "<leader>tq") 'tab-close

    ; AI
    (kbd "<leader>l") '(lambda () (interactive) (gptel "*Ollama*"))

    ; help
    (kbd "<leader>hp") '(lambda () (interactive)
                          (eldoc))
                          ;(let ((b1 (get-buffer "*eldoc*"))
                          ;      (b2 (get-buffer "*eldoc for interactive*")))
                          ;  (if b2
                          ;      (switch-to-buffer-other-window b2)
                          ;    (if b1
                          ;        (switch-to-buffer-other-window b1)
                          ;      (message "No eldoc buffer")))))

    ;(kbd "<leader>hp") 'describe-point
    (kbd "<leader>hf") 'describe-function ;'counsel-describe-function
    (kbd "<leader>hv") 'describe-variable ;'counsel-describe-variable
    (kbd "<leader>hk") 'describe-key

    ;; eglot/lsp symbol stuff
    ;(kbd "<leader>s,") 'xref-find-definitions
    ;(kbd "<leader>s.") 'xref-find-definitions-other-window
    (kbd "<leader>so") 'xref-go-back
    (kbd "<leader>sr") 'xref-find-references
    (kbd "<leader>sc") 'xref-find-references-and-replace
    (kbd "<leader>-") 'xref-find-definitions
    (kbd "<leader>_") 'xref-find-definitions-other-window))
    ;(kbd "<leader>sp") 'lsp-ui-peek-find-definitions
    ;(kbd "<leader>s'") 'lsp-ui-peek-find-references
    ;(kbd "<leader>s,") 'lsp-ui-peek--goto-xref-other-window
    ;;(kbd "<leader>s,") 'lsp-ui-peek--goto-xref
    ;(kbd "<leader>sr") 'lsp-rename

    ;; chat/irc/slack
    ;"(kbd <leader>if") '((lambda () (interactive) (irc-freenode-connect)) :which-key "Freenode")
    ;"(kbd <leader>ib") '((lambda () (interactive) (erc-switch-to-buffer)) :which-key "ERC switch buffer")

    ; racket
    ;"(kbd <leader>rR") 'racket-run
    ;"(kbd <leader>rr") 'racket-run-and-switch-to-repl
    ;"(kbd <leader>rp") 'racket-repl
    ;"(kbd <leader>re") 'racket-repl-switch-to-edit
    ;"(kbd <leader>rd") 'racket-doc

    ; rust
    ;; (kbd "<leader>rD") '((lambda () (interactive)
        ;; (let ((rustdir "~/projects/rust/"))
          ;; (progn (cd rustdir)
           ;; (setq default-directory rustdir))))
      ;; :which-key "cd ~/projects/rust")
    ;; (kbd "<leader>rn") 'cargo-process-new
    ;; (kbd "<leader>rr") 'cargo-process-run
    ;; (kbd "<leader>rd") 'cargo-process-doc
    ;; (kbd "<leader>rt") 'cargo-process-test
    ;; (kbd "<leader>rf") 'cargo-process-format
    ;; (kbd "<leader>rc") 'cargo-process-clean
    ;; (kbd "<leader>rb") 'cargo-process-build

    ;  ; magit
    ;  (kbd "<leader>md") 'magit-diff-unstaged
    ;  (kbd "<leader>mD") 'magit-diff-buffer-file
    ;  (kbd "<leader>mg") 'magit-diff-staged
    ;  (kbd "<leader>ml") 'magit-log-all
    ;  (kbd "<leader>ms") 'magit-status
    ;  (kbd "<leader>mc") 'magit-branch-checkout
    ;  (kbd "<leader>mb") 'magit-blame


;; Allow M-x in serial term
(eval-after-load 'term
  '(define-key term-raw-map (kbd "M-x") #'execute-extended-command))

(defun my/new-project-tab ()
  "Create a new tab for a project, such that emacs restricts certain commands to only see that's project's buffers."
  (interactive)
  (tab-bar-new-tab)
  (call-interactively #'project-switch-project)
  ;(call-interactively #'project-find-file)
  (unless bufferlo-mode
    (bufferlo-mode 1))
  (bufferlo-isolate-project)
  (let ((project-name (file-name-nondirectory
                       (directory-file-name
                        (project-root (project-current))))))
    (tab-bar-rename-tab project-name)))


(defun my/toggle-terminal-window ()
  "Toggle a terminal window between visiable and hidden"
  (interactive)
  ;(let* ((buffer "*terminal*")
  (let* ((buffer "*eshell*")
         (window (get-buffer-window buffer)))
    (if window
        (progn
          (select-window window)
          (delete-window window))
      (progn
        (select-window (split-window-below))
        ;(unless (eq major-mode 'term-mode)
        (unless (eq major-mode 'eshell-mode)
          ;(term "/bin/bash"))))))
          (eshell))))))


(defun kill-all-buffers ()
  "Kill all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(provide 'init)

;;; init ends here
