;; -*- lexical-binding: t; -*-

(add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) . ("rust-analyzer")))
(add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pyright" "--stdio")))
(add-to-list 'eglot-server-programs '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd" "--background-index")))
;(add-to-list 'eglot-server-programs '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd" "--compile-commands-dir=\"build\")))
(add-to-list 'eglot-server-programs '((haskell-mode haskell-ts-mode) . ("haskell-language-server-wrapper")))
;(add-to-list 'eglot-server-programs '((js-mode typescript-mode ts-mode) . ("typescript-language-server" "--stdio")))
;(add-to-list 'eglot-server-programs '((java-mode java-ts-mode) . ("jdtls")))
;(add-to-list 'eglot-server-programs '((go-mode) . ("gopls")))
;(add-to-list 'eglot-server-programs '((web-mode html-mode css-mode) . ("vscode-html-language-server" "--stdio")))
(add-hook 'prog-mode-hook 'eglot-ensure)


;(use-package eglot
;  :ensure t
;  :init
;  (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) . ("rust-analyzer")))
;  (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pyright" "--stdio")))
;  (add-to-list 'eglot-server-programs '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd" "--background-index")))
;  ;(add-to-list 'eglot-server-programs '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd" "--compile-commands-dir=\"build\")))
;  (add-to-list 'eglot-server-programs '((haskell-mode haskell-ts-mode) . ("haskell-language-server-wrapper")))

;;; Start eglot/lsp automatically when entering these programming modes
;(add-hook 'c-mode-hook 'eglot-ensure)
;(add-hook 'c-ts-mode-hook 'eglot-ensure)
;(add-hook 'c++-mode-hook 'eglot-ensure)
;(add-hook 'c++-ts-mode-hook 'eglot-ensure)
;(add-hook 'c-mode-hook 'eglot-ensure)    ; Requires clangd to be installed already
;(add-hook 'c++-ts-mode-hook 'eglot-ensure)  ; Same as ^^^
;(add-hook 'haskell-mode-hook 'eglot-ensure)
;(add-hook 'haskell-ts-mode-hook 'eglot-ensure)
;(add-hook 'rust-mode 'eglot-ensure)
;(add-hook 'rust-ts-mode 'eglot-ensure)

;;; Convenient keybinding support for evil
(use-package general
  :ensure t)
(elpaca-wait)

(use-package vterm
  :ensure t)

;;; Different undo/redo behavior
;(use-package undo-tree
;  :ensure t
;  :init (global-undo-tree-mode))

;;; Visualize the undo tree (vundo)
(use-package vundo
  :ensure t)

;;; Completion ui
(use-package vertico
  :ensure t
  :bind (:map minibuffer-local-map
          ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  ;(vertico-resize t)
  (setq vertico-count 30)
  (setq vertico-scroll-margin 0)
  :init
  (vertico-mode))

;;; Show recent commands/files at the top
(require 'savehist)
(savehist-mode)

;;; Add context to results in the margins (file permissions, dates, help info, etc...)
(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;;; Project level commands (switch project roots, find files, grep, etc...)
;(use-package project
;  :ensure t)

;;; Provides ripgrep and file finding
(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.01
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        read-file-name-completion-ignore-case t  ; ignore case when searching filenames
        read-buffer-completion-ignore-case t     ; ignore case when grepping buffers
        completion-ignore-case t)                ; ignore case on completions
  :config
  (consult-customize
    consult-theme :preview-key '(:debounce 0.2 any)
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
  :bind (("C-c" . embark-act)         ;; pick some comfortable binding
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


;;; Automatically enable <lang>-ts-mode for appropriate modes
(use-package treesit-auto
  :ensure (:host github :repo "renzmann/treesit-auto")
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; Inline UI popup for completions (instead of just in the minibuffer)
;(use-package corfu
;  :ensure t
;  :init
;  (global-corfu-mode)
;  :custom
;  (setq eglot-stay-out-of '(completion-at-point))
;  (corfu-auto t)                 ;; Enable auto completion
;  (corfu-auto-delay 0.1)        ;; 0 is not recommended
;  (corfu-auto-prefix 1)         ;; 1 is not recommended
;  ;(corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;  ;(corfu-separator ?\s)         ;; Orderless field separator
;  ;;(corfu-quit-at-boundary nil) ;; Never quit at completion boundary
;  ;(corfu-quit-no-match 'separator)      ;; Never quit, even if there is no match
;  ;;(corfu-preview-current nil)    ;; Disable current candidate preview
;  ;;(corfu-preselect 'prompt)      ;; Preselect the prompt
;  ;;(corfu-on-exact-match nil)     ;; Configure handling of exact matches
;  ;;(corfu-scroll-margin 5)        ;; Use scroll margin
;  ;; Enable Corfu only for certain modes.
;  :hook ((prog-mode shell-mode eshell-mode) . corfu-mode))

(use-package company
  :ensure t
  :init
  (global-company-mode)
  :custom
  (company-idle-delay 0.0) ;; Delay before showing completions (adjust as needed)
  (company-minimum-prefix-length 1) ;; Trigger completions after 1 character
  (company-tooltip-align-annotations t) ;; Align annotations (e.g., function signatures)
  :config
  (add-to-list 'company-backends 'company-capf))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(require 'which-key)
(which-key-mode)

(use-package nerd-icons
  :ensure t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  ;(setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))


;; Indent code somewhat sanely when pasting
(use-package snap-indent
  :ensure t
  :hook (prog-mode . snap-indent-mode)
  :custom ((snap-indent-format '(untabify delete-trailing-whitespace))
           (snap-indent-on-save nil)))

;; Buffers that visit files look slightly different than other buffer types
(use-package solaire-mode
  :ensure t
  :config (solaire-global-mode +1))

;; Scheme REPL integration
(use-package geiser-guile
  :ensure t
  :config (setq geiser-guile-binary "guile3.0"))

(use-package racket-mode
  :ensure t
  :init
  (setq racket-completion-min-chars 1)
  :hook (racket-mode . (lambda () (define-key racket-mod-map (kbd "<F5>") 'racket-run)))
        (racket-mode . company-mode))


(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure)

(use-package protobuf-mode
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package cargo
  :ensure t)

(use-package atom-one-dark-theme
  :ensure (:host github :repo "jonathanchu/atom-one-dark-theme")
  :init
  (set-face-background 'line-number "#212124")
  (set-face-background 'line-number-current-line "#232326")
  (set-face-background 'default "#232326")
  :config
  (load-theme 'atom-one-dark t))

;(use-package doom-themes
;  :ensure t)
;(with-eval-after-load 'doom-themes
;  (setq doom-themes-treemacs-theme "doom-colors")
;  (doom-themes-treemacs-config))
;(use-package all-the-icons
;  :ensure t)

;;; doom-modeline for bottom status bar
;(use-package doom-modeline
;      :ensure t
;      :hook (after-init . doom-modeline-mode)
;      :config
;      ;; Don’t compact font caches during GC. Improves performance
;      (setq inhibit-compacting-font-caches t)
;      ;; Fix symlink bug for emacs
;      (setq find-file-visit-truename t))

(use-package telephone-line
  :ensure (:host github :repo "dbordak/telephone-line")
  :config
    (telephone-line-mode 1))

(use-package neotree
  :ensure t
  ;:hook (neo-after-create . (lambda (&optional dummy)
  ;                            (set-face-background 'default "#232326" (current-buffer))))
  :config
  (setq neo-window-width 40)
  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow)))

(use-package all-the-icons
  :ensure t)


;(use-package treemacs
;  :ensure t
;  ;:defer t
;  :config
;  (setq treemacs-filewatch-mode t
;        treemacs-follow-mode t
;        ;treemacs-add-and-display-current-directory t
;        ;treemacs-display-current-directory-exclusively t
;        treemacs-display-current-project-exclusively t
;        treemacs-project-follow-mode t
;        treemacs-file-follow-delay 0.0
;        treemacs-follow-after-init t
;        treemacs-expand-after-init t
;        treemacs-litter-directories '("/.venv" "/build" "/.cache" "/eln-cache")
;        treemacs-show-cursor nil
;        treemacs-wide-toggle-width 70
;        treemacs-width 40
;        treemacs-move-files-by-mouse-dragging t
;        treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;        treemacs-missing-project-action 'ask
;        treemacs-find-workspace-method 'find-for-file-or-pick-first
;        treemacs-collapse-dirs 3
;        treemacs-project-follow-cleanup t
;        treemacs-git-mode 'simple)
;  (treemacs-resize-icons 18))
;
;(use-package treemacs-evil
;  :after treemacs
;  :ensure t)

(use-package transient
  :ensure (:host github :repo "magit/transient"))

(use-package magit
  :ensure (:host github :repo "magit/magit"))

;;; Remove trailing whitespace on lines you've edited
;(use-package ws-butler
;  :ensure t
;  :config
;  (ws-butler-global-mode t))

;(use-package ethan-wspace
;  :ensure t
;  :config
;  (global-ethan-wspace-mode 1))

;; Format on save for rust files
;(add-hook 'rust-ts-mode-hook
;          (lambda ()
;             (add-hook 'before-save-hook 'cargo-process-fmt nil 'local))) ; local save hook for rust

;(use-package dimmer
;  :ensure t
;  :config (dimmer-mode t))

;(use-package doom-themes
;  :ensure t
;  :config
;  ;; Global settings (defaults)
;  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
;        doom-themes-enable-italic t)
;  ; if nil, italics is universally disabled
;  ;(load-theme 'doom-horizon t)
;  ;(load-theme 'doom-dracula t)
;  (load-theme 'doom-one t)
;  ;(load-theme 'doom-vibrant t)
;  ;(load-theme 'doom-acario-dark t)
;  ;(load-theme 'doom-Iosvkem)
;  ;(load-theme 'doom-material)
;  ;(load-theme 'doom-monokai-classic)
;  ;(load-theme 'doom-monokai-pro)
;  ;(load-theme 'doom-molokai)
;  ;(load-theme 'doom-palenight)
;  ;(load-theme 'doom-snazzy)
;  ;(load-theme 'doom-solarized-dark)
;  ;(load-theme 'doom-tomorrow-night)
;  ;(load-theme 'doom-wilmersdorf)
;  ;(doom-themes-visual-bell-config)
;)

;;; Benchmarking tool for testing purposes
;;(use-package esup
;;    :config
;;    ;; Workaround an issue with esup and byte-compiled cl-lib
;;    ;; but this also seems to make it less useful
;;    (setq esup-depth 0)


(provide 'init-packages)
