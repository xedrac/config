;; improved list-packages with github integration
(use-package paradox
  :ensure t
  :init
  (setf paradox-github-token "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")
  (paradox-enable))

;; better custom keybinding support with evil keymaps
(use-package general
  :ensure t)

;; command discovery and descriptions
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

;; vim emulation
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;(setq evil-want-C-u-scroll t)
  ;(setq evil-shift-width 4)
  ;(setq evil-search-module 'evil-search)

  :config
  (evil-mode 1)
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-motion-state-cursor '("orange" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("magenta" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow)))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-escape
  :ensure t)


(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 30)
  (setq ivy-count-format "(%d/%d")
  (setq ivy-display-style 'fancy)
  (setq ivy-format-function 'ivy-format-function-line)
  ;(setq ivy-re-builders-alist
  ;  '((counsel-M-x . ivy--regex-fuzzy)
  ;    (t . ivy--regex-plus)))

  (setq ivy-initial-inputs-alist nil)
  (setq enable-recursive-minibuffers t))

(use-package lsp-ivy
  :ensure t)


(use-package projectile
  :ensure t
  ;:bind (("C-x p" . projectile-switch-project)
  ;       ("s-p" . projectile-command-map)
  ;       ("C-c p" . projectile-command-map))
  :config
  ;(projectile-mode))
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package counsel
  :ensure t
  :diminish (counsel-mode . "")
  :requires (ivy)
  :config
  (counsel-mode t))

(use-package counsel-projectile
  :ensure t
  :config
  (setq counsel-projectile-ag-initial-input '(ivy-thing-at-point))
  (counsel-projectile-mode))

(use-package swiper
  :ensure t)

(use-package smex
  :ensure t)

(use-package buffer-move
  :ensure t)

(use-package lispy
  :ensure t
  :config
  (setq lispy-safe-delete t
        lispy-safe-copy t
        lispy-safe-paste t
        lispy-safe-actions-no-pull-delimiters-into-comments t))

(use-package lispyville
  :ensure t
  ;:init
  ;(general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode)
  :hook
  (emacs-list-mode . lispyville-mode)
  (list-mode . lispyville-mode))


(provide 'init-navigation)
