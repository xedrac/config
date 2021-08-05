;; evil.el

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration nil)
  ;(setq evil-want-C-u-scroll t)
  ;(setq evil-shift-width 4)
  ;(setq evil-search-module 'evil-search)

  :config
  (evil-mode t)
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-motion-state-cursor '("orange" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("magenta" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))

  (general-define-key
    :states 'motion
    ";" 'evil-ex
    ":" 'evil-repeat-find-char)
)


(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-escape
  :ensure t)
