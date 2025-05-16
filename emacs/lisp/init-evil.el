;; -*- lexical-binding: t; -*-

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
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-motion-state-cursor '("orange" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("magenta" box))
  (setq evil-insert-state-cursor '("blue" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))

(general-define-key
  :states 'motion
  ";" 'evil-ex
  ;; ":" 'evil-repeat-find-char
  ))

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

(use-package evil-vimish-fold
  :ensure
  :after vimish-fold
  :hook ((prog-mode text-mode conf-mode) . evil-vimish-fold-mode))

;; Like evil-surround, but can be configured to support custom stuff
;(use-package evil-embrace)

;(use-package evil-args)

;(use-package evil-indent-plus
;  :ensure)

;(use-package evil-nerd-commenter
;  :ensure)

;(use-package buffer-move
;  :ensure)

(provide 'init-evil)
