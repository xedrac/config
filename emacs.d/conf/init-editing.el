(use-package rainbow-delimiters
  :ensure t
  ;; Add hooks for programming mode
  :hook
  ;(prog-mode . rainbow-delimiters-mode))
  (lisp-mode . rainbow-delimiters-mode))

;(use-package smartparens
;  :ensure t
;  :config
;  (require 'smartparens-config)
;  (smartparens-global-mode t)
;  (setq sp-highlight-pair-overlay nil)
;  (setq sp-show-pair-from-inside nil)
;  :diminish smartparens-mode
;  :hook
;  (lisp-mode . smartparens-mode))

;(use-package move-text
;  :ensure t
;  :bind (("M-<up>" .  move-text-up)
;	 ("M-<down>" .  move-text-down)))

;; Remember line when you revisit a file
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))
(setq delete-active-region t)

(define-key global-map (kbd "RET") 'newline-and-indent)

;; Save last session
 (desktop-save-mode 1)

;; No text wrapping
(set-default 'truncate-lines t)

;; Disable autosaves
; (setq auto-save-default nil)

;; Evil configuration
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil
        evil-want-integration nil)
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

(provide 'init-editing)
