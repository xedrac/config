;; which-key.el

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode)
  (which-key-setup-minibuffer)
  ;(which-key-setup-side-window-right)
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-max-description-length 27)
  (setq which-key-add-column-padding 0))
  ;(setq which-key-max-display-columns nil))
