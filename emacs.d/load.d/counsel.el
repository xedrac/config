;; counsel.el

(use-package counsel
  :ensure t
  :diminish (counsel-mode . "")
  :requires (ivy)
  :config
  (counsel-mode t))
