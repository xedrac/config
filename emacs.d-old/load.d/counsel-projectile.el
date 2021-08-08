;; counsel-projectile.el

(use-package counsel-projectile
  :ensure t
  :config
  (setq counsel-projectile-ag-initial-input '(ivy-thing-at-point))
  (counsel-projectile-mode))
