;; python.el

(use-package lsp-python
  :ensure t
  :after (lsp-mode)
  :init (add-hook 'python-mode-hook #'lsp-python-enable))
