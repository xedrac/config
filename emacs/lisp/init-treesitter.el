(require 'treesit)

;;; Automatically enable <lang>-ts-mode for appropriate modes
(use-package treesit-auto
  :ensure (:host github :repo "renzmann/treesit-auto")
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'init-treesitter)
