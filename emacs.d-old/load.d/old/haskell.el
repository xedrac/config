; haskell.el

(use-package haskell-mode
  :init
  (setq haskell-process-type 'stack-ghci)
  :hook interactive-haskell-mode)

(use-package hasky-stack)
