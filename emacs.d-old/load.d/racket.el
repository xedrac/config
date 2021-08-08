;; racket.el

(use-package racket-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.rkt\\'". racket-mode)))
