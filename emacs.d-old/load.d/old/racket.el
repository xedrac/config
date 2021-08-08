;; racket.el

(use-package racket-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.rkt\\'". racket-mode)))
