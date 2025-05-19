(use-package haskell-mode
  :ensure t)

;; Scheme REPL integration
(use-package geiser-guile
  :ensure t
  :config (setq geiser-guile-binary "guile3.0"))

(use-package racket-mode
  :ensure t
  :hook (racket-mode . (lambda ()
                         (racket-xp-mode)
                         (font-lock-mode)))
  :bind (:map racket-mode-map
              ("<f5>" . racket-run))
  :config
  ;(setq racket-completion-min-chars 1)
  ;(setq racket-xp-completion-min-chars 1)
  (setq tab-always-indent 'complete))


;(use-package clojure-mode
;  :ensure t)
;
;(use-package cider
;  :ensure)

(use-package protobuf-mode
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package cargo
  :ensure t)

;; Format on save for rust files
;(add-hook 'rust-ts-mode-hook
;          (lambda ()
;             (add-hook 'before-save-hook 'cargo-process-fmt nil 'local))) ; local save hook for rust


(provide 'init-languages)
