(require 'eglot)

(add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) . ("rust-analyzer")))
(add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pyright" "--stdio")))
(add-to-list 'eglot-server-programs '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd" "--background-index")))
;(add-to-list 'eglot-server-programs '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd" "--compile-commands-dir=\"build\")))
(add-to-list 'eglot-server-programs '((haskell-mode haskell-ts-mode) . ("haskell-language-server-wrapper" "--lsp")))
;(add-to-list 'eglot-server-programs '((js-mode typescript-mode ts-mode) . ("typescript-language-server" "--stdio")))
;(add-to-list 'eglot-server-programs '((java-mode java-ts-mode) . ("jdtls")))
;(add-to-list 'eglot-server-programs '((go-mode) . ("gopls")))
;(add-to-list 'eglot-server-programs '((web-mode html-mode css-mode) . ("vscode-html-language-server" "--stdio")))

;; Enable lsp for all programming languages except the lispy ones
(add-hook 'prog-mode-hook (lambda ()
                            (unless (member major-mode '(emacs-lisp-mode racket-mode lisp-mode scheme-mode common-lisp-mode))
                              (eglot-ensure))))


;;; Completion ui in minibuffer
(use-package vertico
  :ensure t
  :bind (:map minibuffer-local-map
          ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  ;(vertico-resize t)
  (setq vertico-count 30)
  (setq vertico-scroll-margin 0)
  :init
  (vertico-mode))

;;; Add context to results in the margins (file permissions, dates, help info, etc...)
(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;;; Inline UI popup for completions (instead of just in the minibuffer)
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  :custom
  (setq eglot-stay-out-of '(completion-at-point))
  (corfu-auto t)                ;; Enable auto completion
  (corfu-auto-delay 0.2)        ;; seconds before completion popup is shown
  (corfu-auto-prefix 1)         ;; num chars required to show completions
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  :hook ((prog-mode shell-mode eshell-mode) . corfu-mode))

;;; Much nicer icons for corfu completions
(use-package kind-icon
  :ensure t
  :after corfu
  ;:custom
  ; (kind-icon-blend-background t)
  ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode))

(provide 'init-completions)
