;; lsp-mode main package
(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-eldoc-render-all t)
  :config
  (setq lsp-enable-snippet nil
        lsp-auto-configure t
        lsp-auto-guess-root t
        lsp-headerline-breadcrumb-enable nil)
  :hook
  (rustic-mode . lsp-mode)
  (c++-mode . lsp-mode)
  (python-mode . lsp-mode))


;; lsp-ui for flycheck integration
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode))

;; company-lsp for company integration
;(use-package company-lsp
;  :ensure t
;  :commands company-lsp
;  :config
;  (push 'company-lsp company-backends)
;  (setq company-lsp-cache-candidates 'auto
;	    company-lsp-async t
;	    ;company-lsp-enable-snippet t
;	    company-lsp-enable-recompletion t))

(provide 'init-lsp)
;;; init-lsp ends here
