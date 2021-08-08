;; rust.el

; (use-package racer
;   :ensure t)

(use-package cargo
  :ensure t)

(use-package flycheck-rust
  :ensure t
  :after (flycheck)
  :init
  (setq flycheck-error-list-minimum-level 'warning))

; (use-package rust-mode
;   :ensure t
;   :after (company)
;   :mode "\\.rs\\'"
;   :init
;   (setq rust-format-on-save t)
;   (setq company-tooltip-align-annotations t)
;   :hook
;   ;(rust-mode . lsp-rust-enable)
;   (rust-mode . racer-mode)
;   (rust-mode . cargo-minor-mode)
;   (racer-mode . eldoc-mode)
;   (racer-mode . company-mode)
;   ;(rust-mode . flycheck-mode)
;   ;(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;   ;(add-hook 'rust-mode-hook #'racer-mode)
;   ;(add-hook 'racer-mode-hook #'eldoc-mode)
;   ;(add-hook 'rust-mode-hook #'cargo-minor-mode)
;   ;(add-hook 'racer-mode-hook #'company-mode)      ; code completion
;   ;:general
;   ;(:keymaps 'rust-mode-map "TAB" 'company-indent-or-complete-common)
;   )

(use-package lsp-rust
  :after lsp-mode)
