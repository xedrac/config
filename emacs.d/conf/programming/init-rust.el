;;; init-rust.el
;;
;;  Note:  You'll need to install rust-src and rust-analyzer manually

;;  $ rustup component add rust-src

;;  $ git clone https://github.com/rust-analyzer/rust-analyzer.git
;;  $ cd rust-analyzer
;;  $ cargo xtask install --server # will install rust-analyzer into $HOME/.cargo/bin


(use-package cargo
  :ensure t)

(use-package flycheck-rust
  :ensure t
  :after (flycheck)
  :init
  (setq flycheck-error-list-minimum-level 'warning))

(use-package rustic
  :after lsp-mode
  ;:bind (:map rustic-mode-map
  ;      ("M-j" . lsp-ui-imenu)
  ;      ("M-?" . lsp-find-references)
  ;      ("C-c C-c l" . flycheck-list-errors)
  ;      ("C-c C-c a" . lsp-execute-code-action)
  ;      ("C-c C-c r" . lsp-rename)
  ;      ("C-c C-c q" . lsp-workspace-restart)
  ;      ("C-c C-c Q" . lsp-workspace-shutdown)
  ;      ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-to-list 'lsp-language-id-configuration '(rust-mode-hook . "rust"))
  )


(provide 'init-rust)
