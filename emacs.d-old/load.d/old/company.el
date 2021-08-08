;; company.el

(use-package company
  :ensure t
  :init
  ;(setq company-global-modes '(not gud-mode))  ; disable when running gdb
  (setq company-idle-delay .1)                 ; make completion popup faster
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  :general
  (company-active-map
    "C-n" 'company-select-next
    "C-p" 'company-select-previous
    "RET" 'company-complete-selection
    "<return>" 'company-complete-selection))

(use-package company-lsp
  :ensure t
  :init
  (setq company-transformers nil)
  (setq company-lsp-cache-candidates nil)  ; disable company caching, as server is faster (recommended in lsp docs)
  (setq company-lsp-async t)               ; async completion
  (setq company-lsp-enable-recompletion t) ; should help with c++ std:: type completions
  :config
  (push 'company-lsp company-backends))

; Company frontend with custom icons and displays documentation for candidates (gui emacs only)
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;(use-package company-qml
;  :ensure t
;  :config
;  (push 'company-qml company-backends))

(use-package 
