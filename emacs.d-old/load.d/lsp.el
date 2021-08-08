;; lsp.el

; This contains everything related to lsp and dap

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((prog-mode . lsp))  ; Automatically start lsp when entering a programming mode
  :config
  (setq lsp-rust-server 'rust-analyzer
        lsp-idle-delay 0.500   ; controls how lsp refreshes as you type
	lsp-prefer-capf t
	lsp-keep-workspace-alive t
	;lsp-log-io nil
	;lsp-auto-guess-root t
	lsp-enable-completion-at-point t
	;lsp-enable-xref t
	;lsp-prefer-flymake nil
	;lsp-use-native-json t
	;lsp-enable-indentation t
	;lsp-response-timeout 10
	lsp-restart 'auto-restart
	lsp-eldoc-render-all nil
	lsp-enable-snippet nil
	lsp-enable-folding t))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil 
	lsp-ui-peek-always-show nil
	lsp-ui-doc-use-childframe t
	imenu-auto-rescan t)
  :hook
  ((lsp-mode . lsp-ui-mode)
	 (lsp-after-open . (lambda ()
			     (lsp-ui-flycheck-enable t)
			     (lsp-ui-imenu-enable t)
			     (lsp-lens-mode t)
			     (lsp-ui-peek-enable t)
			     (lsp-ui-dock-enable t))))
)

; Modular completion framework
(use-package company
  :ensure t
  :init
  (setq company-global-modes '(not gud-mode))  ; disable when running gdb
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (push 'company-capf company-backends)
  (setq company-tooltip-align-annotations t
	company-idle-delay 0.0
	company-minimum-prefix-length 1)
  ;:general
  ;(company-active-map
  ;  "C-n" 'company-select-next
  ;  "C-p" 'company-select-previous
  ;  "TAB" 'company-complete-common-or-cycle
  ;  "RET" 'company-complete-selection
  ;  "<return>" 'company-complete-selection
  ;)
  )

; ; Company backend for lsp-mode (TODO: company-lsp not supported anymore?  switch to company-capf?)
; (use-package company-lsp
;   :ensure t
;   :config
;   (setq company-transformers nil
;         company-lsp-cache-candidates nil  ; disable company caching, as server is faster (recommended in lsp docs)
;         company-lsp-async t               ; async completion
;         company-lsp-enable-recompletion t ; should help with c++ std:: type completions
; 	company-lsp-cache-candidates t
; 	company-lsp-enable-snippet t)
;   (push 'company-lsp company-backends))

; Company frontend with custom icons and displays documentation for candidates (gui emacs only)
; Note:  Has icons for company-lsp but not company-capf?
;(use-package company-box
;  :ensure t
;  :hook (company-mode . company-box-mode))

;(use-package company-qml
;  :ensure t
;  :config
;  (push 'company-qml company-backends))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

;(use-package lsp-treemacs
;  :ensure t
;  :commands lsp-treemacs-errors-list)
