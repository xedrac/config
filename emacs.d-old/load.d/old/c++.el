;; c++.el

; (c-add-style "my-style"
; 	     '("badger"
; 	       (indent-tabs-mode . nil)               ; use spaces rather than tabs
; 	       (c-basic-offset . 4)                   ; indent by four spaces
; 	       (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
; 				   (brace-list-open . 0)
; 				   (statement-case-open . +)))))
; 
; (defun my-c++-mode-hook ()
;   (c-set-style "my-style")  ; use my-style defined above
;   (auto-fill-mode)
;   (c-toggle-auto-hungry-state 1))
; 
; ;(add-hook 'c++-mode-hook 'my-c++-mode-hook)
; ;(add-hook 'c-mode-common-hook 'flycheck-mode)
; 
; ; use c++-mode for .ipp files (boost uses lots of them)
; (add-to-list 'auto-mode-alist '("\\.ipp\\'". c++-mode))
; ; just treat .h files as c++ mode... fixes some indenting issues
; (add-to-list 'auto-mode-alist '("\\.h\\'". c++-mode))
; 
; ; ide-like debugging layout
; (setq gdb-many-windows t)
; 
; ; speed up loading
; (setq gdb-create-source-file-list nil)
; 
; ;; Don't force popup gdb's io window (requires emacs >= 25) (worked! the above didn't...)
; (setq gdb-display-io-nopopup t)
; (setq pop-up-windows nil)
; 
; ;; cquery settings
; (setq cquery-extra-args `(,(format "--log-file=%s/.cq.log" (getenv "HOME"))))
; (setq cquery-cache-dir (format "%s/.cquery_cached_index" (getenv "HOME")))
; 
; ;; specify loading subprojects?
; ;; TODO: can this be used to find build*/compile_commands.json?
; (with-eval-after-load 'projectile
;   (setq projectile-project-root-files-top-down-recurring
; 	(append '("compile_commands.json"
; 		  ".cquery")
; 		projectile-project-root-files-top-down-recurring)))
; 
; ;; Enable lsp for all c/c++ modes
; (defun cquery//enable ()
;   (condition-case nil
;       (lsp-cquery-enable)
;     (user-error nil)))
; 
; (use-package cquery
;   :commands lsp-cquery-enable
;   :init
;   (setq cquery-executable "/usr/local/bin/cquery")
;   :hook
;   (c-mode-common . cquery//enable)
;   (c++-mode . my-c++-mode-hook))
