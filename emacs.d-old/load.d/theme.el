;; theme.el

;(use-package monokai-theme :ensure t
;  :config (load-theme 'monokai' t))

;(use-package molokai-theme :ensure t
;  :config (load-theme 'molokai' t))

(add-to-list 'default-frame-alist '(font . "Source Code Pro-11"))
;(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-12"))


(use-package zerodark-theme
  :ensure t
  :config
  (load-theme 'zerodark' t)
  (zerodark-setup-modeline-format)
)

;(use-package spacemacs-dark-theme
;  :ensure t
;  :config
;  (load-theme 'spacemacs-dark t))

; (use-package doom-themes
;   :ensure t
;   :config
;   (load-theme 'doom-one t)
; ;  (load-theme 'doom-palenight t)
; ;  (load-theme 'doom-nord t)
; ;  (load-theme 'doom-opera t)
; ;  (load-theme 'doom-spacegrey t)
; )

; NOTE:  need to manually run: M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

; (use-package spaceline
;   :ensure t)
; 
; (use-package spaceline-all-the-icons
;   :ensure t
;   :after spaceline
;   :config
;   (spaceline-all-the-icons-theme))
