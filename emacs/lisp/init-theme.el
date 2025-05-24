
;(use-package atom-one-dark-theme
;  ;:ensure (:host github :repo "jonathanchu/atom-one-dark-theme")
;  :ensure (:host github :repo "xedrac/atom-one-dark-theme")
;  ;:init
;  ;(set-face-background 'line-number "#232326")
;  ;(set-face-background 'line-number-current-line "#232326")
;  ;(set-face-background 'default "#232326")
;  :config
;  (load-theme 'atom-one-dark t)
;
;(use-package dimmer
;  :ensure t
;  :config (dimmer-mode t))
;
;(use-package telephone-line
;  :ensure (:host github :repo "dbordak/telephone-line")
;  :config
;    (telephone-line-mode 1))
;
;;;; Show each matching paren pair as a different color to make them easier to parse visually
;(use-package rainbow-delimiters
;  :ensure t
;  :hook ((racket-mode lisp-mode) . rainbow-delimiters-mode))
;
;;;; Buffers that visit files look slightly different than other buffer types
;;(use-package solaire-mode
;;  :ensure t
;;  :config (solaire-global-mode +1))
;
;(use-package nerd-icons
;  :ensure t)
;  ;:init
;  ;(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)



;(use-package vscode-dark-plus-theme
;  :ensure t
;  :config
;  (load-theme 'vscode-dark-plus t))


;(use-package doom-themes
;  :ensure t
;  :config
;  ;; Global settings (defaults)
;  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
;        doom-themes-enable-italic t)
;  ; if nil, italics is universally disabled
;  ;(load-theme 'doom-horizon t)
;  ;(load-theme 'doom-dracula t)
;  (load-theme 'doom-one t)
;  ;(load-theme 'doom-vibrant t)
;  ;(load-theme 'doom-acario-dark t)
;  ;(load-theme 'doom-Iosvkem)
;  ;(load-theme 'doom-material)
;  ;(load-theme 'doom-monokai-classic)
;  ;(load-theme 'doom-monokai-pro)
;  ;(load-theme 'doom-molokai)
;  ;(load-theme 'doom-palenight)
;  ;(load-theme 'doom-snazzy)
;  ;(load-theme 'doom-solarized-dark)
;  ;(load-theme 'doom-tomorrow-night)
;  ;(load-theme 'doom-wilmersdorf)
;  ;(doom-themes-visual-bell-config)
;)

;;; doom-modeline for bottom status bar
;(use-package doom-modeline
;      :ensure t
;      :hook (after-init . doom-modeline-mode)
;      :config
;      ;; Don’t compact font caches during GC. Improves performance
;      (setq inhibit-compacting-font-caches t)
;      ;; Fix symlink bug for emacs
;      (setq find-file-visit-truename t))

;(use-package doom-themes
;  :ensure t)
;(with-eval-after-load 'doom-themes
;  (setq doom-themes-treemacs-theme "doom-colors")
;  (doom-themes-treemacs-config))
;(use-package all-the-icons
;  :ensure t)

(provide 'init-theme)
