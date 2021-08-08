(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
 (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
;(load-theme 'doom-horizon t)
;(load-theme 'doom-dracula t)
;(load-theme 'doom-one t)
;(load-theme 'doom-vibrant t)
;(load-theme 'doom-acario-dark t)
;(load-theme 'doom-Iosvkem)
;(load-theme 'doom-material)
;(load-theme 'doom-monokai-classic)
(load-theme 'doom-monokai-pro)
;(load-theme 'doom-molokai)
;(load-theme 'doom-palenight)
;(load-theme 'doom-snazzy)
;(load-theme 'doom-solarized-dark)
;(load-theme 'doom-tomorrow-night)
;(load-theme 'doom-wilmersdorf)
(doom-themes-visual-bell-config))

;(use-package zenburn-theme
;  :ensure t
;  :config
;  (load-theme 'zenburn t))

;(use-package molokai-theme
;  :ensure t
;  :config
;  (load-theme 'molokai t))

;(use-package atom-one-dark-theme
;  :ensure t
;  :config
;  (load-theme 'atom-one-dark t))

;(use-package spacemacs-dark-theme
;  :ensure t
;  :config
;  (load-theme 'spacemacs-dark t))

;(use-package atom-dark-theme
;  :ensure t
;  :config
;  (load-theme 'atom-dark t))

;(use-package nimbus-theme
;  :ensure t
;  :config
;  (load-theme 'nimbus t))

;;; Available themes:
;; doom-one
;; doom-one-light
;; doom-vibrant
;; doom-acario-dark
;; doom-acario-light
;; doom-city-lights
;; doom-challenger-deep
;; doom-dark+
;; doom-dracula
;; doom-ephemeral
;; doom-fairy-floss
;; doom-gruvbox
;; doom-horizon
;; doom-Iosvkem
;; doom-laserwave
;; doom-material
;; doom-manegarm
;; doom-molokai
;; doom-monokai-classic
;; doom-monokai-pro
;; doom-moonlight
;; doom-nord
;; doom-nord-light
;; doom-nova
;; doom-oceanic-next
;; doom-opera
;; doom-opera-light
;; doom-outrun-electric
;; doom-palenight
;; doom-peacock
;; doom-rouge
;; doom-snazzy
;; doom-solarized-dark
;; doom-solarized-light
;; doom-sourcerer
;; doom-spacegrey
;; doom-tomorrow-day
;; doom-tomorrow-night
;; doom-wilmersdorf


;; Add tab bar to show open buffers
;(use-package centaur-tabs
;  :demand
;  :config
;  (centaur-tabs-mode t)
;  (setq centaur-tabs-style "slant")
;  (setq centaur-tabs-height 32)
;  (setq centaur-tabs-set-icons t)
;  ;(setq centaur-tabs-set-bar 'under)
;  :bind
;  ("C-<prior>" . centaur-tabs-backward)
;  ("C-<next>" . centaur-tabs-forward)
;  ;:init
;  ;(centaur-tabs-headline-match)
;)

; remove some GUI elements
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Download icons
(use-package all-the-icons
  :ensure t)

(defcustom is-all-the-icons-installed nil
  "DO NOT EDIT. Checks if all-the-icons fonts are installed yet."
  :group 'smb
  :type 'boolean)

(unless is-all-the-icons-installed
  (all-the-icons-install-fonts)
  (customize-save-variable 'is-all-the-icons-installed t))

;; doom-modeline for bottom status bar
(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode)
      :config
      ;; Don’t compact font caches during GC. Improves performance
      (setq inhibit-compacting-font-caches t)
      ;; Fix symlink bug for emacs
      (setq find-file-visit-truename t))

(use-package neotree
  :ensure t
  :config
  ;; (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(defcustom last-neotree-toggled nil
  "Checks if neotree was last toggled."
  :group 'smb
  :type 'boolean)

(global-set-key [f8] (lambda ()
		       (interactive)
		       (if last-neotree-toggled
			   (progn
			     (customize-save-variable 'last-neotree-toggled nil)
			     (neotree-hide))
			 (progn
			   (customize-save-variable 'last-neotree-toggled t)
			   (neotree-show)))))

(if last-neotree-toggled
    (neotree-show))


;; Start fullscreen
;(add-hook 'window-setup-hook #'toggle-frame-fullscreen)

;; Set window size and center it on the screen
;; emacs-x = (screen-x / 2) - (emacs-width / 2)
;; emacs-y = (screen-y / 2) - (emacs-height / 2)
(setq emacs-width-px 1400)
(setq emacs-height-px 768)
(set-frame-size (selected-frame) emacs-width-px emacs-height-px t)
(set-frame-position (selected-frame) (- (/ (display-pixel-width) 2) (/ emacs-width-px 2)) (- (/ (display-pixel-height) 2) (/ emacs-height-px 2)))

;(setq-default line-spacing 0.2)
(setq-default indent-tabs-mode nil)  ; don't use tabs by default

;; Extra config
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq save-interprogram-paste-before-kill t)
(setq apropos-do-all t)
(setq mouse-yank-at-point t)
(setq inhibit-startup-screen t)      ; don't show splash screen
(setq ring-bell-function 'ignore)    ; disable that obnoxious noise
;(setq visible-bell nil)              ; don't flash the screen either
(setq require-final-newline t)       ; add newline to end of file if necessary
(setq show-trailing-whitespace t)    ; show trailing whitespace
(setq scroll-margin 3)
(setq scroll-conservatively 9999)
(setq auto-window-vscroll nil)
(setq linum-format "%4d" column-number-mode t)  ; Line number format
;(setq global-linum-mode t)                     ; Show line numbers in the buffers
;(setq compilation-scroll-output 'first-error)  ; scroll until the first error
;(setq gc-cons-threshold 100000000)             ; make garbage collection stalls much less frequent
(setq read-process-output-max (* 512 1024))     ; increase process read limits for better lsp performance

;(blink-cursor-mode t)
;(set-cursor-color "#cccccc")

;; Set default font
;(set-face-attribute 'default nil
;                    ;:family "Ubuntu Mono"
;                    ;:family "Source Code Pro-14"
;                    :family "Liberation Mono-14"
;                    ;:height 130
;                    ;:weight 'normal
;                    ;:width 'normal
;)

;(set-frame-font "Monospace-14" nil t)
(set-frame-font "DejaVu Sans Mono-14" nil t)
;(set-frame-font "Liberation Mono-14" nil t)
;(set-frame-font "Source Code Pro-14" nil t)
;(set-frame-font "Fira Code Regular-14" nil t)

(provide 'init-ui)

