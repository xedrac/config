;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(consult-fd-args "fd --color=never --type f --hidden --exclude \\#*\\#")
 '(consult-preview-allowed-hooks '(save-place-find-file-hook global-font-lock-mode))
 '(initial-frame-alist '((fullscreen . maximized)))
 '(org-hide-emphasis-markers t)
 '(save-place-mode t nil nil "Customized with use-package emacs")
 '(tab-bar-auto-width-max '((150) 20))
 '(tab-bar-close-button-show nil)
 '(tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
 '(tab-bar-format
   '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
 '(tab-bar-new-tab-choice 'bookmark-bmenu-get-buffer))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-bar ((t (:background "#2C323C" :height 1.3))))
 '(telephone-line-evil-insert ((t (:inherit telephone-line-evil :background "#61afef" :foreground "#000000"))))
 '(telephone-line-evil-normal ((t (:inherit telephone-line-evil :background "#98c379" :foreground "#000000"))))
 '(telephone-line-evil-replace ((t (:inherit telephone-line-evil :background "#e06c75" :foreground "#000000"))))
 '(telephone-line-evil-visual ((t (:inherit telephone-line-evil :background "#c678dd" :foreground "#000000"))))
 '(treemacs-root-face ((t (:inherit font-lock-constant-face :weight bold :height 1.0)))))
 ;'(treemacs-window-background-face ((t (:inherit default :background "#232326")))))

 ;'(neo-dir-link-face ((t (:background "#232323" :foreground "SteelBlue1" :family "Liberation Mono"))))
 ;'(neo-file-link-face ((t (:background "#232323" :foreground "light gray" :width narrow :family "Liberation Mono"))))
 ;'(neo-root-dir-face ((t (:background "#232323" :foreground "sandy brown" :weight bold :family "Liberation Mono"))))

 ;'(neo-expand-btn-face     ((t (:background "#232326" :foreground "#98be65"))))
 ;'(neo-banner-face         ((t (:background "#232326" :foreground "#c678dd"))))
 ;'(neo-header-face         ((t (:background "#232326" :foreground "#dcdccc"))))
 ;'(neo-button-face         ((t (:background "#232326" :foreground "#98be65")))))
 ;'(cursor                  ((t (:background "#ffffff")))))
