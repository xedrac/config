
;(use-package neotree
;  :ensure t
;  :config
;  (setq neo-window-width 40)
;  (setq neo-autorefresh t)
;  (setq neo-window-fixed-size -1)
;  ;(setq neo-auto-indent-point t)
;  ;(setq neo-vc-integration t)
;  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
;  ;(setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
;
;; Make the nerd icons smaller in the neotree buffer
;(add-hook 'neo-after-create-hook
;          (lambda (&rest _)
;            (with-current-buffer (neo-global--get-buffer)
;              (setq-local nerd-icons-scale-factor 1.0))))


(use-package treemacs
  :ensure t
  ;:defer t
  :config
  (setq treemacs-filewatch-mode t
        treemacs-follow-mode t
        ;treemacs-add-and-display-current-directory t
        ;treemacs-display-current-directory-exclusively t
        treemacs-display-current-project-exclusively t
        treemacs-project-follow-mode t
        treemacs-file-follow-delay 0.0
        treemacs-follow-after-init t
        treemacs-expand-after-init t
        treemacs-litter-directories '("/.venv" "/build" "/.cache" "/eln-cache")
        treemacs-show-cursor nil
        treemacs-wide-toggle-width 70
        treemacs-width 40
        treemacs-move-files-by-mouse-dragging t
        treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-missing-project-action 'ask
        treemacs-find-workspace-method 'find-for-file-or-pick-first
        treemacs-collapse-dirs 3
        treemacs-project-follow-cleanup t
        treemacs-git-mode 'simple)
  (treemacs-resize-icons 18)
  :hook (treemacs-mode . (lambda () (set-face-background 'treemacs-window-background-face "#232326"))))

(use-package treemacs-evil
  :after treemacs
  :ensure t)

(provide 'init-filetree)
