
;;;; Provides ripgrep and file finding
;(use-package consult
;  :ensure t
;  :demand t
;  :hook
;  ((completion-list-mode . consult-preview-at-point-mode)
;   (consult-after-jump . font-lock-mode))
;  :init
;  (setq register-preview-delay 0.01
;        register-preview-function #'consult-register-format)
;  (advice-add #'register-preview :override #'consult-register-window)
;  (setq xref-show-xrefs-function #'consult-xref
;        xref-show-definitions-function #'consult-xref
;        read-file-name-completion-ignore-case t  ; ignore case when searching filenames
;        read-buffer-completion-ignore-case t     ; ignore case when grepping buffers
;        completion-ignore-case t)                ; ignore case on completions
;  (setq consult-fd-args "fd --color=never --type f --hidden --exclude \\#*\\#")  ; Don't show temp files in search find results
;
;  :config
;  ;(setq consult-buffer-sources '(consult--source-file))  ; only show file-backed buffers in the list
;  ;(setq consult-ripgrep-args "rg --null --line-buffered --color=always --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip")
;  ;(defun consult-list-all-project-files ()
;  ;  "Show all project files immediately"
;  ;  (interactive)
;  ;  (consult--read (project-files (project-current t))
;  ;                 :prompt "Project file: "
;  ;                 :category 'file))
;
;  ; Emulate telescope.vim by showing file live previews
;  (defun consult-list-all-project-files ()
;    "Show all project files immediately with live preview"
;    (interactive)
;    (let* ((prj (project-current t))
;           (files (project-files prj))
;           (root (expand-file-name (project-root prj)))
;           (relativefiles (mapcar (lambda (file)
;                                    (string-remove-prefix root file))
;                                  files)))
;      ;(consult--read (project-files (project-current t))
;      (consult--read relativefiles
;                     :prompt "Project file: "
;                     :category 'file
;                     :state (consult--file-state)
;                     :require-match t)))
;  (consult-customize
;    consult-theme :preview-key '(:debunce 0.2 any)
;    consult-ripgrep consult-git-grep consult-grep
;    consult-bookmark consult-recent-file consult-xref
;    consult--source-bookmark consult--source-file-register
;    consult--source-recent-file consult--source-project-recent-file
;    :preview-key '(:debounce 0.0 any)))
;
;
;;;; Allow specifying substrings instead of having to tab complete from the beginning in Vertico
;(use-package orderless
;  :ensure t
;  :init
;  (setq completion-styles '(orderless basic)
;        completion-category-defaults nil
;        completion-category-overrides '((file (styles partial-completion)))))
;
;;;; Contextual actions for vertico (delete, rename, etc...)
;(use-package embark
;  :ensure t
;  :bind (("C-c" . embark-act)         ;; pick some comfortable binding
;         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
;
;  :init
;  ;; Optionally replace the key help with a completing-read interface
;  (setq prefix-help-command #'embark-prefix-help-command)
;
;  :config
;  ;; Hide the mode line of the Embark live/completions buffers
;  (add-to-list 'display-buffer-alist
;               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;                 nil
;                 (window-parameters (mode-line-format . none)))))
;
;
;;;; Contextual actions for consult stuff too
;(use-package embark-consult
;  :ensure t   ; only need to install it, embark loads it after consult if found
;  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-searching)
