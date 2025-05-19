;; -*- lexical-binding: t; -*-

;;; Convenient keybinding support for evil
(use-package general
  :ensure t)
(elpaca-wait)

;;; vim emulation
(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-keybinding nil)
  ;(setq evil-want-C-u-scroll t)
  ;(setq evil-shift-width 4)
  ;(setq evil-search-module 'evil-search)
  :config
  (evil-mode 1)
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-motion-state-cursor '("orange" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("magenta" box))
  (setq evil-insert-state-cursor '("blue" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))

  ;(evil-set-leader '(normal visual motion) (kbd "SPC"))
  ;(evil-define-key 'normal 'global
  ;                 ";" 'evil-ex
  ;                 ":" 'evil-repeat-find-char
  ;                 (kbd "<leader>.") 'find-file
  ;                 (kbd "<leader>,") 'switch-to-buffer
  ;                 (kbd "<leader>gs" 'magit-status)
  ;                 "R"   '(lambda() (interactive) (load-file user-init-file))))
  (evil-set-leader 'normal (kbd "SPC"))

  (evil-define-key '(normal motion visual) 'global
    "/" 'consult-line
    ";" 'evil-ex
    ":" 'evil-repeat-find-char)

  (evil-define-key 'normal 'global
    ; misc
    ;"SPC" 'execute-extended-command
    (kbd "<leader>.")   'project-find-file
    ;(kbd "<leader>'")   '(lambda () (interactive) (term "/bin/bash"))
    ;(kbd "<leader>?")   'general-describe-keybindings
    (kbd "<leader>`")   '(lambda () (interactive) (dired user-emacs-directory))
    (kbd "<leader>R")   '(lambda() (interactive) (load-file user-init-file))
    (kbd "<leader>eb")  'eval-buffer
    (kbd "<leader>es")  'eval-last-sexp
    (kbd "<leader>ee")  'eval-expression
    (kbd "<leader>ef")  'eval-defun
    (kbd "<leader>+")   'text-scale-increase
    (kbd "<leader>_")   'text-scale-decrease
    (kbd "<leader>c")   'comment-line
    (kbd "<leader>C")   'comment-region
    ;(kbd "<leader>0")   '(lambda () (interactive) (serial-term "/dev/ttyUSB0" 115200))
    ;(kbd "<leader>1")   '(lambda () (interactive) (serial-term "/dev/ttyUSB1" 115200))
    ;(kbd "<leader>2")   '(lambda () (interactive) (serial-term "/dev/ttyUSB2" 115200))

    ; file s
    (kbd "<leader>-") 'consult-locate
    ;(kbd "<leader>ou" 'project-find-file ;'consult-fd
    (kbd "<leader>of") 'find-file
    (kbd "<leader>ou") 'consult-fd
    (kbd "<leader>oi") 'consult-ripgrep
    (kbd "<leader>og") 'consult-git-grep
    (kbd "<leader>oe") 'consult-buffer

    ; buffers
    (kbd "<leader>ba") 'consult-buffer
    (kbd "<leader>bb") 'consult-project-buffer
    (kbd "<leader>bl") 'list-buffers
    (kbd "<leader>bN") 'evil-buffer-new
    (kbd "<leader>bd") '((lambda () (interactive) (kill-buffer (current-buffer))))  ; this works more reliably than 'kill-this-buffer
    (kbd "<leader>bq") '((lambda () (interactive) (kill-buffer (current-buffer))))
    (kbd "<leader>bn") 'evil-next-buffer
    (kbd "<leader>bp") 'evil-prev-buffer
    (kbd "<leader>bs") 'save-buffer
    (kbd "<leader>bS") '((lambda () (interactive) (save-some-buffers t))) ; :which-key "save all")
    (kbd "<leader>bH") 'buf-move-left
    (kbd "<leader>bL") 'buf-move-right
    (kbd "<leader>bJ") 'buf-move-down
    (kbd "<leader>bK") 'buf-move-up
    (kbd "<leader>bC") 'my-write-copy-to-file

    ; windows
    (kbd "<leader>wd") 'delete-window
    (kbd "<leader>wq") 'delete-window
    (kbd "<leader>wv") 'evil-window-vsplit
    (kbd "<leader>ws") 'evil-window-split
    (kbd "<leader>wn") 'evil-window-next
    (kbd "<leader>wp") 'evil-window-prev
    (kbd "<leader>w>") '(lambda () (interactive) (evil-window-increase-width 10))
    (kbd "<leader>w<") '(lambda () (interactive) (evil-window-decrease-width 10))
    (kbd "<leader>w+") '(lambda () (interactive) (evil-window-increase-height 10))
    (kbd "<leader>w-") '(lambda () (interactive) (evil-window-decrease-height 10))

    ; project
    ;(kbd "<leader>pf") 'counsel-projectile-find-file
    ;(kbd "<leader>pd") 'counsel-projectile-find-dir
    ;(kbd "<leader>pg") 'counsel-projectile-grep
    ;(kbd "<leader>pa") 'counsel-projectile-ag
    ;(kbd "<leader>pr") 'counsel-projectile-rg
    ;(kbd "<leader>ps") '(lambda () (interactive) (counsel-projectile-ag "-s"))
    ;(kbd "<leader>pb") 'counsel-projectile-switch-to-buffer
    (kbd "<leader>p") 'project-switch-project ;'counsel-projectile-switch-project

    ; toggles
    ;"<leader>ts" 'neotree-toggle
    (kbd "<leader>ts") 'treemacs
    (kbd "<leader>tw") 'whitespace-mode

    ; help
    (kbd "<leader>hp") 'describe-point
    (kbd "<leader>hf") 'describe-function ;'counsel-describe-function
    (kbd "<leader>hv") 'describe-variable ;'counsel-describe-variable
    (kbd "<leader>hk") 'describe-key

    ;; lsp symbol stuff
    (kbd "<leader>s,") 'xref-find-definitions
    (kbd "<leader>s.") 'xref-find-definitions-other-window
    (kbd "<leader>so") 'xref-go-back
    (kbd "<leader>sr") 'xref-find-references
    (kbd "<leader>sc") 'xref-find-references-and-replace)
    ;(kbd "<leader>sp") 'lsp-ui-peek-find-definitions
    ;(kbd "<leader>s'") 'lsp-ui-peek-find-references
    ;(kbd "<leader>s,") 'lsp-ui-peek--goto-xref-other-window
    ;;(kbd "<leader>s,") 'lsp-ui-peek--goto-xref
    ;(kbd "<leader>sr") 'lsp-rename

    ;; chat/irc/slack
    ;"(kbd <leader>if") '((lambda () (interactive) (irc-freenode-connect)) :which-key "Freenode")
    ;"(kbd <leader>ib") '((lambda () (interactive) (erc-switch-to-buffer)) :which-key "ERC switch buffer")

    ; racket
    ;"(kbd <leader>rR") 'racket-run
    ;"(kbd <leader>rr") 'racket-run-and-switch-to-repl
    ;"(kbd <leader>rp") 'racket-repl
    ;"(kbd <leader>re") 'racket-repl-switch-to-edit
    ;"(kbd <leader>rd") 'racket-doc

    ; rust
    ;; (kbd "<leader>rD") '((lambda () (interactive)
        ;; (let ((rustdir "~/projects/rust/"))
          ;; (progn (cd rustdir)
           ;; (setq default-directory rustdir))))
      ;; :which-key "cd ~/projects/rust")
    ;; (kbd "<leader>rn") 'cargo-process-new
    ;; (kbd "<leader>rr") 'cargo-process-run
    ;; (kbd "<leader>rd") 'cargo-process-doc
    ;; (kbd "<leader>rt") 'cargo-process-test
    ;; (kbd "<leader>rf") 'cargo-process-format
    ;; (kbd "<leader>rc") 'cargo-process-clean
    ;; (kbd "<leader>rb") 'cargo-process-build

    ;  ; magit
    ;  (kbd "<leader>md") 'magit-diff-unstaged
    ;  (kbd "<leader>mD") 'magit-diff-buffer-file
    ;  (kbd "<leader>mg") 'magit-diff-staged
    ;  (kbd "<leader>ml") 'magit-log-all
    ;  (kbd "<leader>ms") 'magit-status
    ;  (kbd "<leader>mc") 'magit-branch-checkout
    ;  (kbd "<leader>mb") 'magit-blame


    ;; Center active search highlight
  (advice-add 'evil-search-next :after
    (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
  (advice-add 'evil-search-previous :after
    (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos)))))


;;; evil keybindings for other parts of emacs
(use-package evil-collection
  :ensure
  :after evil
  :config
  (evil-collection-init))

;;; Use escape (or custom escape sequence e.g. "jk"), to escape from everything
(use-package evil-escape
  :ensure
  :after evil)


;;; Quick way to move or target something with 2-char sequence
(use-package evil-snipe
  :ensure
  :after evil
  :config
  (evil-snipe-override-mode 1))

;;; Quickly change surrounding tags/quotes etc...
(use-package evil-surround
  :ensure
  :after evil
  :config
  (global-evil-surround-mode 1))

;;; Align things like a list of key=value lines to have the = signs all line up on the same column
(use-package evil-lion
  :ensure
  :config
  (evil-lion-mode))

(use-package vimish-fold
  :ensure
  :after evil)

(use-package evil-vimish-fold
  :ensure
  :after vimish-fold
  :hook ((prog-mode text-mode conf-mode) . evil-vimish-fold-mode))

;; Like evil-surround, but can be configured to support custom stuff
;(use-package evil-embrace)

;(use-package evil-args)

;(use-package evil-indent-plus
;  :ensure)

;(use-package evil-nerd-commenter
;  :ensure)

(use-package buffer-move
  :ensure)

(provide 'init-evil)
