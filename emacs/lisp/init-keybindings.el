;;; keybindings.el

;;; Custom leader key
;(general-create-definer leader-define-key
;  :prefix "SPC"
;  :non-normal-prefix "M-SPC"
;
;;; Custom leader keybindings
;(leader-define-key
;  :init (general-def :states '(normal visual motion) "SPC" nil)  ; unbind SPC so we can use it as prefix
;  :states '(normal visual motion)

;(evil-set-leader 'normal (kbd "SPC"))

(with-eval-after-load 'evil
  (evil-define-key '(normal motion visual) 'global
    "/" 'consult-line
    ";" 'evil-ex
    ":" 'evil-repeat-find-char)

  (evil-define-key 'normal 'global
    ; misc
    ;"SPC" 'execute-extended-command
    (kbd "<leader>.")   'project-find-file
    (kbd "<leader>,")   'consult-buffer
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
    (kbd "<leader>sc") 'xref-find-references-and-replace))
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





  ; TODO debugging
  ; TODO email?
  ; TODO org mode?

;; Normal keybindings
;(general-define-key
;  :states '(normal visual motion)
;  "/" 'consult-line)
;  ;"/" 'swiper)


;;; Swap ; and : for convenience
;(general-define-key
;  :states 'normal
;  ";" 'evil-ex
;  ":" 'evil-repeat-find-char)

;;; Rename the which-key prefixes
;(which-key-add-key-based-replacements
;  "SPC b"  "buffers"
;  "SPC f"  "files"
;  "SPC h"  "help"
;  "SPC p"  "projects"
;  "SPC t"  "toggles"
;  "SPC w"  "windows"
;  "SPC e"  "eval"
;  "SPC s"  "symbols"
;  ;"SPC r"  "rust"
;  ;"SPC r c" "cargo"
;  "SPC i"  "irc")


;; Global keybindings
;(general-define-key
;  "\C-s" (lambda () (save-buffer 't))
;  "\C-S" (lambda () (save-some-buffers 't)))

;(general-define-key "C->" 'indent-rigidly-right-to-tab-stop)
;(general-define-key "C-<" 'indent-rigidly-left-to-tab-stop)

;(general-define-key
;  :states '(motion normal)
;  ;">" 'indent-rigidly-right-to-tab-stop)
;  ">" 'indent-rigidly-right)
;
;(general-define-key
;  :states '(motion normal)
;  ;"<" 'indent-rigidly-left-to-tab-stop)
;  "<" 'indent-rigidly-left)


;; Rapid key sequences for insert mode
;(use-package key-seq
;  :ensure t
;  :init
;  (key-chord-mode 1)
;  :config
;  (key-seq-define evil-insert-state-map "qs" #'save-buffer)
;  (key-seq-define evil-insert-state-map "hc" #'evil-escape))


;; Allow M-x in serial term
(eval-after-load 'term
  '(define-key term-raw-map (kbd "M-x") #'execute-extended-command))


;; describe the elisp function/variable under the cursor (aka point)
(defun describe-point ()
    "Show the documentation of the Elisp function and/or variable near point.
This checks in turn:
-- for a function name where point is
-- for a variable name where point is
-- for a surrounding function call
"
    (interactive)
    (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
     (cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                           (save-excursion
                               (or (not (zerop (skip-syntax-backward "_w")))
                                (eq (char-syntax (char-after (point))) ?w)
                                (eq (char-syntax (char-after (point))) ?_)
                                (forward-sexp -1))
                               (skip-chars-forward "`'")
                               (let ((obj (read (current-buffer))))
                                (and (symbolp obj) (fboundp obj) obj))))))
            (describe-function sym))
         ((setq sym (variable-at-point)) (describe-variable sym))
         ;; now let it operate fully -- i.e. also check the
         ;; surrounding sexp for a function call.
         ((setq sym (function-at-point)) (describe-function sym)))))

(provide 'init-keybindings)
