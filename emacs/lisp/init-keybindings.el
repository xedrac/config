;;; keybindings.el

;;; Custom leader key
(general-create-definer leader-define-key
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

;; Custom leader keybindings
(leader-define-key
  :init (general-def :states '(normal visual motion) "SPC" nil)  ; unbind SPC so we can use it as prefix
  :states '(normal visual motion)

  ; misc
  ;"SPC" 'execute-extended-command
  "."   'project-or-external-find-file
  ;"'"   '(lambda () (interactive) (term "/bin/bash"))
  "'"   'term ;'project-eshell
  "?"   'general-describe-keybindings
  "`"   '(lambda () (interactive) (dired (expand-file-name "lisp/" user-emacs-directory)))
  ;"`"   '(lambda () (interactive) (find-file (expand-file-name "lisp/init-keybindings.el" user-emacs-directory)))
  "~"   '(lambda () (interactive) (find-file (expand-file-name "lisp/init-packages.el" user-emacs-directory)))
  "R"   '(lambda() (interactive) (load-file user-init-file))
  "e b" 'eval-buffer
  "e s" 'eval-last-sexp
  "e e" 'eval-expression
  "e f" 'eval-defun
  "+"   'text-scale-increase
  "_"   'text-scale-decrease
  "c"   'comment-line
  "C"   'comment-region
  "0"   '(lambda () (interactive) (serial-term "/dev/ttyUSB0" 115200))
  "1"   '(lambda () (interactive) (serial-term "/dev/ttyUSB1" 115200))
  "2"   '(lambda () (interactive) (serial-term "/dev/ttyUSB2" 115200))

  ; files
  "-" 'consult-locate
  ;"o u" 'project-find-file ;'consult-fd
  "o u" 'consult-fd
  "o i" 'consult-ripgrep
  "o g" 'consult-git-grep
  "o e" 'consult-buffer

  ; buffers
  "b a" 'consult-buffer
  "b b" 'consult-project-buffer
  "b l" 'list-buffers
  "b N" 'evil-buffer-new
  "b d" '((lambda () (interactive) (kill-buffer (current-buffer))))  ; this works more reliably than 'kill-this-buffer
  "b q" '((lambda () (interactive) (kill-buffer (current-buffer))))
  "b n" 'evil-next-buffer
  "b p" 'evil-prev-buffer
  "b s" 'save-buffer
  "b S" '((lambda () (interactive) (save-some-buffers t))) ; :which-key "save all")
  "b H" 'buf-move-left
  "b L" 'buf-move-right
  "b J" 'buf-move-down
  "b K" 'buf-move-up
  "b C" 'my-write-copy-to-file

  ; windows
  "w d" 'delete-window
  "w q" 'delete-window
  "w v" 'evil-window-vsplit
  "w s" 'evil-window-split
  "w n" 'evil-window-next
  "w p" 'evil-window-prev
  "w >" '(lambda () (interactive) (evil-window-increase-width 10))
  "w <" '(lambda () (interactive) (evil-window-decrease-width 10))
  "w +" '(lambda () (interactive) (evil-window-increase-height 10))
  "w -" '(lambda () (interactive) (evil-window-decrease-height 10))

  ; project
  ;"p f" 'counsel-projectile-find-file
  ;"p d" 'counsel-projectile-find-dir
  ;"p g" 'counsel-projectile-grep
  ;"p a" 'counsel-projectile-ag
  ;"p r" 'counsel-projectile-rg
  ;"p s" '(lambda () (interactive) (counsel-projectile-ag "-s"))
  ;"p b" 'counsel-projectile-switch-to-buffer
  "p" 'project-switch-project ;'counsel-projectile-switch-project

  ; toggles
  "t s" 'neotree-toggle
  ;"t s" 'treemacs
  "t w" 'whitespace-mode

  ; help
  "h p" 'describe-point
  "h f" 'describe-function ;'counsel-describe-function
  "h v" 'describe-variable ;'counsel-describe-variable
  "h k" 'describe-key

  ;; lsp symbol stuff
  "s ," 'xref-find-definitions
  "s ." 'xref-find-definitions-other-window
  "s o" 'xref-go-back
  "s r" 'xref-find-references
  "s c" 'xref-find-references-and-replace)
  ;"s p" 'lsp-ui-peek-find-definitions
  ;"s '" 'lsp-ui-peek-find-references
  ;"s ," 'lsp-ui-peek--goto-xref-other-window
  ;;"s ," 'lsp-ui-peek--goto-xref
  ;"s r" 'lsp-rename

  ;; chat/irc/slack
  ;"i f" '((lambda () (interactive) (irc-freenode-connect)) :which-key "Freenode")
  ;"i b" '((lambda () (interactive) (erc-switch-to-buffer)) :which-key "ERC switch buffer")

  ; racket
  ;"r R" 'racket-run
  ;"r r" 'racket-run-and-switch-to-repl
  ;"r p" 'racket-repl
  ;"r e" 'racket-repl-switch-to-edit
  ;"r d" 'racket-doc

  ; rust
  ;; "r D" '((lambda () (interactive)
      ;; (let ((rustdir "~/projects/rust/"))
        ;; (progn (cd rustdir)
         ;; (setq default-directory rustdir))))
    ;; :which-key "cd ~/projects/rust")
  ;; "r n" 'cargo-process-new
  ;; "r r" 'cargo-process-run
  ;; "r d" 'cargo-process-doc
  ;; "r t" 'cargo-process-test
  ;; "r f" 'cargo-process-format
  ;; "r c" 'cargo-process-clean
  ;; "r b" 'cargo-process-build

;  ; magit
;  "m d" 'magit-diff-unstaged
;  "m D" 'magit-diff-buffer-file
;  "m g" 'magit-diff-staged
;  "m l" 'magit-log-all
;  "m s" 'magit-status
;  "m c" 'magit-branch-checkout
;  "m b" 'magit-blame


  ; TODO debugging
  ; TODO email?
  ; TODO org mode?

;; Normal keybindings
(general-define-key
  :states '(normal visual motion)
  "/" 'consult-line)
  ;"/" 'swiper)


;;; Swap ; and : for convenience
(general-define-key
  :states 'normal
  ";" 'evil-ex
  ":" 'evil-repeat-find-char)

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

(general-define-key
  :states '(motion normal)
  ;">" 'indent-rigidly-right-to-tab-stop)
  ">" 'indent-rigidly-right)

(general-define-key
  :states '(motion normal)
  ;"<" 'indent-rigidly-left-to-tab-stop)
  "<" 'indent-rigidly-left)


;; Rapid key sequences for insert mode
;(use-package key-seq
;  :ensure t
;  :init
;  (key-chord-mode 1)
;  :config
;  (key-seq-define evil-insert-state-map "qs" #'save-buffer)
;  (key-seq-define evil-insert-state-map "hc" #'evil-escape))


;; Center active search highlight
(advice-add 'evil-search-next :after
  (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
(advice-add 'evil-search-previous :after
  (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))

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
