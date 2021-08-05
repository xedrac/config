;; keybindings.el

;; Custom leader key
(general-create-definer leader-define-key
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

;; Custom leader keybindings
(leader-define-key
  :init (general-def :states '(normal visual motion) "SPC" nil)  ; unbind SPC so we can use it as prefix
  :states '(normal visual motion)

  ; misc
  "SPC" '(counsel-M-x :which-key "run command")
  "'"   '((lambda () (interactive) (term "/bin/bash")) :which-key "terminal")
  "?"   '(general-describe-keybindings :which-key "keybindings")
  "`"   '(lambda () (interactive) (find-file "~/.emacs.d/keybindings.el"))
  "e b" 'eval-buffer
  "e s" 'eval-last-sexp
  "e e" 'eval-expression
  "e f" 'eval-defun
  "+"   'text-scale-increase
  "-"   'text-scale-decrease
  
  ; files
  "f f" 'counsel-find-file
  "f g" 'counsel-git
  "f r" 'counsel-fzf
  "f e k" '(lambda () (interactive) (find-file "~/.emacs.d/keybindings.el"))
  "f e d" '(lambda () (interactive) (dired "~/.emacs.d/load.d"))

  ; buffers
  "b b" 'ivy-switch-buffer
  "b l" 'list-buffers
  "b N" 'evil-buffer-new
  "b d" '((lambda () (interactive) (kill-buffer (current-buffer))) :which-key "kill-this-buffer")  ; this works more reliably than 'kill-this-buffer
  "b q" '((lambda () (interactive) (kill-buffer (current-buffer))) :which-key "kill-this-buffer")
  "b n" 'evil-next-buffer
  "b p" 'evil-prev-buffer
  "b s" 'save-buffer
  "b S" '((lambda () (interactive) (save-some-buffers t)) :which-key "save all")
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
  "p f" 'counsel-projectile-find-file
  "p d" 'counsel-projectile-find-dir
  "p g" 'counsel-projectile-grep
  "p a" 'counsel-projectile-ag
  "p r" 'counsel-projectile-rg
  "p s" '(lambda () (interactive) (counsel-projectile-ag "-s"))
  "p b" 'counsel-projectile-switch-to-buffer
  "p p" 'counsel-projectile-switch-project

  ; toggles
  "t n" 'neotree-toggle
  "t w" 'whitespace-mode

  ; help
  "h p" 'describe-point
  "h f" 'counsel-describe-function
  "h v" 'counsel-describe-variable
  "h k" 'describe-key

  ; lsp symbol stuff
  "s ." 'xref-find-definitions
  "s o" 'xref-find-definitions-other-window
  "s p" 'lsp-ui-peek-find-definitions
  "s '" 'lsp-ui-peek-find-references
  "s ," 'lsp-ui-peek--goto-xref-other-window
  ;"s ," 'lsp-ui-peek--goto-xref

  ;"s '" 'xref-find-references
  "s r" 'lsp-rename

  ; chat/irc/slack
  "i f" '((lambda () (interactive) (irc-freenode-connect)) :which-key "Freenode")
  "i b" '((lambda () (interactive) (erc-switch-to-buffer)) :which-key "ERC switch buffer")

  ; racket
  ;"r R" 'racket-run
  ;"r r" 'racket-run-and-switch-to-repl
  ;"r p" 'racket-repl
  ;"r e" 'racket-repl-switch-to-edit
  ;"r d" 'racket-doc

  ; rust
  "r D" '((lambda () (interactive)
	    (let ((rustdir "~/projects/rust/"))
	      (progn (cd rustdir)
		     (setq default-directory rustdir))))
	  :which-key "cd ~/projects/rust")
  "r n" 'cargo-process-new
  "r r" 'cargo-process-run
  "r d" 'cargo-process-doc
  "r t" 'cargo-process-test
  "r f" 'cargo-process-format
  "r c" 'cargo-process-clean
  "r b" 'cargo-process-build

  ; magit
  "m d" 'magit-diff-unstaged
  "m D" 'magit-diff-buffer-file
  "m g" 'magit-diff-staged
  "m l" 'magit-log-all
  "m s" 'magit-status
  "m c" 'magit-branch-checkout

  ; TODO debugging
  ; TODO email?
  ; TODO org mode?

  )

;; Global keybindings
(general-define-key
  "\C-s" 'swiper)

;; Normal keybindings
(general-define-key
  :states '(normal visual motion)
  "/" 'swiper)

;; Rename the which-key prefixes
(which-key-add-key-based-replacements
  "SPC b"  "buffers"
  "SPC f"  "files"
  "SPC h"  "help"
  "SPC p"  "projects"
  "SPC t"  "toggles"
  "SPC w"  "windows"
  "SPC e"  "eval"
  "SPC s"  "symbols"
  "SPC r"  "rust"
  "SPC r c" "cargo"
  "SPC i"  "irc")


;; Rapid key sequences for insert mode
(use-package key-seq
  :ensure t
  :init
  (key-chord-mode 1)
  :config
  (key-seq-define evil-insert-state-map "qs" #'save-buffer)
  (key-seq-define evil-insert-state-map "hc" #'evil-escape))


;; Center active search highlight
(advice-add 'evil-search-next :after
  (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
(advice-add 'evil-search-previous :after
  (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))



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
