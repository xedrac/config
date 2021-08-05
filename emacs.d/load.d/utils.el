;; utils.el

(require 'diminish)

; Allows for renaming mode indicators on the modeline
; This is useful if they are too long
(defmacro rename-major-mode (package-name mode new-name)
 `(eval-after-load ,package-name
   '(defadvice ,mode (after rename-modeline activate)
      (setq mode-name ,new-name))))

(defmacro rename-minor-mode (package mode new-name)
 `(eval-after-load ,package
   '(diminish ',mode ,new-name)))

(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "elisp")))

(rename-minor-mode "company" company-mode "comp")
(rename-minor-mode "eldoc" eldoc-mode "doc")
(rename-minor-mode "undo-tree" undo-tree-mode nil)
(rename-minor-mode "flycheck" flycheck-mode "fly")
;(rename-minor-mode "projectile" projectile-mode "proj")
(rename-minor-mode "lsp" lsp-mode "lsp")
(rename-minor-mode "abbrev" abbrev-mode "abrv")

