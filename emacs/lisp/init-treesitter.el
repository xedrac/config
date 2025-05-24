;(require 'treesit)
;
;;;; Automatically enable <lang>-ts-mode for appropriate modes
;(use-package treesit-auto
;  :ensure (:host github :repo "renzmann/treesit-auto")
;  :custom
;  (treesit-auto-install 'prompt)
;  :config
;  (treesit-auto-add-to-auto-mode-alist 'all)
;  (global-treesit-auto-mode))







;;(setq treesit-language-source-alist
;;  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;    (c "https://github.com/tree-sitter/tree-sitter-c")
;;    ;(c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
;;    (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.5")
;;    (cmake "https://github.com/uyha/tree-sitter-cmake")
;;    (commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
;;    ;(css "https://github.com/tree-sitter/tree-sitter-css")
;;    (cuda "https://github.com/tree-sitter-grammars/tree-sitter-cuda")
;;    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;    (glsl "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
;;    ;(go "https://github.com/tree-sitter/tree-sitter-go")
;;    (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
;;    (hlsl "https://github.com/tree-sitter-grammars/tree-sitter-hlsl")
;;    (html "https://github.com/tree-sitter/tree-sitter-html")
;;    ;(java "https://github.com/tree-sitter/tree-sitter-java")
;;    ;(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;    (json "https://github.com/tree-sitter/tree-sitter-json")
;;    ;(julia "https://github.com/tree-sitter/tree-sitter-julia")
;;    (lua "https://github.com/tree-sitter/tree-sitter-lua")
;;    (make "https://github.com/tree-sitter-grammars/tree-sitter-make")
;;    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;    ;(ocaml "https://github.com/tree-sitter/tree-sitter-ocaml")
;;    ;(php "https://github.com/tree-sitter/tree-sitter-php")
;;    (python "https://github.com/tree-sitter/tree-sitter-python")
;;    ;(regex "https://github.com/tree-sitter/tree-sitter-regex")
;;    ;(ruby "https://github.com/tree-sitter/tree-sitter-ruby")
;;    (rust "https://github.com/tree-sitter/tree-sitter-rust")
;;    ;(scala "https://github.com/tree-sitter/tree-sitter-scala")
;;    ;(swift "https://github.com/tree-sitter/tree-sitter-swift")
;;    (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;    ;(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;    ;(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;    (udev "https://github.com/tree-sitter/tree-sitter-udev")
;;    (verilog "https://github.com/tree-sitter/tree-sitter-verilog")
;;    (xml "https://github.com/tree-sitter-grammars/tree-sitter-xml")
;;    (yaml "https://github.com/ikatyang/tree-sitter-yaml")
;;))

;;dolist (source treesit-language-source-alist)
;;  (unless (treesit-ready-p (car source))
;;    (treesit-install-language-grammar (car source))))
;;
;;(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
;;(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;;(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(provide 'init-treesitter)
