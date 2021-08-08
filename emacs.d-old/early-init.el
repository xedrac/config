;; early-init.el

(require 'package)

; Add package repositories
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(setq package-quickstart t)

(provide 'early-init)
