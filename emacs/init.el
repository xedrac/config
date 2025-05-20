;; -*- lexical-binding: t; -*-

;;; Add conf folder to the load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;(add-to-list 'default-frame-alist '(background-color . "#232326"))

;;; Limit garbage collection to idle times (use gcmh-mode)
;;;   NOTE:  This will probably be unnecessary once the igc branch is merged in emacs
(require 'init-gcmh)
(gcmh-mode 1)

;(add-hook 'emacs-startup-hook
;          (lambda ()
;            (set-frame-parameter nil 'background-color "#232326")))

;; A big contributor to startup times is garbage collection.  We up the GC
;; threshold to temporarily prevent it from running, and then reset it later
;; using a hook.
;(setq gc-cons-threshold most-positive-fixnum
;      gc-cons-percentage 0.6)

;; Set the file-name-handler to nil (because regexing is cpu intensive)
;; But, keep a ref to it so we can hook it in later
;(defvar default-file-name-handler-alist file-name-handler-alist)
;(setq file-name-handler-alist nil)
;(add-hook 'emacs-startup-hook
;  (lambda ()
;    (setq gc-cons-threshold 16777216
;          gc-cons-percentage 0.1
;          file-name-handler-alist default-file-name-handler-alist)))

;; For native comp
;(when (fboundp 'native-compile-async)
;  (setq comp-async-jobs-number 15
;        comp-deferred-compilation t
;        comp-deferred-compilation-black-list '()))

;(load (expand-file-name "~/.roswell/helper.el"))


;;; Core modules
(require 'elpaca-bootstrap)
(require 'cl-lib)

;;; My modules
(require 'init-core)
(require 'init-theme)
(require 'init-evil)
(require 'init-completions)
(require 'init-searching)
(require 'init-packages)
(require 'init-keybindings)
(require 'init-treesitter)
(require 'init-languages)
(require 'init-filetree)
(require 'init-git)

;(add-hook 'after-init-hook
;          (lambda ()
;            (set-background-color "#232326")))

(provide 'init)

;;; init ends here
