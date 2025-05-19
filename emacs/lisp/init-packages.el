;; -*- lexical-binding: t; -*-

(use-package vterm
  :ensure t)

;;; Different undo/redo behavior
;(use-package undo-tree
;  :ensure t
;  :init (global-undo-tree-mode))

;;; Visualize the undo tree (vundo)
;(use-package vundo
;  :ensure t)


;;; Show recent commands/files at the top
(require 'savehist)
(savehist-mode)

(require 'which-key)
(which-key-mode)

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  ;(setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))


;; Indent code somewhat sanely when pasting
(use-package snap-indent
  :ensure t
  :hook (prog-mode . snap-indent-mode)
  :custom ((snap-indent-format '(untabify delete-trailing-whitespace))
           (snap-indent-on-save nil)))


;;; Benchmarking tool for testing purposes
;;(use-package esup
;;    :config
;;    ;; Workaround an issue with esup and byte-compiled cl-lib
;;    ;; but this also seems to make it less useful
;;    (setq esup-depth 0)


(provide 'init-packages)
