;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 1) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(setq evil-want-keybinding nil)
;(setq mode-require-final-newline nil)

;; Disable built-in package manager so that we can use elpaca instead
(setq package-enable-at-startup nil)

;; Set default font
;(set-face-attribute 'default nil
;                    ;:family "Ubuntu Mono"
;                    ;:family "Source Code Pro-14"
;                    :family "Liberation Mono Regular-14"
;                    :height 130
;                    :weight 'normal
;                    :width 'normal)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)
