;; init.el

;; for native-comp branch
(when (fboundp 'native-compile-async)
  (setq comp-async-jobs-number 4 ;; not using all cores
        comp-deferred-compilation t
        comp-deferred-compilation-black-list '()))


; Load emacs settings
(load "~/.emacs.d/settings.el")

; Load my packages
(load "~/.emacs.d/packages.el")

; Load my keybindings
(load "~/.emacs.d/keybindings.el")

; Load generated custom variables
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
