;; packages.el

; Packages to install at launch, if not present
; Most packages are loaded from load.d/ via use-package,
; these initial packages are just for getting use-package up and running
(defvar my-packages
  '(
    use-package
    general
    load-dir
    diminish
    delight))

(require 'seq)

(defun install-missing-packages ()
  (let ((missing (seq-remove (lambda (p) (package-installed-p p)) my-packages)))
    (when missing
      (package-refresh-contents)
      (dolist (p missing)
        (package-install p)))))

(install-missing-packages)

; Allow use-package to use :diminish :delight to modify mode strings
(require 'diminish)
(require 'delight)
(require 'general)

; Load *.el files in ~/emacs.d/load.d/
(require 'load-dir)
(custom-set-variables '(load-dirs t))
(load-dirs)

