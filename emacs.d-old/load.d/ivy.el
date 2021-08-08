;; ivy.el

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 30)
  (setq ivy-count-format "(%d/%d" )
  (setq ivy-display-style 'fancy)
  (setq ivy-format-function 'ivy-format-function-line)
  ;(setq ivy-re-builders-alist
  ;  '((counsel-M-x . ivy--regex-fuzzy)
  ;    (t . ivy--regex-plus)))

  (setq ivy-initial-inputs-alist nil)
  (setq enable-recursive-minibuffers t))
