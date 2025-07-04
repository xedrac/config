
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




(provide 'init-functions)
