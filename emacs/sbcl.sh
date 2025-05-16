#!/usr/bin/sbcl --script

#sbcl --load ~/quicklisp/quicklisp.lisp

(load "~/quicklisp/quicklisp.lisp")
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(ql:quickload \"quicklisp-slime-helper\")

