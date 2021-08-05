
(defun irc-freenode-connect ()
  (erc-tls :server "chat.freenode.net"
           :port 6697
           :nick "sbadger"
           :password "insight"))

(setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))
