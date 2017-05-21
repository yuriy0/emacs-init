;; eshell
(require 'helm-eshell)
(fset 'shell 'eshell) ; replace shell with eshell 
(add-hook 'eshell-mode-hook 
  (lambda () 
    (define-key eshell-mode-map (kbd "M-]") 'helm-eshell-history)
    (setq eshell-cmpl-ignore-case t)
    (eshell-cmpl-initialize)
    (set-face-attribute 'eshell-prompt nil :foreground "chartreuse4")
    (push-mark)
    ))
(setq eshell-prompt-function #'(lambda nil
  (concat
    (getenv "USER") "@" (system-name) ":" (abbreviate-file-name (eshell/pwd))
    (if (= (user-uid) 0) " # " " $ "))))