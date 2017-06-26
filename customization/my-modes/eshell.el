;; eshell
(require 'helm-eshell)
(require 'em-prompt)

(add-hook 'eshell-mode-hook 
  (lambda () 
    (define-key eshell-mode-map (kbd "M-]") 'helm-eshell-history)
    (setq eshell-cmpl-ignore-case t)
    (eshell-cmpl-initialize)
    (set-face-attribute 'eshell-prompt nil :foreground "chartreuse4")
    (push-mark)
    (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
    ))

(setq eshell-prompt-function #'(lambda ()
  (let ((sp (propertize " " 'face '(:background "#fff"))))
    (concat
     sp (abbreviate-file-name (eshell/pwd)) "\n"
     sp (if (= (user-uid) 0) "#" "$") " ")) ))
