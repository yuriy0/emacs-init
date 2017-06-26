;; eshell
(require 'helm-eshell)

;; just for *Help*
(mapc #'require '(em-alias em-banner em-basic em-cmpl em-dirs
      em-glob em-hist em-ls em-pred em-prompt em-rebind em-script
      em-smart em-term em-tramp em-unix em-xtra esh-arg esh-cmd
      esh-ext esh-groups esh-io esh-mode esh-module esh-opt
      esh-proc esh-util esh-var))

(add-hook 'eshell-mode-hook 
  (lambda () 
    (setq eshell-cmpl-ignore-case t)
    (eshell-cmpl-initialize)
    (set-face-attribute 'eshell-prompt nil :foreground "chartreuse4")
    (push-mark)
    (define-keys eshell-mode-map 
      (kbd "M-]") 'helm-eshell-history
      [remap eshell-pcomplete] 'helm-esh-pcomplete
      (kbd "<home>") 'eshell-bol
      )
    ))

(setq eshell-prompt-function #'(lambda ()
   (let ((sp (propertize " " 'face '(:background "#fff"))))
     (concat
      sp (abbreviate-file-name (eshell/pwd)) "\n"
      sp (if (= (user-uid) 0) "#" "$") " ")) ))
