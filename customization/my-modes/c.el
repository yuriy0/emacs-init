;; C
(require 'cc-mode)

;; Lumberyard/CryEngine shader files
(add-to-list 'auto-mode-alist '("\\.cfi\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cfx\\'" . c-mode))

(add-hook 'c-mode-common-hook
  #'(lambda()
      (setq
       c-default-style "bsd"
       c-basic-offset 4
       tab-width 4
       indent-tabs-mode nil
       )
    )
)
