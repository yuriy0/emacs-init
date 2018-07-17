;; C
(require 'cc-mode)

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
