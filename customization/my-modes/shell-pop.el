;; shell pop + eshell
(setq shell-pop-shell-type "eshell"
      shell-pop-universal-key "C-'") ; this doesn't work(??)
(global-set-keys (kbd "C-'") 'shell-pop)
