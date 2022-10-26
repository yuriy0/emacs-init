;; Web mode 
(use-package web-mode
  :ensure
  :commands (web-mode)

  :config

  (define-key web-mode-map (kbd "RET") 'electric-indent-just-newline)
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
)

(use-package web-server
  :ensure
  :after (web-mode)
)
