
(use-package php-mode
  :ensure
  :commands (php-mode)

  :config

  (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
)
