(use-package smart-mode-line
  :ensure
  :config

  (setq sml/theme 'light)
  (sml/setup)
)

(use-package smart-mode-line-powerline-theme
  :ensure
  :after (smart-mode-line)
  :config

  (sml/apply-theme 'light-powerline)
)
