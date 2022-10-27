(use-package smart-mode-line
  :disabled
  :ensure
  :config

  (setq sml/theme 'light)
  (sml/setup)
)

(use-package smart-mode-line-powerline-theme
  :disabled
  :ensure
  :require (smart-mode-line)
  :config

  (sml/apply-theme 'light-powerline)
)

(use-package spaceline-config
  :disabled
  :ensure spaceline

  :config
  (spaceline-emacs-theme)
  (spaceline-helm-mode)
  (spaceline-toggle-flycheck-error-off)
  (spaceline-toggle-flycheck-warning-off)
  (spaceline-toggle-flycheck-info-off)
)


(use-package sublimity
  :disabled ;; nice feature but causes heavy lag occasionally
  :ensure
  :config

  (sublimity-mode 1)
  (require 'sublimity-scroll)

  (setq sublimity-scroll-weight 10
        sublimity-scroll-drift-length 5)
)
