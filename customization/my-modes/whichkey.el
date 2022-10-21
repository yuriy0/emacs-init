(use-package which-key
  :ensure

  :config
  (which-key-mode)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'right)
  (setq which-key-side-window-max-width 0.45)
  (setq which-key-idle-delay 3.0)
  (define-key global-map (kbd "C-h /") 'which-key-show-major-mode)
)
