(use-package which-key
  :ensure
  :defer 10 ;; which-key mainly functions by an idle timer so we have good way to determine when this is needed

  :config
  (which-key-mode)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'right)
  (setq which-key-side-window-max-width 0.45)
  (setq which-key-idle-delay 3.0)
  (define-key global-map (kbd "C-h /") 'which-key-show-major-mode)
)
