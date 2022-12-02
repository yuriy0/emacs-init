(use-package which-key
  :ensure
  :defer 10 ;; which-key mainly functions by an idle timer so we have no good way to determine when this is needed
  :delight which-key-mode

  :config
  (which-key-mode)
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'right
        which-key-show-prefix 'top
        which-key-side-window-max-width 0.45
        which-key-idle-delay 2.5
        which-key-idle-secondary-delay 0.05
        which-key-show-early-on-C-h t
        )

  (define-key global-map (kbd "C-h /") 'which-key-show-major-mode)
)
