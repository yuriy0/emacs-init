(use-package expand-region
  :ensure t
  :commands (er/expand-region)

  :config
  (bind-keys
   ((if (display-graphic-p) "C-=" "C-]") ;; TODO: how to use all keybinds correctly in TTY?
    . er/expand-region))
  )
