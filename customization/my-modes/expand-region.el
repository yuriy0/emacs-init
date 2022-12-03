(use-package expand-region
  :ensure t
  :commands (er/expand-region)

  :init
  (bind-keys
   ((if (display-graphic-p) "C-=" "C-]") ;; TODO: how to use all keybinds correctly in TTY?
    . er/expand-region))

  :config
  ;; empty
  )
