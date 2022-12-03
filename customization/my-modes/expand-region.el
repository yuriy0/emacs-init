(use-package expand-region
  :ensure t
  :commands (er/expand-region)

  :init
  (bind-keys
   ("C-=" . er/expand-region)
   )

  (add-hook
   'after-make-frame-functions
   (lambda ()
     ;; in TTY frames on Windows Terminal and other terminal emulators,
     ;; some keybinds like `C-=' don't work, so when such a frame is created,
     ;; bind a different key...
     ;; TODO: we should figure out how to use all keybinds correctly in TTY?
     (when (not (display-graphic-p))
       (bind-keys ("C-]" . er/expand-region)))
     )
   )

  :config
  ;; empty
  )
