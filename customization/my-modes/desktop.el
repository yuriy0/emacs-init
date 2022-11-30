(use-package desktop
  :ensure
  :config
  (setq desktop-load-locked-desktop t
        desktop-dirname "~/.emacs.d/desktop/"
        desktop-path (list desktop-dirname)
        desktop-save t 
        desktop-auto-save-timeout 120
        desktop-restore-eager 0 ; don't load any buffers at start as the loading of buffers might trigger major modes which would
        )

  ;; disable restoring of some minor modes on desktop read
  (add-to-list 'desktop-minor-mode-table (list 'whitespace-mode nil))
  (many 1 (apply-partially 'add-to-list 'desktop-minor-mode-handlers)
        (cons 'lsp-mode #'identity)
        (cons 'flycheck-mode #'identity)
        (cons 'agda2-mode #'identity)
        )

  (add-hook 'desktop-after-read-hook 
            #'(lambda () 
                (delete-other-frames) ; single frame on startup 
                ))

  ;; save some additional global properties
  (many 1 (apply-partially 'add-to-list 'desktop-globals-to-save) 
        'extended-command-history
        'kill-ring)

  ;; for debugging, print any saved buffers which fail to load
  (defun my/desktop-create-buffer (fn &rest rest)
    (condition-case the-err
        (apply fn rest)
      (t
       (message "desktop-create-buffer%s error: %s" rest the-err))
      )
    )
  (advice-add 'desktop-create-buffer :around #'my/desktop-create-buffer)

  ;; add a hook to restore the desktop at the appropriate time
  ;; check if this is a server emacs, if so, add the hook to a function called
  ;; once the first frame is created
  (add-hook-once
   (if
       (and (boundp 'server-process)
            (processp server-process)
            (server-running-p))
       'server-after-make-frame-hook
     'after-init-hook)
   (lambda()
     (run-with-timer
      5
      nil
      (lambda()
        (desktop-save-mode 1)
        (desktop-read)))
     t))
)
