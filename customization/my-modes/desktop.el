(use-package desktop
  :ensure
  :config

  ;; desktop 
  (desktop-save-mode 1)
  (setq desktop-load-locked-desktop t
        desktop-dirname "~/.emacs.d/desktop/"
        desktop-path (list desktop-dirname)
        desktop-save t 
        desktop-auto-save-timeout 120
        desktop-restore-eager 0 ; don't load any buffers at start as the loading of buffers might trigger major modes which would
        )

  ;; disable modes on startup
  (add-to-list 'desktop-minor-mode-table (list 'whitespace-mode nil)) 
  (add-to-list 'desktop-minor-mode-table (list 'agda2-mode nil)) 

  (add-hook 'desktop-after-read-hook 
            #'(lambda () 
                (delete-other-frames) ; single frame on startup 
                ))

  (many 1 (apply-partially 'add-to-list 'desktop-globals-to-save) 
        'extended-command-history 
        'kill-ring)
)
