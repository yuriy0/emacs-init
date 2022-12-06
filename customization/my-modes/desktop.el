;; -*- lexical-binding: t; -*-

(use-package desktop-lazy-read
   :load-path "lisp-pkg/desktop-lazy-read"
   :defer t)

(use-package desktop
  :ensure
  :demand t
  :config

  (setq desktop-load-locked-desktop t
        desktop-dirname "~/.emacs.d/desktop/"
        desktop-path (list desktop-dirname)
        desktop-save t 
        desktop-auto-save-timeout 120
        desktop-restore-forces-onscreen nil
        )

  ;; disable restoring of some minor modes on desktop read
  (add-to-list 'desktop-minor-mode-table (list 'whitespace-mode nil))
  (many 1 (apply-partially 'add-to-list 'desktop-minor-mode-handlers)
        (cons 'lsp-mode #'identity)
        (cons 'flycheck-mode #'identity)
        (cons 'agda2-mode #'identity)
        )

  ;; save some additional global properties
  (many 1 (apply-partially 'add-to-list 'desktop-globals-to-save) 
        'extended-command-history
        'kill-ring)

  (require 'desktop-lazy-read)

  ;; in non-server mode, the frame will already be ready. in server mode, we
  ;; wait until the first frame is created for the first client.
  (if (not (daemonp))
      (progn
        (desktop-save-mode 1))

    (defun restore-desktop (frame)
      (with-selected-frame frame
        (desktop-save-mode 1)
        (desktop-read))
      t)
    (add-hook-once 'after-make-frame-functions #'restore-desktop))
  )
