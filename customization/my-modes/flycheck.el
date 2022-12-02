(use-package flycheck
  :ensure
  :commands (flycheck-mode)

  :delight flycheck-mode

  :bind
  (:map flycheck-mode-map
        ("C-c !" . #'flycheck-list-errors))

  (:map flycheck-command-map
        ("q" . #'flycheck-errors-list-kill-buffer))

  :config
  (with-eval-after-load 'hercules
    (hercules-def
     :show-funs #'flycheck-list-errors
     :hide-funs '(flycheck-errors-list-kill-buffer)
     :keymap 'flycheck-command-map
     :transient t)
    )
)

;;;###autoload
(defun flycheck-errors-list-kill-buffer ()
  (interactive)
  (with-current-buffer "*Flycheck errors*" (kill-buffer-and-window)))
