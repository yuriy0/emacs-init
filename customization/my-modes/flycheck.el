(use-package flycheck
  :ensure
  :commands (flycheck-mode)
  :autoload (hydra-flycheck-with-errs-list)

  :delight flycheck-mode

  :bind
  (:map flycheck-mode-map
        ("C-c !" . #'hydra-flycheck-with-errs-list))
)

;;;###autoload
(defun hydra-flycheck-with-errs-list ()
  "Opens flycheck error list buffer and starts flycheck navigation hydra"
  (interactive)
  (flycheck-list-errors)
  (hydra-flycheck/body))

;;;###autoload
(defun flycheck-errors-list-kill-buffer ()
  (interactive)
  (with-current-buffer "*Flycheck errors*" (kill-buffer-and-window)))

(defhydra hydra-flycheck ()
  "Move around flycheck errors"
  ("n" flycheck-next-error "next")
  ("p" flycheck-previous-error "prev")
  ("f" flycheck-first-error "first")
  ("q" nil "break")
  ("Q" flycheck-errors-list-kill-buffer "close"))
