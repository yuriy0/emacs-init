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
  (require 'yaml)

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

;; something inside flycheck wants to use `yaml' but doesn't correctly depend on it?
(use-package yaml
  :ensure
  :defer t)

;; (use-package flycheck-posframe
;;   :ensure t
;;   :after flycheck
;;   :config

;;   (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
;;   (setq flycheck-posframe-border-width 1)
;; )


;;;###autoload
(defun my/posframe-mouse-banish-function-do-nothing(&rest _)
)

;;;###autoload
(defun my/flycheck-inline-display-function (msg pos err)
  (let ((posframe-mouse-banish-function #'my/posframe-mouse-banish-function-do-nothing))
    (posframe-show "*lsp-flycheck-inline*"
                   :string msg
                   :position pos
                   :border-width 1
                   :border-color "red"
                   :hidehandler 'posframe-hidehandler-when-buffer-switch
                   )
    )
  "*lsp-flycheck-inline*"
)

;;;###autoload
(defun my/flycheck-inline-clear-function ()
  (posframe-delete "*lsp-flycheck-inline*")
)

(use-package flycheck-inline
  :ensure t
  :after flycheck
  :config

  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)
    (require 'posframe))

  (setq flycheck-inline-display-function #'my/flycheck-inline-display-function
        flycheck-inline-clear-function #'my/flycheck-inline-clear-function)
)

