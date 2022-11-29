;; Web mode
(use-package web-mode
  :ensure
  :commands (web-mode)

  :init
  (mapc (lambda(f)
          (add-to-list 'auto-mode-alist `(,f . web-mode)))
        '("\\.jsx?$"
          "\\.js?$"
          "\\.html?$"
          "\\.css?$"
          "\\.ts?$"
          "\\.tsx?$"
          ))

  :config

  (define-key web-mode-map (kbd "RET") 'electric-indent-just-newline)
  (add-hook 'web-mode-hook #'lsp-deferred)

  ;; todo: is this every useful in JS? everything has type `any' ...
  (progn
    (setq lsp-javascript-display-variable-type-hints t)
    (setq lsp-javascript-display-enum-member-value-hints t)
    (setq lsp-javascript-display-parameter-name-hints t)
    (setq lsp-javascript-display-inlay-hints t)
    (setq lsp-javascript-display-parameter-type-hints t)
    (setq lsp-javascript-display-property-declaration-type-hints t)
    (setq lsp-javascript-display-return-type-hints t)
    (setq lsp-javascript-implicit-project-config-check-js t)
    )

  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
)

(use-package web-server
  :ensure
  :after (web-mode)
)
