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

  (defun my/web-mode-hook()
    (setq-local lsp-ui-peek-fontify 'always) ;; lsp-ui-peek breaks for web-mode files because the fontification only happens for file-visiting buffers
    )
  (add-hook 'web-mode-hook #'my/web-mode-hook)

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

  (setq web-mode-enable-auto-indentation nil)
  (setq js-indent-level 2) ;; used by json files

  ;; needed to get LSP to consider the indent size the same as which we set above
  ;; https://github.com/emacs-lsp/lsp-mode/issues/2915#issuecomment-855156802
  (with-eval-after-load 'lsp-mode
    (setf (alist-get 'web-mode lsp--formatting-indent-alist) 'web-mode-code-indent-offset))

  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
)

(use-package web-server
  :ensure
  :after (web-mode)
)
