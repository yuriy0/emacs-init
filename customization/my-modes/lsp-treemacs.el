;; -*- lexical-binding: t; -*-

(use-package lsp-treemacs
  :load-path "lisp-pkg/lsp-treemacs"

  :after (treemacs lsp-mode)
  :commands (lsp-treemacs-symbols)
  :config

  (setq lsp-treemacs-error-list-severity 5
        )

  (lsp-treemacs-sync-mode 1)
)
