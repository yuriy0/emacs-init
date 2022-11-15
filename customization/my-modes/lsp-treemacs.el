;; -*- lexical-binding: t; -*-

(use-package lsp-treemacs
  :ensure t
  :load-path "lisp-pkg/lsp-treemacs"

  :after (treemacs lsp-mode)
  :commands (lsp-treemacs-symbols)
  :config

  (lsp-treemacs-sync-mode 1)
)
