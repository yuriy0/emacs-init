;; -*- lexical-binding: t; -*-

(use-package lsp-treemacs
  :load-path "lisp-pkg/lsp-treemacs"

  :after (treemacs lsp-mode)
  :commands (lsp-treemacs-symbols)
  :bind
  (:map lsp-treemacs-error-list-mode-map

        ;; rebind lsp-treemacs-cycle-severity because it conflicts with existing `=` binding
        ;; in treemacs basic functionality
        ("=" . nil)
        ("`" . lsp-treemacs-cycle-severity)
        )

  :config

  (setq lsp-treemacs-error-list-severity 5)

  (lsp-treemacs-sync-mode 1)
)

