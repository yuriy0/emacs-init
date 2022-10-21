;; see https://github.com/rksm/emacs-rust-config/blob/master/init.el


(use-package rustic
  :ensure

  :init
  (setq rustic-lsp-setup-p nil) ;; disables auto setup which seems to break any further configuration?

  :config
  (progn
    ;; find rust analyzer
    (setq lsp-rust-server 'rust-analyzer)
    (let ((rust-analyzer-path
           (shell-command-to-string  "rustup which --toolchain stable rust-analyzer")))
      (setq rustic-analyzer-command (list rust-analyzer-path))
      )
   )
  (add-hook 'rustic-mode-hook
    (lambda ()

      ;; indent settings
      (setq indent-tabs-mode nil)
      (setq tab-width 2)
      (setq default-tab-width 2)
      (setq rust-indent-offset 2)
    ))
)

(use-package flycheck :ensure)

(use-package lsp-mode
  :ensure
  :commands lsp

  :custom
  (lsp-idle-delay 1.25)

  ;;"lens" = count references to symbols
  (lsp-lens-enable t)

  ;;interferes with diagnostics from flycheck
  (lsp-eldoc-enable-hover nil)

  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects. 
  ;; currently have enabled most rust-isms...
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints nil)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints t)

  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'lsp-mode-hook 'flycheck-mode)
)

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode

  :bind
  ;; standard xref jump using lsp-ui instead
  (([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
   ([remap xref-find-references] . #'lsp-ui-peek-find-references)
   )

  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t)
)
