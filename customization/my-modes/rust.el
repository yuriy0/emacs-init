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

      ;; don't need in rust mode, the rust analyzer gives very good results
      ;; (setq-local company-backends
      ;;    (remove 'company-files (
      ;;     remove 'company-etags (
      ;;     remove 'company-dabbrev company-backends)))
      ;;  )
    ))

  :bind (:map
         rustic-mode-map
         ("C-c C-c a" . lsp-execute-code-action))
)

(use-package flycheck :ensure)

;;;###autoload
(defun company-abort-then-complete ()
  (interactive)
  (call-interactively 'company-complete)
)

(use-package company
  :ensure

  :custom

  ;; idle completion
  (company-idle-delay 0)
  (company-tooltip-idle-delay 9999)

  ;; completion starts with any # of characters
  (company-minimum-prefix-length 1)

  (company-frontends
   '(
     company-pseudo-tooltip-unless-just-one-frontend
     company-preview-if-just-one-frontend))

  :bind

  ;; explicit completion
  ;; ("M-/" . 'yuriy/company-complete)

  (:map company-active-map
        ;; esc while completion popup is active closes it
        ("ESC" . 'company-abort)

        ;; ("TAB" . company-complete-selection)
        ("<tab>" . company-complete-selection)
        )

  :init
  (add-hook 'company-completion-started-hook
            (lambda (is-manual)
              (if is-manual
                  (setq company-tooltip-idle-delay 0)
                )
              )
            )
  (add-hook 'company-after-completion-hook
            (lambda (s)
              (setq company-tooltip-idle-delay 99999)
              )
            )
)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-/") #'company-complete))

(use-package lsp-mode
  :ensure
  :commands lsp

  :custom
  (lsp-idle-delay 0.65)

  ;;"lens" = count references to symbols
  (lsp-lens-enable t)

  ;;interferes with diagnostics from flycheck
  (lsp-eldoc-enable-hover nil)

  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints nil)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints t)

  ;; after changes clear diagnostics since they will usually refer to invalid line/column numbers
  (lsp-diagnostic-clean-after-change t)

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
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics t)
)
