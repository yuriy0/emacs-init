;; see https://github.com/rksm/emacs-rust-config/blob/master/init.el
(use-package rustic
  :ensure

  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; find rust analyzer
  (progn
    (setq lsp-rust-server 'rust-analyzer)
    (let ((rust-analyzer-path
           (shell-command-to-string  "rustup which --toolchain stable rust-analyzer")))
      (setq rustic-analyzer-command (list rust-analyzer-path))
      )
   )

  ;; indent settings
  (add-hook 'rustic-mode-hook
    (lambda ()
      (setq indent-tabs-mode nil)
      (setq tab-width 2)
      (setq default-tab-width 2)
      (setq rust-indent-offset 2)
    ))

  ;; faces for compilation output (inherit from defaults)
  (custom-set-faces
   '(rustic-compilation-column ((t (:inherit compilation-column-number))))
   '(rustic-compilation-line ((t (:inherit compilation-line-number))))
   '(rustic-compilation-error-face ((t (:inherit compilation-error))))
   '(rustic-compilation-warning-face ((t (:inherit compilation-warning))))
   )
)

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom

  ;;"lens" = count references to symbols
  (lsp-lens-enable t)

  ;; full doc strings are very long and get broken when put into the minibuffer
  ;; the "short" version shows types for some subexpressions (but occasionally shows
  ;; nothing useful?)
  (lsp-eldoc-render-all nil)

  ;; what to use when checking on-save. "check" or "clippy"
  (lsp-rust-analyzer-cargo-watch-command "check")

  (lsp-idle-delay 0.6)

  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "never")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints "never")

  ;; after changes clear diagnostics since they will usually refer to invalid line/column numbers
  (lsp-diagnostic-clean-after-change t)

  ;; disables some types of diagnostics from rust-analyzer
  ;; see https://rust-analyzer.github.io/manual.html#diagnostics
  ;; (lsp-rust-analyzer-diagnostics-disabled [])

  :config

  ;; improves lsp-mode performance
  (setq read-process-output-max (expt 2 16))
  (setq gc-cons-threshold (* 3 (expt 10 8)))

  (add-hook 'lsp-mode-hook 'lsp-ui-mode))


(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode

  :bind
  (:map lsp-ui-mode-map
        ("C-c C-c s" . #'toggle-lsp-ui-sideline-show-hover)
        ("C-c C-c t" . #'lsp-ui-doc-mode)
        ("C-c C-c >" . #'lsp-describe-thing-at-point)
        ("M-j" . #'lsp-ui-imenu)
        ("C-c C-c a" . #'lsp-execute-code-action)

        ;; standard xref jump using lsp-ui instead
        (([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . #'lsp-ui-peek-find-references)
         )
        )

  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics t)
)

(defun toggle-lsp-ui-sideline-show-hover ()
  (interactive)
  (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
)

(use-package flycheck :ensure)

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'rustic-mode-hook 'yas-minor-mode)
  )

(use-package company
  :ensure

  :custom

  ;; idle completion
  (company-idle-delay (lambda() (if (company-in-string-or-comment) nil 0.0)))

  ;; completion starts with any # of characters
  (company-minimum-prefix-length 1)

  ;; put completions from recent buffers at the top
  (company-transformers '(company-sort-by-occurrence))

  (company-frontends
   '(
     company-pseudo-tooltip-unless-just-one-frontend
     company-preview-if-just-one-frontend))

  :bind

  ;; explicit completion
  ("M-/" . #'company-complete)

  (:map company-active-map
        ;; esc while completion popup is active closes it
        ("ESC" . 'company-abort)

        ;; ("TAB" . company-complete-selection)
        ("<tab>" . company-complete-selection)
        )
)
