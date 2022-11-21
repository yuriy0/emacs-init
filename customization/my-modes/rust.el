;; see https://github.com/rksm/emacs-rust-config/blob/master/init.el
(use-package rustic
  :ensure
  :commands (rustic-mode)
  :autoload (my/rustic-make-process-advice)

  :init
  (setq rustic-lsp-setup-p nil) ; setup ourselfs

  :config
  ;; find rust analyzer
  (progn
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

      (yas-minor-mode)

      ;; required for lsp in rustic mode
      ;; rustic-mode would do this for us if `rustic-lsp-setup-p' was `t' except it doesn't use deferred lsp
      (rustic-lsp-mode-setup)
      (lsp-deferred)
    ))

  (advice-add 'rustic-make-process :around 'my/rustic-make-process-advice)
  ;; faces for compilation output (inherit from defaults)
  :custom-face
   (rustic-compilation-column ((t (:inherit compilation-column-number))))
   (rustic-compilation-line ((t (:inherit compilation-line-number))))
   (rustic-compilation-error ((t (:inherit compilation-error))))
   (rustic-compilation-warning ((t (:inherit compilation-warning))))
)

;;;###autoload
(defun my/rustic-make-process-advice (fn &rest args)
  (let (
        ;; setup environment variables for rust related processes before
        ;; spawning those processes.
        (process-environment
         (nconc (list "CARGO_TERM_COLOR=always") process-environment))

        ;; workaround bug fix for "rustic-make-process", which treats this as a `symbolp'
        ;; but is actually a `listp'
        (default-process-coding-system 'utf-8-unix))
    (apply fn args)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom

  ;; what to use when checking on-save. "check" or "clippy"
  (lsp-rust-analyzer-cargo-watch-command "check")

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

  ;; disables some types of diagnostics from rust-analyzer
  ;; see https://rust-analyzer.github.io/manual.html#diagnostics
  ;; (lsp-rust-analyzer-diagnostics-disabled [])
)
