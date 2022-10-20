;; see https://github.com/rksm/emacs-rust-config/blob/master/init.el


(setq rustic-lsp-setup-p nil) ;; disables auto setup which seems to break any further configuration?
(require 'rustic)

;; find rust analyzer
(setq lsp-rust-server 'rust-analyzer)
(let ((rust-analyzer-path
       (shell-command-to-string  "rustup which --toolchain stable rust-analyzer")))
      (setq rustic-analyzer-command (list rust-analyzer-path))
)

(add-hook 'rustic-mode-hook
  (lambda ()

    ;; indent settings
    (setq indent-tabs-mode nil)
    (setq tab-width 2)
    (setq default-tab-width 2)
    (setq rust-indent-offset 2)
  )
)
