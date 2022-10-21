(use-package ace-jump-mode
  :ensure
  :config

  ;; jumping
  (define-key global-map (kbd "C-c SPC") 'ace-jump-char-mode)
  (define-key global-map (kbd "C-c C-SPC") 'ace-jump-word-mode)
)

(use-package ace-jump-zap
  :ensure
  :requires (ace-jump-mode)
  :config

  ;; zapping
  (global-set-key [remap zap-to-char] 'ace-jump-zap-up-to-char)
  (setq ajz/zap-function 'kill-region)
)
