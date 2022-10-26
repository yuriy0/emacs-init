(use-package ace-jump-mode
  :ensure

  :bind
  (
  ("C-c SPC" . ace-jump-char-mode)
  ("C-c C-SPC" . ace-jump-word-mode)
  )
)

(use-package ace-jump-zap
  :ensure
  :requires (ace-jump-mode)

  :bind
  ([remap zap-to-char] . ace-jump-zap-up-to-char)

  :config
  (setq ajz/zap-function 'kill-region)
)
