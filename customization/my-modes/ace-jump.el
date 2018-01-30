;; jumping
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; zapping
(global-set-key [remap zap-to-char] 'ace-jump-zap-up-to-char)
(setq ajz/zap-function 'kill-region)
