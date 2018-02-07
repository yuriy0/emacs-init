;; jumping
(define-key global-map (kbd "C-c SPC") 'ace-jump-char-mode)
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-word-mode)

;; zapping
(global-set-key [remap zap-to-char] 'ace-jump-zap-up-to-char)
(setq ajz/zap-function 'kill-region)
