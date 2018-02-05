;; jumping
(define-key global-map (kbd "C-c SPC") 'ace-jump-word-mode)
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-char-mode)

;; zapping
(global-set-key [remap zap-to-char] 'ace-jump-zap-up-to-char)
(setq ajz/zap-function 'kill-region)
