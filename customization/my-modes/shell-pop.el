;; shell pop + eshell
(customize-set-variable
  'shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))) )
(global-set-keys (kbd "C-'") 'shell-pop)
(setq shell-pop-restore-window-configuration nil)


(defun shell-pop--shell-buffer-name (index)
  (if (string-match-p "*\\'" shell-pop-internal-mode-buffer)
      (replace-regexp-in-string
       "*\\'" (format "*<%d>" index) shell-pop-internal-mode-buffer)
    (format "%s<%d>" shell-pop-internal-mode-buffer index)))
