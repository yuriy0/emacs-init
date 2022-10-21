;; visual regexp
(use-package visual-regexp
  :ensure
 
  :config 
  (define-key global-map (kbd "C-c r") 'vr/replace)
  (define-key global-map (kbd "C-c q r") 'vr/query-replace)
)
