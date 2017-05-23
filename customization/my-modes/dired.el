;; find file with `a' in same buffer
(put 'dired-find-alternate-file 'disabled nil)

;; find parent dir with `^' in same buffer
;; https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))))
