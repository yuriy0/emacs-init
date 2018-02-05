;; general
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))
(save-place-mode 1)

;; find file with `a' in same buffer
(put 'dired-find-alternate-file 'disabled nil)

;; find parent dir with `^' in same buffer
;; https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))))

;; dired-subtree
(setq dired-subtree-use-backgrounds nil)
(many 2 (apply-partially 'define-key dired-mode-map)
   (kbd "i") #'dired-subtree-insert
   (kbd "<tab>") #'dired-subtree-toggle
   (kbd "<backtab>") #'dired-subtree-cycle
   (kbd "<RET>") #'dired-find-alternate-file
   (kbd "a") #'dired-find-file
   )
