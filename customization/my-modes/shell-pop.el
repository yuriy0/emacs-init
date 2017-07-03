;; shell pop + eshell
(customize-set-variable
  'shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))) )
(global-set-keys (kbd "C-'") 'shell-pop)
(setq shell-pop-restore-window-configuration nil)

(defun extremal-windows (dirs)
  "Computes the windows in the current frame farthest in the
directions specified. `DIRS' must be a non-empty list of
directions (a direction being that which which can be passed to
`window-in-direction') containing no opposite directions."
  (when (or (eq dirs nil)
            (cl-subsetp '(above below) dirs)
            (cl-subsetp '(left right) dirs)
            (not (cl-subsetp dirs '(above left right below))))
    (user-error "Invalid set of directions: %s" dirs))
  (--filter
   (every
    (lambda(d) (not (window-in-direction d it))) dirs)
   (window-list)))

(defun my--shell-pop-split-window ()
  (unless (shell-pop--full-p)
    (let ((win (car (extremal-windows '(right below)))))
      (setq shell-pop-window-position
            (if (eq (count-windows) 1) "right" "bottom"))
      (split-window
       win
       (shell-pop--calculate-window-size)
       (shell-pop--translate-position shell-pop-window-position)))))
(fset 'shell-pop-split-window 'my--shell-pop-split-window)

(defun shell-pop--shell-buffer-name (index)
  (if (string-match-p "*\\'" shell-pop-internal-mode-buffer)
      (replace-regexp-in-string
       "*\\'" (format "*<%d>" index) shell-pop-internal-mode-buffer)
    (format "%s<%d>" shell-pop-internal-mode-buffer index)))
