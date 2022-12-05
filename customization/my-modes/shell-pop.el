(use-package shell-pop
  :ensure
  :bind ("C-'" . shell-pop)

  :config
  (customize-set-variable
   'shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))) )

  (setq shell-pop-restore-window-configuration nil)
  (fset 'shell-pop-split-window 'my--shell-pop-split-window)

  (add-hook 'shell-pop-in-after-hook #'my/shell-pop-in-after-hook)
  (advice-add 'shell-pop-out :override #'my/shell-pop-out)
)

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

(defun shell-pop--shell-buffer-name (index)
  (if (string-match-p "*\\'" shell-pop-internal-mode-buffer)
      (replace-regexp-in-string
       "*\\'" (format "*<%d>" index) shell-pop-internal-mode-buffer)
    (format "%s<%d>" shell-pop-internal-mode-buffer index)))

;;;###autoload
(defun my/shell-pop-in-after-hook()
   ;; make shell pop windows dedicated. this is especially important when using
   ;; find-file from the popped shell, which would replace that shell buffer
  (set-window-dedicated-p nil t)
  )

(defun my/shell-pop-out()
  (run-hooks 'shell-pop-out-hook)
  (if (shell-pop--full-p)
      (let ((window-conf (cl-first shell-pop-window-configuration))
            (marker (cl-second shell-pop-window-configuration)))
        (set-window-configuration window-conf)
        (when (marker-buffer marker)
          (goto-char marker)))
    (when (and (not (one-window-p)) (not (= shell-pop-window-height 100)))
      ;; fixes a bug with the default implemenentation of shell-pop
      ;; bury-buffer will **sometimes** delete the window, in which case
      ;; calling delete-window later does the wrong thing.
      (let ((starting-window (selected-window)))
        (bury-buffer)
        (when (eq starting-window (selected-window)) (delete-window))
        (when (and shell-pop-last-window (window-live-p shell-pop-last-window))
          (select-window shell-pop-last-window))
        )
      )
    (when shell-pop-restore-window-configuration
      (switch-to-buffer shell-pop-last-buffer))))
