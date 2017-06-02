(defun window-anchored (&optional win)
  (-zip-with 'eq 
   (window-edges (frame-root-window win) nil nil t)
   (window-edges nil nil nil t)))

(defun dir-to-ix (dir) 
  (alist-get dir 
    '((left   . 0)
      (top    . 1) 
      (right  . 2)
      (bottom . 3)
     )))

(setq pull-window-base-mult 2)

(defun pull-window-dir (delta dir)
  (-if-let* 
    ( (dir-ix (dir-to-ix dir))
    )
      (-let* 
          ((c-crd (if (memq dir '(left right)) t nil))
           (c-dir (nth dir-ix (window-anchored)))
           (c-val (* pull-window-base-mult (if c-dir -1 1) (abs delta)))
           )
        (window-resize nil c-val c-crd)
        )
    (message "pull-window-dir(%S): error" dir) ))

(defun pull-window-left (delta) 
  (interactive "p") 
  (pull-window-dir delta 'left))

(defun pull-window-up (delta) 
  (interactive "p") 
  (pull-window-dir delta 'top))

(defun pull-window-right (delta) 
  (interactive "p") 
  (pull-window-dir delta 'right))

(defun pull-window-down (delta) 
  (interactive "p") 
  (pull-window-dir delta 'bottom))

(defun pull-window-up (delta) 
  (interactive "p") 
  (pull-window-dir delta 'top))

;; advanced window resizing
(global-set-keys 
  (kbd "C-c <C-down>" ) 'pull-window-down
  (kbd "C-c <C-up>"   ) 'pull-window-up
  (kbd "C-c <C-left>" ) 'pull-window-left
  (kbd "C-c <C-right>") 'pull-window-right
)

;; when splitting windows, open a different buffer in the new window
(defun split-window--new-buffer (do-split &rest do-split-args)
  (interactive)
  (let* ((buf (nth (length (window-list)) (-filter 'buffer-file-name (buffer-list))))
         (res (apply do-split do-split-args))
         )
    (save-selected-window
      (select-window res)
      (switch-to-buffer buf)
      )
    res))
(advice-add 'split-window-right :around 'split-window--new-buffer)
(advice-add 'split-window-below :around 'split-window--new-buffer)
