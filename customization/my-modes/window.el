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
(defun opp-dir (dir) 
  (alist-get dir 
    '((left   . right )
      (top    . bottom) 
      (right  . left  )
      (bottom . top   )
     )))

(setq pull-window-base-mult 5)

(defun pull-window-dir (delta dir)
  (-if-let* 
    ( (dir-ix (dir-to-ix dir))
    )
      (-let* 
          ((c-crd (if (memq dir '(left right)) t nil))
           (ws (window-anchored))
           (c-dir (nth dir-ix ws))
           (c-odir (nth (dir-to-ix (opp-dir dir)) ws))
           (c-val (* pull-window-base-mult (if c-dir -1 1) (abs delta)))
           )
        (if (or c-odir c-dir)
            (window-resize nil c-val c-crd)
          (message "pull-window-dir: don't know what to do"))
        )
    (message "pull-window-dir(%S): error" dir) ))

(setq pull-window-keymap
  `(( ,(kbd "C-c <C-down>" ) . bottom )
    ( ,(kbd "C-c <C-up>"   ) . top    )
    ( ,(kbd "C-c <C-left>" ) . left   )
    ( ,(kbd "C-c <C-right>") . right  )  ))

(--each pull-window-keymap (global-set-key (car it) 'pull-window-dir-i))

;; relies on pull-window-keymap to determine the direction 
(defun pull-window-dir-i (delta)
  (interactive "p")
  (-when-let* ((key (this-command-keys))
               (dir (cdr (assoc key pull-window-keymap))))
    (pull-window-dir delta dir) ))


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

(defun other-window-rev (count &optional all-frames)
  (interactive "p") 
  (other-window (- count) all-frames))
(global-set-keys (kbd "C-x i" ) 'other-window-rev)


