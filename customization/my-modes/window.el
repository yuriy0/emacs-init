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
      (select-window res t)
      (switch-to-buffer buf t)
      )
    res))
(advice-add 'split-window-right :around 'split-window--new-buffer)
(advice-add 'split-window-below :around 'split-window--new-buffer)

(global-set-keys (kbd "C-x i" ) (with-negated-prefix-arg 'other-window))

;;;###autoload
(defun my-window-list (&optional buf)
  "A version of `window-list' which starts with `BUF' or 
the first buffer in the buffer list if `NIL'."
  (let* 
      ((first-buf (or buf (car (-filter 
          (lambda (b) (and (get-buffer-window b) (not (transitive-bufferp b)))) 
          (buffer-list)))))
       (first-win (get-buffer-window first-buf))
       (next-win (next-window first-win))
       (wins (list first-win))
       )
    (while (not (equal first-win next-win))
      (add-to-list 'wins next-win) 
      (setq next-win (next-window (car wins))))
    (-filter (-compose 'not 'transitive-bufferp 'window-buffer) (reverse wins))
    ))

;;;###autoload
(defun update-windows-and-redisplay-mode-line (wins extra-cond)
  "Updates `last-window-list' to the given window list and
schedules an update of the mode line"
  (if (not (frame-parameter nil 'last-window-list))
      (set-frame-parameter nil 'last-window-list nil))
  (when (and extra-cond 
             (not (equal (frame-parameter nil 'last-window-list) wins)))
      (set-frame-parameter nil 'last-window-list wins)
      (run-with-idle-timer 0.05 nil 
         '(lambda() (force-mode-line-update t)))
      ))

;;;###autoload
(defun buffer-index-str (&optional buf-arg)
  "The index of `BUF-ARG' (or current buffer if `nil') in the list of visible buffers,
as a string."
  (let* 
      ((buf (or buf-arg (current-buffer)))
       (win (get-buffer-window buf))
       (wins (my-window-list))
       (ix (-find-index (apply-partially 'equal win) wins))
       )
    (if ix 
        (progn (update-windows-and-redisplay-mode-line wins (eq ix 0))
               (format "%d:" ix))
      "") ))

(setq-default mode-line-buffer-identification 
  (cons '(:eval (buffer-index-str)) mode-line-buffer-identification ) )
