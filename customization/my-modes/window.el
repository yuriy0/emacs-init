;; -*- lexical-binding: t; -*-

(defun window-anchored (&optional win)
  (-zip-with 'eq 
   (window-edges (frame-root-window win) nil nil t)
   (window-edges nil nil nil t)))

(setq dir-to-ix  
      '((left   . 0)
        (top    . 1) 
        (right  . 2)
        (bottom . 3) ))
(setq opp-dir
      '((left   . right )
        (top    . bottom) 
        (right  . left  )
        (bottom . top   ) ))
(setq dir-to-side 
      '((left   . right)
        (top    . above) 
        (right  . right)
        (bottom . above)))

(setq pull-window-keymap
  `(( ,(kbd "C-c <C-down>" ) . bottom )
    ( ,(kbd "C-c <C-up>"   ) . top    )
    ( ,(kbd "C-c <C-left>" ) . left   )
    ( ,(kbd "C-c <C-right>") . right  )  ))

(setq pull-window-base-mult 3)

(defun pull-window-dir (delta dir &optional win) 
  "Pulls the edges of the given window in the given
direction (`DIR-ARG') according to the following rules:

If the window is anchored (attached to the edge of its frame) in
the specified cardinal direction (up/down or left/right), pulls
the non-anchored edge in the given direction.

If the window has an adjacent window (in the specified cardinal
direction), pulls the edge of both the given window and the
adjacent window in the given direction (effectively resizing the
given window, and leaving the others unchanged if the resize
would not leave any window smaller than its minimum size).

`DELTA' determines by how much edges should be pulled. A negative
delta is treated as its absolute value. `pull-window-base-mult'
is multiplied by the given delta to determine the true delta."
  (-if-let* 
      ((dir-ix (alist-get dir dir-to-ix)) )
      (-let* 
          ((c-crd (if (memq dir '(left right)) t nil))
           (ws (window-anchored))
           (c-dir (nth dir-ix ws))
           (c-odir (nth (alist-get (alist-get dir opp-dir) dir-to-ix) ws))
           (c-val (* pull-window-base-mult (if c-dir -1 1) (abs delta)))
           (m-side (window-in-direction (alist-get dir dir-to-side) win t))
           (m-delta (* pull-window-base-mult (if (memq dir '(top left)) -1 1) (abs delta)))
           )
        (cond ((or c-odir c-dir) 
               (window-resize win c-val c-crd))
              (m-side
               (--each (list win m-side) (adjust-window-trailing-edge it m-delta c-crd)))
              (t (message "pull-window-dir(%S): don't know what to do" dir))
              ))
    (error "pull-window-dir: invalid direction: %S" dir-arg) ))

(--each pull-window-keymap (global-set-key (car it) '(lambda (delta) 
  (interactive "p")
  (-when-let* ((key (this-command-keys))
               (dir (cdr (assoc key pull-window-keymap))))
    (pull-window-dir delta dir) )) ))

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
(global-set-keys (kbd "C-x p") #'(lambda (count &optional all-frames)
  (interactive "sTarget window: ")
  (funcall 'other-window (string-to-number count) all-frames)))

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
(defun update-windows (&optional wins-def)
  "Updates `last-window-list' to the given window list. Returns
if the window list was actually updated."
  (if (not (frame-parameter nil 'last-window-list))
      (set-frame-parameter nil 'last-window-list nil))
  (let* ((wins (or wins-def (my-window-list)))
         (do-update (not (equal (frame-parameter nil 'last-window-list) wins)))
          )
    (when do-update (set-frame-parameter nil 'last-window-list wins))
    do-update))

(add-hook 'post-command-hook '(lambda () (update-windows) (force-mode-line-update t)))
(add-hook 'window-configuration-change-hook 'update-windows)

;;;###autoload
(defun buffer-index-str (&optional buf-arg)
  "The index of `BUF-ARG' (or current buffer if `nil') in the list of visible buffers,
as a string."
  (let* 
      ((buf (or buf-arg (current-buffer)))
       (win (get-buffer-window buf))
       (wins (frame-parameter nil 'last-window-list))
       (ix (-find-index (apply-partially 'equal win) wins))
       )
    (when (not wins) (message "Window list not created"))
    (if ix (format "%d:" ix) "") ))

(defun do-add-buffer-index-str-to-mode-line (k)
  (funcall k 'mode-line-buffer-identification 
     (cons '(:eval (buffer-index-str)) mode-line-buffer-identification ) ))

;; In order to get the window index in windows which already override the
;; default mode line, that need to be `set' (instead of `set-default') inside
;; the appropriate hook for the mode
(do-add-buffer-index-str-to-mode-line 'set-default)
(add-hook 'dired-after-readin-hook 
  (apply-partially 'do-add-buffer-index-str-to-mode-line 'set))

(defun string-to-nat (str) 
  (if (string-match "\\`[0-9]*[1-9][0-9]*\\'" str) (string-to-number str)))
