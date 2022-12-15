;; -*- lexical-binding: t; -*-

(use-package windresize
  :ensure
  :commands (windresize)
  :defer t

  :init
  (global-set-key (kbd "C-c C-w") #'windresize)

  :config

  ;;replaces these two functions which are bugged upstream
  ;;(they are called without optional prefix arguments by some keybinds!)
  (defun windresize-up-force-up (&optional n)
    "If two movable borders, move the upper border.
N is the number of lines by which moving borders."
    (interactive "P")
    (let ((i (if n (prefix-numeric-value n)
	       windresize-increment)))
      (windresize-up i t)))

  (defun windresize-down-force-up (&optional n)
    "If two movable borders, move the upper border.
N is the number of lines by which moving borders."
    (interactive)
    (let ((i (if n (prefix-numeric-value n)
	       windresize-increment)))
      (windresize-down i t)))

  ;; (with-eval-after-load 'hercules
  ;;   (hercules-def
  ;;    :show-funs 'windresize
  ;;    :hide-funs '(windresize-exit windresize-cancel-and-quit)
  ;;    :keymap 'windresize-map
  ;;    )
  ;;   )
  )


;; when splitting windows, open a different buffer in the new window
(defun split-window--new-buffer (do-split &rest do-split-args)
  (interactive)
  (let* ((buf (nth (length (window-list)) (-filter 'buffer-file-name (tab-bar-buffer-list))))
         (res (apply do-split do-split-args))
         )
    (save-selected-window
      (select-window res t)
      (switch-to-buffer buf t)
      )
    res))
(advice-add 'split-window-right :around 'split-window--new-buffer)
(advice-add 'split-window-below :around 'split-window--new-buffer)

(defalias 'other-window-inverted (with-negated-prefix-arg 'other-window))
(global-set-keys (kbd "C-c i") #'other-window-inverted)

(defun other-window-target (count &optional all-frames)
  (interactive "sTarget window: ")
  (funcall 'other-window (string-to-number count) all-frames))
(global-set-keys (kbd "C-c w") 'other-window-target)


;;;###autoload
(defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "nWindow buffer to transpose with: ")
   (let* ((wins (my-window-list))
          (this-win (nth 0 wins))
          (othr-win (nth arg wins))
          (this-buf (window-buffer this-win))
          (othr-buf (window-buffer othr-win)) )
     (set-window-buffer this-win othr-buf)
     (set-window-buffer othr-win this-buf)
     (select-window othr-win)))
(global-set-keys (kbd "C-c t") 'transpose-windows)

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
      (push next-win wins)
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

(defun update-windows-and-mode-line () 
  (update-windows) (force-mode-line-update t))

(many 1 (lambda (h) (add-hook h 'update-windows-and-mode-line))
      'post-command-hook
      'window-configuration-change-hook
      'kill-buffer-hook
      )

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

(defun read-from-minibuffer-i (with-contents prompt &optional initial-contents keymap read hist default-value inherit-input-method)
  (let* ((wins0 (window-list))
         (promptl (length prompt))
         (mb nil)
         (map (copy-keymap (or keymap minibuffer-local-map)))
         (did-quit nil)
         )
    (define-key map (kbd "C-g") (lambda() (interactive) (setq did-quit t) (exit-minibuffer)))
    (cc:thread 0
      (while (not mb)
        (setq mb (-difference (window-list) wins0)))
      (setq mb (window-buffer (car mb)))
      (fset 'on-change 
            (lambda (beg end) 
                (funcall with-contents 
                         (with-current-buffer mb 
                           (let* ((str (buffer-string)))
                             (set-text-properties 0 promptl nil str)
                             (s-chop-prefix prompt str))))))
      (with-current-buffer mb (add-hook 'before-change-functions 'on-change nil t))
      )
    (let ((res (read-from-minibuffer prompt initial-contents map read hist default-value inherit-input-method)))
      (if did-quit nil res)))
  )

