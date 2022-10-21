;; multi-cursor
(use-package multiple-cursors
  :ensure
  :config
  (global-set-keys
   (kbd "C->") 'mc/mark-next-like-this
   (kbd "C-<") 'mc/mark-previous-like-this
   (kbd "C-.") 'mc/mark-next-symbol-like-this
   (kbd "C-,") 'mc/mark-previous-symbol-like-this
   (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click
   (kbd "C-S-c C-S-c") 'mc/edit-lines)
)
