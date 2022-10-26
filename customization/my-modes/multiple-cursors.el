;; multi-cursor
(use-package multiple-cursors
  :ensure

  :bind
  (
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-." . mc/mark-next-symbol-like-this)
   ("C-," . mc/mark-previous-symbol-like-this)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   ("C-S-c C-S-c" . mc/edit-lines)
   )
  )
