;; -*- lexical-binding: t; -*-

(use-package undo-tree
  :ensure t
  :defer 5

  :commands (undo-tree-visualize undo-tree-undo undo-tree-redo)

  :bind
  (:map undo-tree-map
        ;; disable some keybinds
        ("C-/" . nil)
        ("C-_" . nil)
        ("C-?" . nil)
        ([remap undo] . nil)
        ([remap undo-only] . nil)
        ([remap redo] . nil)

        ;; custom keybinds
        ("C-z" . undo-tree-undo)
        ("C-M-z" . undo-tree-redo)
        )

  (:map undo-tree-visualizer-mode-map
        ("<return>" . undo-tree-visualizer-quit))

  :custom
  (undo-tree-mode-lighter "")
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "/undo-tree-history"))))

  :config
  (global-undo-tree-mode)

  (defun my/display-undo-vis-buffer-filter(buf ac)
    (let*
        ((nm (if (stringp buf) buf (buffer-name buf)))
         (q (string-match (concat ".*" (regexp-quote "*undo-tree*") ".*") nm))
         )
      q)
    )

  (defun my/display-undo-vis-buffer(buf ac)
    (display-buffer-in-side-window
     buf
     `((side . right)))
    )

  ;; draw the undo/redo visualization buffer on the right side
  (add-to-list
   'display-buffer-alist
   '(my/display-undo-vis-buffer-filter
     my/display-undo-vis-buffer))
)
