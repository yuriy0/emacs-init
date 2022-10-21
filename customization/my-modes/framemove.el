;; {frame+wind}move
(use-package framemove
  :config
  (global-set-keys
   (kbd "C-x C-<left>") 'windmove-left
   (kbd "C-x C-<right>") 'windmove-right
   (kbd "C-x C-<up>") 'windmove-up
   (kbd "C-x C-<down>") 'windmove-down)
  (setq framemove-hook-into-windmove t)
  (framemove-default-keybindings 'super)
)
