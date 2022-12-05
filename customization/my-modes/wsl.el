(when (system-type-wsl-p)

  ;; makes copying to the linux clipboard in WSL also copy to the windows clipboard
  (defun my/gui-select-text(text)
    (shell-command-with-stdin "clip.exe" text)
    nil)
  (advice-add 'gui-select-text :after #'my/gui-select-text)

)
