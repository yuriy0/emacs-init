(use-package helm
  :ensure

  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  (helm-adaptive-mode t)



  ;; handles a very strange issue in which M-x looks at the text at point
  ;; and if it looks like a URL it tries to ping that URL...
  ;; see https://github.com/emacs-helm/helm/issues/648
  (setq ffap-machine-p-known 'reject)

  (many 2 (apply-partially 'define-key helm-map)
        (kbd "<tab>") 'helm-execute-persistent-action ; rebind tab to run persistent action
        (kbd "C-i") 'helm-execute-persistent-action   ; make TAB works in terminal
        (kbd "C-z")  'helm-select-action)             ; list actions using C-z

  (global-set-keys
   (kbd "M-x") 'helm-M-x
   (kbd "M-y") 'helm-show-kill-ring
   (kbd "C-M-y") 'helm-all-mark-rings
   (kbd "C-x C-f") 'helm-find-files
   (kbd "C-x b") 'helm-buffers-list
   (kbd "C-x q") 'helm-resume)

  (setq helm-M-x-fuzzy-match t                  ; optional fuzzy matching for helm-M-x
        helm-ff-newfile-prompt-p nil            ; don't ask to create new file
        helm-display-header-line nil            ; no helm header 
        helm-split-window-in-side-p t           ; prevent helm from temporarily hiding other buffers
        helm-split-window-default-side 'below
        helm-apropos-fuzzy-match t
        helm-default-external-file-browser  "explorer.exe"
        helm-buffer-max-length nil             ; don't truncate buffer names 
        helm-M-x-always-save-history t         ; save command to history even if it produces an error
        history-delete-duplicates t            ; dont put duplicate commands in the command history
   )

  (many 1 (apply-partially 'add-to-list 'helm-boring-buffer-regexp-list)
        "\\*magit-process:" 
        "\\*magit-diff:"
        "\\*Quail Completions*"
        "\\*Backtrace*"
        )

  ;; find-file - when the file doesn't exist, create it
  (defun find-file-create-if-nonexistant (_)
    (when (not (file-exists-p (buffer-file-name)))
      (save-buffer) ) )
  (advice-add 'helm-find-files :after 'find-file-create-if-nonexistant)
)

(use-package helm-ag
  :ensure
  :requires (helm)

  :config
  ;; helm-ag
  (global-set-keys
   (kbd "C-x / 1") 'helm-do-ag
   (kbd "C-x / 2") 'helm-do-ag-this-file
   (kbd "C-x / 3") 'helm-do-ag-project-root
   (kbd "C-x / 4") 'helm-do-ag-buffers)

  (setq helm-ag-base-command "ag --vimgrep --no-color")
  (setq helm-ag-fuzzy-match t)

  (set-face-attribute 'helm-grep-finish nil
                      :foreground "MediumSeaGreen"
                      :weight 'bold
                      :width 'expanded
                      )
)

  ;; misc.
(use-package ac-helm :ensure :requires (helm))
(use-package helm-descbinds :ensure :requires (helm)
  :config
  (helm-descbinds-mode)
)
(use-package helm-tramp :ensure :requires (helm))
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
