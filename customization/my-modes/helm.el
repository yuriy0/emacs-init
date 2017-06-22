;; helm 
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode t)
(helm-adaptive-mode t)

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
      helm-split-window-in-side-p t           ; prevent helm from temporarily hiding other
                                              ; buffers
      helm-split-window-default-side 'below
      helm-apropos-fuzzy-match t
      helm-default-external-file-browser 
        "explorer.exe"
      helm-buffer-max-length nil            ) ; don't truncate buffer names 

(many 1 (apply-partially 'add-to-list 'helm-boring-buffer-regexp-list)
      "\\*magit-process:" 
      "\\*magit-diff:"
      "\\*Quail Completions*"
      "\\*Backtrace*"
      )

;; helm-ag
(require 'helm-ag)
(global-set-keys
 (kbd "C-x / 1") 'helm-do-ag
 (kbd "C-x / 2") 'helm-do-ag-this-file
 (kbd "C-x / 3") 'helm-do-ag-project-root
 (kbd "C-x / 4") 'helm-do-ag-buffers)

(setq helm-ag-fuzzy-match t)

;; misc. 
(helm-descbinds-mode)
