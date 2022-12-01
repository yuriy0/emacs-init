;; -*- lexical-binding: t; -*-

(use-package helm
  :ensure
  :autoload (find-file-create-if-nonexistant)

  :bind
  (
   ("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-M-y" . helm-all-mark-rings)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list)
   ("C-x q" . helm-resume)
   ("C-x b" . helm-tab-buffers-list)
   ("C-x C-b" . helm-mini)

   :map helm-map
   ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
   ("C-i" . helm-execute-persistent-action)   ; make TAB works in terminal
   ("C-z" . helm-select-action)             ; list actions using C-z

   :map helm-buffer-map
   ("M-E" . helm-buffer-remove-from-tab-buffers)
   )

  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  (helm-adaptive-mode t)

  (run-with-timer 0.1 nil (lambda() (diminish 'helm-mode)))

  ;; handles a very strange issue in which M-x looks at the text at point
  ;; and if it looks like a URL it tries to ping that URL...
  ;; see https://github.com/emacs-helm/helm/issues/648
  (setq ffap-machine-p-known 'reject)

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

        helm-buffers-maybe-switch-to-tab t
   )

  (many 1 (apply-partially 'add-to-list 'helm-boring-buffer-regexp-list)
        "\\*magit-process:" 
        "\\*magit-diff:"
        "\\*Quail Completions*"
        "\\*Backtrace*"
        "\\*quelpa-build-checkout*"
        )

  ;; find-file - when the file doesn't exist, create it
  (advice-add 'helm-find-files :after 'find-file-create-if-nonexistant)

  ;; helm buffers command which removes the marked buffers from the current tabs buffer list
  (defun helm-buffer-remove-from-tab-buffers-fn (_candidate)
    "Removes any buffers from the frame 'buffer-list parameter which are marked helm candidates"
    (if (not tab-bar-mode) (message "Tab Bar mode is disabled")
    (-let [cands (mapcar #'buffer-name (helm-marked-candidates))]
      (filter-buffer-list-frame-parameters
       nil
       (lambda(b)
         (not (--any (member (buffer-name b) cands) cands))
         )
       )
      (message "Removed %s buffers from tab \"%s\"" (length cands) (alist-get 'name (tab-bar--tab)))
      )
    ))
  (helm-make-command-from-action helm-buffer-remove-from-tab-buffers
    "Remove buffer from tabs buffer list"
    'helm-buffer-remove-from-tab-buffers-fn)
  (add-to-list-at 'helm-type-buffer-actions 6 '("Remove buffer from tabs buffer list" . helm-buffer-remove-from-tab-buffers-fn))

  ;; completly replace helm-buffers-switch-to-buffer-or-tab with a more robust
  ;; implementation
  (advice-add 'helm-buffers-switch-to-buffer-or-tab :override #'my/helm-buffers-switch-to-buffer-or-tab)

  ;; since we replace behaviour of helm buffer switching, add a new action type which preserves the non-tabbing behaviour
  ;; i.e. just uses switch to buffer
  (add-to-list-at 'helm-type-buffer-actions 1 '("Switch to buffer(s) (in current tab&window)" . helm-buffer-switch-buffers-this-tab))

  ;; helm mark ring & global mark ring actions which raise the buffer in its current tab
  ;; TODO
)

(defvar helm-source-tab-buffers-list nil)
(defvar helm-source-lsp-workspace-buffers-list nil)

;;;###autoload
(defun helm-buffer-list-filtering-by (filterfn)
  (when filterfn
    (->> (helm-buffer-list)
         (-map #'get-buffer)
         (-filter (-partial 'funcall filterfn))
         (-map #'buffer-name)
         )
    )
  )

;;;###autoload
(defmacro helm-make-buffers-source-filtered (source-name filterfn)
  `(helm-make-source ,source-name 'helm-source-buffers
     :buffer-list
     (lambda()
       (helm-buffer-list-filtering-by
        ,filterfn
        )
       )
     )
  )

;;;###autoload
(defun helm-tab-buffers-list ()
  (interactive)

  ;; HACK we rely on helm lazy initialization of its own internals
  ;; if this is the first helm-buffers-like command you call after startup,
  ;; this variable is not yet initialized
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))

  (init-once
   helm-source-tab-buffers-list
   (helm-make-buffers-source-filtered
    "Tab Buffers"
    (when (fboundp 'tab-bar-mode)
      (-let [tabbar-bufs (frame-parameter nil 'buffer-list)]
        (lambda(b) (memq b tabbar-bufs)))
      )))

  (init-once
   helm-source-lsp-workspace-buffers-list
   (helm-make-buffers-source-filtered
    "LSP Workspace Buffers"
    (when-let ((lsp-wss (-uniq (--mapcat (lsp-workspace-folders it) (lsp-workspaces)))))
      (lambda(b)
        (when-let ((buf-visiting-fname (buffer-file-name b)))
          (--any (string-prefix-p it buf-visiting-fname) lsp-wss)
          )
        ))
     ))

  (helm :sources
        '(helm-source-tab-buffers-list
          helm-source-lsp-workspace-buffers-list
          helm-source-buffers-list
          helm-source-buffer-not-found)
        :keymap helm-buffer-map
        :truncate-lines helm-buffers-truncate-lines))

;;;###autoload
(defun find-file-create-if-nonexistant (_)
  (when-let*
      ((bfnm  (buffer-file-name))
       (notex (not (file-exists-p bfnm))))
    (save-buffer)
    ))


;;;###autoload
(defun my/helm-buffers-switch-to-buffer-or-tab (buffer)
  (if (and (fboundp 'tab-bar-mode)
           helm-buffers-maybe-switch-to-tab)
      (or (tab-bar-raise-buffer buffer) (switch-to-buffer buffer))
    (switch-to-buffer buffer)))

;;;###autoload
(defun helm-buffer-switch-buffers-this-tab (_candidate)
  (let ((buffers (helm-marked-candidates)))
    ;; switch buffers
    (let ((helm-buffers-maybe-switch-to-tab nil)) (helm-buffer-switch-buffers _candidate))

    ;; remove from the other tabs' buffer lists
    (tab-bar-remove-buffers-from-invisible-tabs buffers)
    )
  )

(use-package helm-ag
  :ensure
  :after (helm)

  :bind
  (
   ("C-x / 1" . helm-do-ag)
   ("C-x / 2" . helm-do-ag-this-file)
   ("C-x / 3" . helm-do-ag-project-root)
   ("C-x / 4" . helm-do-ag-buffers)
   )

  :config
  (setq helm-ag-base-command "ag --vimgrep --no-color")
  (setq helm-ag-fuzzy-match t)

  (set-face-attribute 'helm-grep-finish nil
                      :foreground "MediumSeaGreen"
                      :weight 'bold
                      :width 'expanded
                      )
)

(use-package helm-swoop
  :ensure
  :commands
  (helm-swoop helm-swoop-back-to-last-point helm-multi-swoop helm-multi-swoop-all helm-swoop-from-isearch
  ;; since we primarily access swoop via isearch we should load it when isearch first starts
  ;; for some reason loading via the `iswoop' command in minibuffer doesn't work?
   isearch-forward isearch-backward
  )

  :config

  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
)

  ;; misc.
(use-package ac-helm :ensure
  :after (helm))
(use-package helm-descbinds :ensure
  :after (helm)
  :config
  (helm-descbinds-mode)
)
(use-package helm-tramp :ensure
  :after (helm))
(use-package helm-lsp
  :after (helm lsp-mode))
