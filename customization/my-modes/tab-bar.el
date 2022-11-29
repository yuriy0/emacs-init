(use-package tab-bar
  :ensure
  :demand t
  :commands (tab-bar-new-tab
             tab-bar-switch-to-next-tab
             tab-bar-switch-to-prev-tab)

  :config

  (defun filter-buffer-list-frame-parameters (frame filter &optional filter-burried)
    (set-frame-parameter nil
                         'buffer-list
                         (-filter (-partial #'funcall filter) (frame-parameter nil 'buffer-list)))
    (set-frame-parameter nil
                         'buried-buffer-list
                         (-filter (-partial #'funcall (or filter-burried filter)) (frame-parameter nil 'buried-buffer-list))))

  (defvar tab-bar-default-shared-buffer-names
    nil
    ;; '("*scratch*" "*Messages*")
    "List of buffer names kept by `my-tab-bar-create'.")

  (defun my/tab-bar-new-tab-after ()
    (when tab-bar-new-tab-choice ;; this variable is set to nil when explicitly cloning an existing tab
      (let ((visible-buffers (helm-buffers-get-visible-buffers)))
        (filter-buffer-list-frame-parameters
         nil
         (lambda (buffer)
           ;; keep buffers which are in the predefined buffer list name;
           ;; and also those which are visible in the new frame configuration (which is
           ;; usually going to be a single window)
           (-let [bufname (buffer-name buffer)]
             (or
              (member buffer visible-buffers)
              (member bufname tab-bar-default-shared-buffer-names)))))
        )
      ))

  ;; modifies new state after creating a tab
  (advice-add 'tab-bar-new-tab-to :after #'my/tab-bar-new-tab-after)

  (setq
   tab-bar-tab-hints t  ; index displayed beside tab name
   )

  ;; use tab bar mode
  (tab-bar-mode +1)
  (tab-bar-history-mode +1)
)
