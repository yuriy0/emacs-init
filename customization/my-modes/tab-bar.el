(use-package tab-bar
  :ensure
  :demand t
  :commands (tab-bar-new-tab
             tab-bar-switch-to-next-tab
             tab-bar-switch-to-prev-tab)

  :config

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

  :custom-face
  (tab-bar-tab ((t (:box (:line-width (1 . 1) :color "#cce8ff" :style released-button) :background "#e5f3ff" :inherit tab-bar))))
  (tab-bar-tab-inactive ((t (:box (:line-width (1 . 1) :style released-button) :background "grey75" :inherit tab-bar-tab))))
)
