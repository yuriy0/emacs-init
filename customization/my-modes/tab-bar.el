;; -*- lexical-binding: t; -*-

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

;;;###autoload
(defun tab-bar-tabs-real()
  "Whereas `tab-bar-tabs' returns a special form of data for the
current the current tab, this returns the actual tab-bar
information for the current tab as well"
  (-map
   (lambda(tab)
     (if (eq (car tab) 'current-tab)
         (let ((tt (tab-bar--tab)))
           (setf (car tt) 'current-tab)
           tt
           )
       tab)
     )
   (tab-bar-tabs)))


;;;###autoload
(defun tab-bar-tab-visible-buffers (tab)
  (->> tab
       (alist-get 'ws)
       window-state-buffers
       (mapcar #'get-buffer)))

;;;###autoload
(defun tab-bar-raise-buffer-score (desired-buf tab tabindex)
  (let ((tab-visible-bufs (tab-bar-tab-visible-buffers tab))
        (tab-buf-list (alist-get 'wc-bl tab))
        (desired-buf-ix-in-tab-buf-list)
        )
    (cond
     ;; pick tabs where the desired buffer is visible
     ((memq desired-buf tab-visible-bufs)
      (if (alist-get 'is-current tab)
          ;; if its the current tab, then switch focus to the window which already displays it
           (list 0 0)

        ;; if its another tab, then switch to that tab, and then focus on the
        ;; window which already displays it
         (list 1 0)
         )
      )

     ;; otherwise prefer tabs whose local buffer-list property contains the
     ;; desired buffer; this may be multiple tabs so we score by the position
     ;; in the buffer list
     ((setq desired-buf-ix-in-tab-buf-list
            (-elem-index desired-buf tab-buf-list))
       (list 2 desired-buf-ix-in-tab-buf-list)
      )

     ;; otherwise we can't raise this buffer in a tab
     (t
      nil
      )
     )
    )
  )

;;;###autoload
(defun tab-bar-raise-buffer (desired-buf)
  "If the buffer is visible in some tab, switch to that tab and to the window containing the buffer,
or if the buffer was opened in some tab, switch to that tab and
make the buffer visible there; and return the index of the
switched-to tab in either case. Otherwise, return `nil'"
  (-let*
    ((desired-buf (get-buffer desired-buf))
     (tab-index
      (->> (tab-bar-tabs-real)
           ;; zip the tab bar index with the score for switching to the desired buffer
           (--map-indexed
            (list
             (tab-bar-raise-buffer-score desired-buf it it-index)
             it-index)
            )
           ;; remove those tabs which can't raise this buffer
           (-filter (-partial 'nth 0))
           ;; sort by score (lower is better)
           (-sort-by-key 'lexographic< (-partial #'nth 0))
           ;; take the best, if any
           car cadr
           )
      )
     )
    (when tab-index
      ;; our indices are zero based but tab-bar mode is 1 based
      (setq tab-index (1+ tab-index))
      (tab-bar-select-tab tab-index)
      (-if-let (desired-buf-window (get-buffer-window desired-buf))
          (select-window desired-buf-window)
        (switch-to-buffer desired-buf))
      tab-index
      )
    )
  )

;;;###autoload
(defun tab-bar-raise-or-switch-to-buffer (buffer)
  (or (tab-bar-raise-buffer buffer) (switch-to-buffer buffer)))


;;;###autload
(defun tab-bar-remove-buffers-from-invisible-tabs (buffers &optional silent)
  "Remove `BUFFERS' from the \"buffer list\"s of all other
  tabs, other than the currently visible one."
  (modf buffers (mapcar #'get-buffer))
  (when tab-bar-mode
    (cl-flet ((filter-buf-list
               (tab bufs)
               (--filter
                (if (member (get-buffer it) buffers)
                    (progn
                      (if (not silent) (message "Removing buffer '%s' from tab '%s'" it (alist-get 'name tab)))
                      nil
                      )
                  t)
                bufs)
               ))
      (-let [tabs (tab-bar-tabs)]
        (dolist (tab tabs)
          (when (eq (car tab) 'tab) ;; excludes the current tab
            (modf (alist-get 'wc-bl tab) (filter-buf-list tab))
            (modf (alist-get 'wc-bbl tab) (filter-buf-list tab))
            )
          )
        (set-frame-parameter nil 'tabs tabs)
        )
      )
    )
)
