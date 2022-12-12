;; -*- lexical-binding: t; -*-

(use-package tab-bar
  :ensure
  :demand t
  :commands (tab-bar-new-tab
             tab-bar-switch-to-next-tab
             tab-bar-switch-to-prev-tab)

  :bind

  ;; bindings for tab switching left/right and tab re-ordering left/right
  ;; in both tab prefix keymap and repeat-mode maps
  (:map tab-bar-switch-repeat-map
        ("<left>" . tab-previous)
        ("<right>" . tab-next))
  (:map tab-bar-move-repeat-map
        ("C-<left>" . tab-bar-move-tab-backward)
        ("C-<right>" . tab-move))
  (:map tab-prefix-map
        ("<left>" . tab-previous)
        ("<right>" . tab-next)
        ("C-<left>" . tab-bar-move-tab-backward)
        ("C-<right>" . tab-move))

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
   tab-bar-new-tab-to 'rightmost ; new tabs go to the end of the tab list
   )

  ;; use tab bar mode
  (tab-bar-mode +1)
  (tab-bar-history-mode +1)

  ;; custom behaviour for display-buffer-in-tab
  (advice-add 'display-buffer-in-tab :around #'my/display-buffer-in-tab)

  ;; enable repeat mode in tab-bar mode
  (add-hook 'tab-bar-mode-hook #'repeat-mode)

  ;; custom behaviour for frameset-save (work in progress, currently broken?)
  ;; (advice-add 'frameset-filter-tabs :around #'my/frameset-filter-tabs)

  :custom-face
  (tab-bar-tab ((t (:box (:line-width (1 . 1) :color "#cce8ff" :style released-button) :background "#e5f3ff" :inherit tab-bar))))
  (tab-bar-tab-inactive ((t (:box (:line-width (1 . 1) :style released-button) :background "grey75" :inherit tab-bar-tab))))
)

;;;###autoload
(defun tab-bar-tabs-real(&optional frame)
  "Whereas `tab-bar-tabs' returns a special form of data for the
current the current tab, this returns the actual tab-bar
information for the current tab as well"
  (-map
   (lambda(tab)
     (if (eq (car tab) 'current-tab)
         (let ((tt (tab-bar--tab frame)))
           (setf (car tt) 'current-tab)
           tt
           )
       tab)
     )
   (tab-bar-tabs frame)))


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
(defun tab-bar-raise-buffer (desired-buf &optional frame)
  "If the buffer is visible in some tab, switch to that tab and to the window containing the buffer,
or if the buffer was opened in some tab, switch to that tab and
make the buffer visible there; and return the index of the
switched-to tab in either case. Otherwise, return `nil'"
  (-let*
    ((desired-buf (get-buffer desired-buf))
     (tab-index
      (->> (tab-bar-tabs-real frame)
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
      (-if-let (desired-buf-window (get-buffer-window desired-buf frame))
          (select-window desired-buf-window)
        (switch-to-buffer desired-buf))
      tab-index
      )
    )
  )

;;;###autoload
(defun my/display-buffer-in-tab (basefn buffer alist)
  (-if-let (raise-mode (alist-get 'raise alist))
      (or
       ;; try to raise the buffer in an existing tab if so configured by the ALIST properties
       (tab-bar-raise-buffer
        buffer
        (if (framep raise-mode) raise-mode nil))

       ;; .. otherwise apply default behaviour
       (funcall basefn buffer alist))
    (funcall basefn buffer alist)
  ))

;;;###autoload
(defun tab-bar-raise-or-switch-to-buffer (buffer)
  (or (tab-bar-raise-buffer buffer) (switch-to-buffer buffer)))


;;;###autoload
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

;;;###autoload
(defun tab-bar-buffer-list(&optional frame-or-tab)
  "Returns the buffer list corresponding to the given frame or tab.

The returned buffer list depends on FRAME-OR-TAB:
- if a frame, the buffer list of the current tab in that frame.
- if a tab (as produced by tab-bar-tabs), the buffer list of that tab.
- if nil, the buffer list of the current tab in the current frame."
  (pcase-exhaustive frame-or-tab
    ((pred framep)
     (frame-parameter frame-or-tab 'buffer-list))

    ((or `(current-tab . ,_) `nil)
     (frame-parameter nil 'buffer-list))

    (`(tab . ,the-tab)
     (alist-get 'wc-bl the-tab))))

;;;###autoload
(defun my/tab-buffer-to-name(buf)
  (cond
   ((bufferp buf) (when (buffer-file-name buf) (buffer-name buf)))
   ((stringp buf) buf)
   (t nil)))

;;;###autoload
(defun my/tab-name-to-buffer(buf-name)
  (cond
   ((stringp buf-name) (get-buffer buf-name))
   ((bufferp buf-name buf-name))
   (t nil)))

;;;###autoload
(defun my/frameset-filter-tabs (base-fn current _filtered _parameters saving)
  (-let ((current (funcall base-fn current _filtered _parameters saving))
         (modify-tab-fn (if saving #'my/tab-buffer-to-name #'my/tab-name-to-buffer))
         )

    ;; tab frameset data looks like `(tabs . ((tab ...) (tab ...)))'
    (pcase current
        (`(tabs . ,tabs) ;; = tabs = `((tab ...) (tab ...))'
         (cons 'tabs
         (--map ;; it = `(tab ... (wc-bl . <x>) ...)'
          (progn
            (setq it (copy-sequence it)) ;; Caution: these cons are SHARED with the live `tab-bar' data!
            (modf-v (alist-get 'wc-bl it) tab-bl ;; tab-bl = `<x> (listp)'
                    ;; (message "tab-bl(%s) = %s" (alist-get 'name it) tab-bl)
                    (-map modify-tab-fn tab-bl))
            it
           )
           tabs
           ))
         )
      (t current)
      )
    )
)
