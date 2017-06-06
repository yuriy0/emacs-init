(require 'workgroups2)

(setq wg-session-load-on-start t)

;; Change prefix key (before activating WG)
(setq wg-prefix-key (kbd "C-c z"))

;; Change workgroups session file
(setq wg-session-file "~/.emacs.d/.emacs_workgroups")

(global-set-keys
 (kbd "M-s r") 'wg-reload-session
 (kbd "M-s s") 'wg-save-session
 (kbd "M-s t") 'wg-switch-to-workgroup
 (kbd "M-s p") 'wg-switch-to-previous-workgroup)

;; What to do on Emacs exit / workgroups-mode exit?
(setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
(setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil

;; Mode Line changes
;; Display workgroups in Mode Line?
(setq wg-mode-line-display-on t           ; Default: (not (featurep 'powerline))
      wg-flag-modified t                  ; Display modified flags as well
      wg-mode-line-decor-left-brace "["
      wg-mode-line-decor-right-brace "]"  ; how to surround it
      wg-mode-line-decor-divider ":"
      wg-buffer-auto-association 'strong
      wg-mess-with-buffer-list t
      wg-remember-frame-for-each-wg t
      wg-restore-remote-buffers nil)

(defun wg-open-default (frame) 
  (unless (wg-current-workgroup t frame) 
      (let* (old-frame (selected-frame))
        (message "wg-open-default: switching")
        (select-frame-set-input-focus frame)
        (wg-switch-to-workgroup-at-index 0) 
        (wg-restore-frames)
        (select-frame-set-input-focus old-frame)
        )
      )
  )
(add-hook 'after-make-frame-functions 'wg-open-default)
;; (add-hook 'focus-in-hook '(lambda () (wg-open-default (selected-frame))))

(workgroups-mode 1)

