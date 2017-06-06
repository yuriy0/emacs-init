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
(setq wg-mode-line-display-on t)          ; Default: (not (featurep 'powerline))
(setq wg-flag-modified t)                 ; Display modified flags as well
(setq wg-mode-line-decor-left-brace "["
      wg-mode-line-decor-right-brace "]"  ; how to surround it
      wg-mode-line-decor-divider ":")

(workgroups-mode 1)   ; put this one at the bottom of .emacs
