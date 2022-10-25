;;;###autoload
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device. Prevent issues with
   the Windows null device (NUL) when using cygwin find with rgrep."
  (let ((null-device "/dev/null"))
        ad-do-it))
(ad-activate 'grep-compute-defaults)



(defun my/browse-url-default-windows-browser (url &optional _new-window)
  "Replaces `browse-url-default-windows-browser' which won't work due to cygwin hackery making
this emacs think its running in a linux-like environment. Specifically, handles the 'file:///' syntax"
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (shell-command (concat "powershell.exe 'start " url "'"))
)
(advice-add 'browse-url-default-windows-browser :override #'my/browse-url-default-windows-browser)





