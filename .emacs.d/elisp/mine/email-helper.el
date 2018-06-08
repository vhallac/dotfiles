(require 'smtpmail)

(defun mimedown ()
  "Add html mime part to message by interpreting body as markdown"
  (interactive)
  (save-excursion
    (let ((default-process-coding-system '(utf-8 . utf-8)))
      (message-goto-body)
      (shell-command-on-region (point) (point-max) "~/bin/mimedown.py" nil t))))

(provide 'email-helper)
