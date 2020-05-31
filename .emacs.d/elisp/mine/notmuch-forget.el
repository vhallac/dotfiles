(defvar blacklist-file "~/.emacs.d/unused-emails.txt"
  "Path to file that contains a list of emails to forget.")

(defun trim (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun read-blacklist-file ()
  (let ((lines nil))
    (save-excursion
      (with-temp-buffer
        (insert-file-contents (expand-file-name blacklist-file))
        (goto-char (point-max))
        (while (> (point) (point-min))
          (let* ((eol (point))
                 (bol (progn (forward-line -1) (point)))
                 (line (buffer-substring bol eol)))
            (setq lines (cons (trim line) lines))))))
      lines))

(defun filter--notmuch-address-options (orig-fun original &rest args)
  (let ((orig-list (apply orig-fun (list original)))
        (blacklisted (regexp-opt (read-blacklist-file))))
    (cl-delete-if (lambda (email)
                    ;; This is a substring match: using regexp for simplicity
                    (string-match-p blacklisted email))
                  orig-list)))

(defun notmuch-forget-install ()
  (require 'notmuch)
  (advice-add #'notmuch-address-options :around #'filter--notmuch-address-options))


(provide 'notmuch-forget)
