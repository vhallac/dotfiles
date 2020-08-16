(require 'org)
(require 'org-clock)

(defvar my/timeclock-file "~/org/timelog.ldg")
(defvar my/timeclock-log-unbillable "Unpaid")
(defvar my/timeclock-account-property "BILLABLE")
(defvar my/timeclock-project-property "PROJECT")
(defvar my/timeclock-description-property "ID")

(defmacro my/timeclock-with-timeclock-file (&rest body)
  "Act on the timeclock file, and then save the result"
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (find-file my/timeclock-file)
     (unwind-protect
         (save-restriction
           (widen)
           (save-excursion
             (progn ,@body)))
       (save-buffer))))

(defun my/timeclock-account ()
  (let ((account (org-entry-get nil my/timeclock-account-property t)))
    (or (when account (concat account ":Billable"))
        my/timeclock-log-unbillable)))

(defun my/timeclock-description ()
  (let* ((description-prop (org-entry-get nil my/timeclock-description-property t))
         (description (or description-prop (downcase (org-get-heading t t t t))))
         (project (org-entry-get nil my/timeclock-project-property t)))
    (when description
      (concat (when project (concat project ": "))
              description))))

(defun my/timeclock-clock-in ()
  (let ((account (my/timeclock-account))
        (description (my/timeclock-description)))
    (when (and account description)
      (my/timeclock-maybe-clock-out)
      (my/timeclock-with-timeclock-file
        (let ((time (format-time-string "%Y-%m-%d %H:%M:%S" org-clock-start-time)))
          (goto-char (point-max))
          (insert (concat "i " time " " account "  " description "\n")))))))

(defun my/timeclock-maybe-clock-out ()
  (my/timeclock-with-timeclock-file
    (let ((time (format-time-string "%Y-%m-%d %H:%M:%S" org-clock-out-time)))
      (goto-char (point-max))
      (skip-chars-backward "\n[:space:]")
      (goto-char (point-at-bol))
      (when (looking-at "i")
        (goto-char (point-max))
        (insert (concat "o " time))))))

(provide 'ledger-timeclock)
