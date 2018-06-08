(eval-when-compile
  (require 'cl))


; Emacs compatibility functions
(unless (fboundp 'buffer-tag-table-files)
    (defun buffer-tag-table-files ()
      (save-excursion
	(if (visit-tags-table-buffer)
	    (let ((tag-path (file-name-directory (buffer-file-name))))
	      (mapc '(lambda (file) (convert-standard-filename
				     (concat tag-path file)))
		    (tags-table-files)))
	  nil))))
(unless (fboundp 'buffer-tag-table-list)
    (defun buffer-tag-table-list ()
      (save-excursion
	(if (visit-tags-table-buffer)
	    (buffer-file-name)))))

(defun vh/c-insert-include-prev-line (include-str offset-from-end)
  "Add INCLUDE-STR to the previous line, and leave cursor OFFSET-FROM-END
characters off from the end. The cursor position is where the header name goes.

TODO: Just use yasnippet. Not that useful anymore."
       (beginning-of-line)
       (insert "#include <.h>\n")
       (forward-line -1)
       (end-of-line)
       (backward-char 3))

(defun vh/c-add-header-file-protection (&optional c++-mode-p)
  "Add the statements that protect a header file against multiple inclusion.
when C++-MODE is set, the header file is assumed to be a C++ header, and no
extern \"C\" statements are added."
  (let ((nm (subst-char-in-string ?. ?_ (concat "INC_" (upcase (buffer-name))))))
	 (save-excursion
	   (goto-char (point-min))
	   (insert (concat (concat "#ifndef " nm) "\n"))
	   (insert (concat (concat "#define " nm) "\n\n"))
       (if (not c++-mode-p)
           (insert "#ifdef __cplusplus\nextern \"C\" {\n#endif /* __cplusplus */\n"))
	   (goto-char (point-max))
       (newline)
       (if c++-mode-p
           (insert (concat (concat "\n#endif // " nm) "\n"))
         (insert "\n#ifdef __cplusplus\n}\n#endif /* __cplusplus */\n\n")
         (insert (concat (concat "\n#endif /* " nm) " */\n"))))))


(provide 'c-helper)
