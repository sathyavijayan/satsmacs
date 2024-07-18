(defun sv-clockify-formatter (ipos tables params)
  (setq uber-time-table tables)
  (setq ftable (nth 2  tables))
  (cl-loop for tbl in tables
           for entries = (nth 2 tbl)
           do (cl-loop for entry in entries
                       for headline = (nth 1 entry)
                       do (setq headline (replace-regexp-in-string "TODO \\|DONE " "* " headline))
                       do (setcar (nthcdr 1 entry) headline)))
  (org-clocktable-write-default ipos '() params))


(defun org-dblock-write:clockify (params)
  (let ((clock-data (org-clock-get-table-data (buffer-name) params)))
    (setq v-clockify-params params)
    (setq v-clockify-clock-data clock-data)

    (insert "Hello")
    )

  )
