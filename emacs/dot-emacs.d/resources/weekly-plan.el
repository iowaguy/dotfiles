;;;###autoload
(defun weekly-plan ()
  "Create a weekly plan file from the latest template."
  (interactive)
  ;; generate file name
  (let* ((filename (concat
                   "weekly-"
                   (format-time-string "%m-%d-%y")
                   ".md"))
         (plan-path "~/writing/planning/")
         (full-file-path (concat plan-path filename)))
    (if (file-exists-p full-file-path)
        ;; open the file if it exists
        (find-file full-file-path)
      (create-new-weekly-plan filename plan-path))))

(defun create-new-weekly-plan (new-plan-filename plan-path)
  (let* ((template (concat plan-path "template.md"))
        (new-plan-path (concat plan-path new-plan-filename))

        ;;   determine previous weekly plan
        (last-plan-date (format-time-string "%m-%d-%y" (time-subtract (current-time) (days-to-time 7))))
        (last-plan (concat plan-path "weekly-" last-plan-date ".md")))

    ;;   copy template into new filename
    (copy-file template new-plan-path 1)

    ;;   open new file and previous weekly plan side by side
    ;; (switch-to-buffer (find-file-noselect new-plan-path))
    ;; (find-file new-plan-path)
    ;; (find-file last-plan)
    (2-parallel-windows new-plan-path last-plan)))


(defun 2-parallel-windows (new-plan old-plan)
    (delete-other-windows)
    (switch-to-buffer  (find-file-noselect new-plan))
    (split-window-horizontally)
    (other-window 1)
    (switch-to-buffer (find-file-noselect old-plan)))

(provide 'weekly-plan)
