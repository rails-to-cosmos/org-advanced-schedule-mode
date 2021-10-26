(require 'org)

(cl-defun oasm:cleanup (schedule)
  (mapcar #'(lambda (datestring)
              (s-replace-regexp "-[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}" "" datestring))
          schedule))

(cl-defun oasm:schedule-to-timestamps (schedule)
  (mapcar #'active-timestamp (oasm:cleanup schedule)))

(cl-defun oasm:timediff (timestamp &optional (current-time (org-current-time)))
  (let* ((current-sec (time-to-seconds current-time))
         (target-sec (org-time-string-to-seconds (active-timestamp timestamp)))
         (diff-sec (- target-sec current-sec))
         (week (* 7 24 60 60)))
    (cond ((and (> diff-sec 0) (< diff-sec week)) diff-sec)
          ((< diff-sec 0) (+ diff-sec week))
          ((> diff-sec week) (- diff-sec week)))))

(cl-defun oasm:sort (schedule &optional (current-time (org-current-time)))
  "Non-destructive sorting of times of SCHEDULE compared to CURRENT-TIME."
  (let ((timedeltas (mapcar #'oasm:timediff (oasm:cleanup schedule))))
    (cl-flet ((comparator (a b)
                (message "oasm:sort schedule: %s" schedule)
                (< (nth (-elem-index a schedule) timedeltas)
                   (nth (-elem-index b schedule) timedeltas))))
      (sort (copy-sequence schedule) #'comparator))))

(cl-defun oasm:nearest (schedule)
  (car (oasm:sort (mapcar #'identity (read schedule)))))

(cl-defun oasm:trigger (change-plist)
  (when (and (eql (plist-get change-plist :type) 'todo-state-change)
             (member (plist-get change-plist :to) org-done-keywords))
    (save-excursion
      (unless (org-at-heading-p)
        (org-back-to-heading-or-point-min))

      (when-let (schedule (org-element-property :ADVANCED_SCHEDULE (org-element-at-point)))
        (let* ((to-state (or (org-entry-get nil "REPEAT_TO_STATE" 'selective)
		             (and (stringp org-todo-repeat-to-state) org-todo-repeat-to-state)
		             (and org-todo-repeat-to-state org-last-state)))
               (nearest (oasm:nearest schedule)))

          (when (or org-log-repeat
	            (catch :clock
		      (save-excursion
		        (while (re-search-forward org-clock-line-re end t)
		          (when (org-at-clock-log-p) (throw :clock t))))))
            (org-entry-put nil "LAST_REPEAT" (format-time-string (org-time-stamp-format t t))))

          (when org-log-repeat
            (if (or (memq 'org-add-log-note (default-value 'post-command-hook))
	            (memq 'org-add-log-note post-command-hook))
	        ;; We are already setup for some record.
	        (when (eq org-log-repeat 'note)
	          ;; Make sure we take a note, not only a time stamp.
	          (setq org-log-note-how 'note))
	      ;; Set up for taking a record.
	      (org-add-log-setup 'state
                                 (car org-done-keywords)
			         org-last-state
			         org-log-repeat)))

          ;; Time-stamps without a repeater are usually skipped.  However,
          ;; a SCHEDULED time-stamp without one is removed, as they are no
          ;; longer relevant.
          (save-excursion
            (let ((scheduled (org-entry-get (point) "SCHEDULED")))
	      (when (and scheduled (not (string-match-p org-repeat-re scheduled)))
	        (org-remove-timestamp-with-keyword org-scheduled-string))))

          (org-add-planning-info 'scheduled nearest)
          (org-todo to-state))))))

(defvar org-advanced-schedule-mode-p nil)
(define-minor-mode org-advanced-schedule-mode
    "A minor mode that enables advanced scheduling of org tasks."
  nil nil nil
  (funcall (if org-advanced-schedule-mode #'add-hook #'remove-hook) 'org-trigger-hook #'oasm:trigger))

(provide 'org-advanced-schedule-mode)
