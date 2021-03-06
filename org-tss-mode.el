(require 'org)

(defvar-local -org-tss:local-timestamps '())

(define-minor-mode org-tss-mode "Handle multiple repeatable timestamps."
  nil nil nil
  (cond
    (org-tss-mode (advice-add 'org-auto-repeat-maybe :before #'org-tss:capture)
                  (advice-add 'org-auto-repeat-maybe :after #'org-tss:restore))
    (t (advice-remove 'org-auto-repeat-maybe #'org-tss:capture)
       (advice-remove 'org-auto-repeat-maybe #'org-tss:restore))))

(cl-defun org-tss:get-buffer-timestamps ()
  (sort (save-restriction
          (org-narrow-to-subtree)
          (cl-loop
             for timestamp in (org-element-map (org-element-parse-buffer) '(timestamp) #'identity)
             when (and (eql 'active (org-element-property :type timestamp))
                       (< 0 (org-element-property :repeater-value timestamp)))
             collect timestamp))
        #'(lambda (lhs rhs) (time-less-p
                        (org-time-string-to-time (org-element-property :raw-value lhs))
                        (org-time-string-to-time (org-element-property :raw-value rhs))))))

(cl-defun org-tss:capture (&rest args)
  (setq-local -org-tss:local-timestamps (org-tss:get-buffer-timestamps)))

(cl-defun org-tss:restore (&rest args)
  (let ((-org-tss:local-timestamps* (org-tss:get-buffer-timestamps)))
    (cl-loop
       for tsi from 1 below (length -org-tss:local-timestamps*)
       for ts = (nth tsi -org-tss:local-timestamps)
       for ts* = (nth tsi -org-tss:local-timestamps*)
       do (save-excursion
            (goto-char (org-element-property :begin ts*))
            (delete-region (org-element-property :begin ts*)
                           (org-element-property :end ts*))
            (insert (org-element-property :raw-value ts))))))

(provide 'org-tss-mode)
