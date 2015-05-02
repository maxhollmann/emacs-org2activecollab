(require 'request)

(defvar org2ac-api-url nil
  "Something like https://yourproject.manageprojects.com/api.php")
(defvar org2ac-api-key nil
  "The API key, get it from Profile -> Options -> API Subscriptions")

(defvar org2ac-label-map '()
  "An alist that maps label names to ids")
(defvar org2ac-assignee-map '()
  "An alist that maps user names to ids")
(defvar org2ac-category-map '()
  "An alist that maps category names to ids")
(defvar org2ac-project-map '()
  "An alist that maps project names to ids")
(defvar org2ac-milestone-map '()
  "An alist that maps milestone names to ids")
(defvar org2ac-priority-map
  '(("A" . 1)
    ("B" . 0)
    ("C" . -1))
  "An alist that maps priority letters to integers")

(defvar org2ac-default-label     nil
  "Label id to use when none is specified")
(defvar org2ac-default-assignee  nil
  "User id to use when none is specified")
(defvar org2ac-default-category  nil
  "Category id to use when none is specified")
(defvar org2ac-default-project   nil
  "Project id to use when none is specified")
(defvar org2ac-default-milestone nil
  "Milestone id to use when none is specified")
(defvar org2ac-default-priority  0
  "Priority integer to use when the entry isn't prioritized")

(defvar org2ac-assignee-property-name "aca"
  "Property that stores the assignee")
(defvar org2ac-category-property-name "acc"
  "Property that stores the category")
(defvar org2ac-project-property-name  "acp"
  "Property that stores the project")
(defvar org2ac-milestone-property-name "acm"
  "Property that stores the milestone")
(defvar org2ac-id-property-name  "ac-id"
  "Property that stores the task-id")



(defmacro org2ac-->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))
(defmacro org2ac--> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append (list (car form) result)
                           (cdr form))))))

(defun org2ac--chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)



(defun ac--format-param (param)
  (cond ((integerp param) (int-to-string param))
        (t param)))

(defun ac-make-url (path)
  (concat org2ac-api-url
          "?path_info="      path
          "&auth_api_token=" org2ac-api-key
          "&format=json"))

(defun ac-do-request (method path &optional data callback)
  (request
   (ac-make-url path)
   :type    method
   :data    data
   :parser  'json-read
   :success callback
   :error   (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                         (message "Error: %S" error-thrown)))
   :status-code '((400 . (lambda (&rest _) (message "Got 400.") (print _))))))

(defun ac-add-or-update-task (task &optional callback)
  (let ((id          (plist-get task :id))
        (title       (plist-get task :title))
        (description (plist-get task :content))
        (project     (plist-get task :project))
        (category    (plist-get task :category))
        (label       (plist-get task :label))
        (milestone   (plist-get task :milestone))
        (priority    (plist-get task :priority))
        (assignee    (plist-get task :assignee)))
    (let ((path (concat "projects/"
                        (int-to-string project)
                        "/tasks/"
                        (if id
                            (concat id "/edit")
                          "add"))))
      (ac-do-request
       "POST"
       path
       (list (cons "task[name]"         title)
             (cons "task[body]"         description)
             (cons "task[visibility]"   "1")
             (cons "task[category_id]"  (ac--format-param category))
             (cons "task[label_id]"     (ac--format-param label))
             (cons "task[milestone_id]" (ac--format-param milestone))
             (cons "task[priority]"     (ac--format-param priority))
             (cons "task[assignee_id]"  (ac--format-param assignee))
             (cons "submitted" "submitted"))
       callback))))


(defun org2ac-convert-priority (prio-letter)
  (or (cdr (assoc prio-letter org2ac-priority-map))
      org2ac-default-priority))
(defun org2ac-convert-project (project)
  (or (cdr (assoc project org2ac-project-map))
      org2ac-default-project))
(defun org2ac-convert-assignee (assignee)
  (or (cdr (assoc assignee org2ac-assignee-map))
      org2ac-default-assignee))
(defun org2ac-convert-category (category)
  (or (cdr (assoc category org2ac-category-map))
      org2ac-default-category))
(defun org2ac-convert-milestone (milestone)
  (or (cdr (assoc milestone org2ac-milestone-map))
      org2ac-default-milestone))
(defun org2ac-convert-label (label)
  (or (cdr (assoc label org2ac-label-map))
      org2ac-default-label))

(defun org2ac-get-content (begin end level)
  (if (and begin end)
      (org2ac-->> (buffer-substring-no-properties begin end)
                  (replace-regexp-in-string "^SCHEDULED:.*$" "")
                                        ; remove properties
                  (replace-regexp-in-string "^:[^\s]+:.*$" "")
                                        ; remove whitespace
                  (org2ac--chomp)
                                        ; remove superfluous *s
                  (replace-regexp-in-string (concat "^\\*\\{"
                                                    (int-to-string level)
                                                    "\\}")
                                            "")
                                        ; newlines to breaks
                  (replace-regexp-in-string "\n" "<br />\n"))
    ""))

(defun org2ac-get-task-data-from-current-heading ()
  (let ((el (org-element-at-point)))
    (let ((type (car el))
          (props (car (cdr el)))
          (properties (org-entry-properties)))
      (if (not (eql 'headline type))
          ""  nil
          (list
           :content     (let ((begin      (plist-get props :contents-begin))
                              (end        (plist-get props :contents-end))
                              (level      (plist-get props :level)))
                          (org2ac-get-content begin end level))

           :title       (plist-get props :title)
           :priority    (org-entry-get (point) "PRIORITY")
           :level       (plist-get props :level)
           :label       (car (plist-get props :tags))
           :assignee    (cdr (assoc org2ac-assignee-property-name properties))
           :category    (cdr (assoc org2ac-category-property-name properties))
           :project     (cdr (assoc org2ac-project-property-name properties))
           :milestone   (cdr (assoc org2ac-milestone-property-name properties))

           :id          (cdr (assoc org2ac-id-property-name properties))
           )))))

(defun org2ac-task-data-convert-to-ac-values (task)
  (let ((priority  (plist-get task :priority))
        (label     (plist-get task :label))
        (assignee  (plist-get task :assignee))
        (category  (plist-get task :category))
        (project   (plist-get task :project))
        (milestone (plist-get task :milestone)))

    (org2ac--> task
               (plist-put :priority  (org2ac-convert-priority  priority))
               (plist-put :label     (org2ac-convert-label     label))
               (plist-put :assignee  (org2ac-convert-assignee  assignee))
               (plist-put :category  (org2ac-convert-category  category))
               (plist-put :project   (org2ac-convert-project   project))
               (plist-put :milestone (org2ac-convert-milestone milestone)))))

(defun org2ac-save-task-from-heading-at-point ()
  (interactive)
  (let ((data (org2ac-get-task-data-from-current-heading)))
    (if (not data)
        (message "Not on a heading")
      (ac-add-or-update-task
       (org2ac-task-data-convert-to-ac-values data)
       (function* (lambda (&key data &allow-other-keys)
                    (org-set-property org2ac-id-property-name
                                      (int-to-string
                                       (cdr (assoc 'task_id data))))
                    (message "Task saved!")))))))
