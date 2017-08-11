;;; company-terraform.el --- A company backend for terraform

;; Copyright (C) 2017 Rafał Cieślak

;; Author: Rafał Cieślak <rafalcieslak256@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "24.4") (company "0.8.12") (terraform-mode "0.06"))
;; Created: 10 August 2017
;; Keywords: abbrev, convenience, terraform, company
;; URL: https://github.com/rafalcieslak/company-terraform

;;; Commentary:

;; company-terraform provides a company backend for terraform files.  It enables
;; context-aware autocompletion for terraform sources.  This includes resource
;; and data arguments and attributes, both in resource and data blocks as well
;; as in interpolations, built-in functions and top-level keywords.

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'subr-x)
(require 'terraform-mode)

(require 'company-terraform-data)

(defun company-terraform--scan-resources (dir)
  (let* ((files (directory-files dir t "\\.tf$"))
         (datas     (make-hash-table :test 'equal))
         (resources (make-hash-table :test 'equal))
         (variables '()))
    (dolist (file files)
      (with-temp-buffer
        (ignore-errors
          (insert-file-contents file))
        (goto-char 1) ; Start by searching for data and resource blocks.
        (while (re-search-forward "\\(resource\\|data\\)[[:space:]\n]*\"\\([^\"]*\\)\"[[:space:]\n]*\"\\([^\"]*\\)\"[[:space:]\n]*{" nil t)
          (let* ((kind (match-string-no-properties 1))
                 (hash-to-use (cond ((equal kind "data") datas)
                                    ((equal kind "resource") resources)))
                 (type (match-string-no-properties 2))
                 (name (match-string-no-properties 3)))
            (when (eq 'empty (gethash type hash-to-use 'empty))
              (puthash type '() hash-to-use))
            (push name (gethash type hash-to-use))))
        (goto-char 1) ; Then search for variable blocks.
        (while (re-search-forward "variable[[:space:]\n]*\"\\([^\"]*\\)\"[[:space:]\n]*{" nil t)
          (push (match-string-no-properties 1) variables))))
    (list datas resources variables)))

(defconst company-terraform-perdir-resource-cache
  (make-hash-table :test 'equal))

(defun company-terraform-get-resource-cache (&optional dir)
  "Return several dictionaries gathering names used in the project.
Searches for blocks in DIR or buffer's directory if DIR is nil.
If available, uses a cached version which lasts serval seconds."
  (let* ((dir (or dir (file-name-directory (buffer-file-name))))
         (v (gethash dir company-terraform-perdir-resource-cache))
         (cache-time (car v))
         (resource-data (cdr v)))
    (if (and v
             (< (- (float-time) cache-time) 20))
        resource-data
      (progn
        (message "Regenerating company-terraform resource cache for %s..." dir)
        (let ((resource-data (company-terraform--scan-resources dir)))
          (puthash dir (cons (float-time) resource-data) company-terraform-perdir-resource-cache)
          resource-data)))))

(defun company-terraform-get-context ()
  "Guess the context in terraform description where point is."
  (let ((nest-level (nth 0 (syntax-ppss)))
        (curr-ppos (nth 1 (syntax-ppss)))
        (string-state (nth 3 (syntax-ppss)))
        (string-ppos (nth 8 (syntax-ppss))))
    (cond
     ;; Resource/data type
     ((and string-state
           (save-excursion
             (goto-char string-ppos)
             (re-search-backward "\\(resource\\|data\\)[[:space:]\n]*\\=" nil t)))
      (list 'object-type (intern (match-string-no-properties 1))))
     ;; String interpolation
     ((and (> nest-level 0)
           string-state
           (save-excursion
             (re-search-backward "\\${[^\"]*\\=" nil t)))
      (list 'interpolation
            (buffer-substring
             (point)
             (save-excursion
               (with-syntax-table (make-syntax-table (syntax-table))
                 ;; Minus, asterisk and dot characters are part of the object path.
                 (modify-syntax-entry ?- "w")
                 (modify-syntax-entry ?. "w")
                 (modify-syntax-entry ?* "w")
                 (skip-syntax-backward "w")
                 (point))))))
     ;; Inside resource/data block
     ((and (eq ?{ (char-after curr-ppos))
           (save-excursion
             (goto-char curr-ppos)
             (re-search-backward "\\(resource\\|data\\)[[:space:]\n]*\"\\([^\"]*\\)\"[[:space:]\n]*\"[^\"]*\"[[:space:]\n]*\\=" nil t)))
      
      (list 'block (intern (match-string-no-properties 1)) (match-string-no-properties 2)))
     ;; Top level
     ((eq 0 nest-level) 'top-level)
     (t 'no-idea))))

(defun company-terraform-test-context ()
  "Echoes a message naming the current context in a terraform file.  Useful for diagnostics."
  (interactive)
  (message "company-terraform-context: %s" (company-terraform-get-context)))

(defun company-terraform--prefix ()
  "Return the text before point that is part of a completable symbol.
Check function ‘company-mode’ docs for the details on how this
function's result is interpreted."
  (if (eq major-mode 'terraform-mode)
      (let ((context (company-terraform-get-context)))
        (cond
         ((eq 'no-idea context) nil)
         ((eq 'top-level context) (company-grab-symbol))
         ((eq (car context) 'interpolation) (cons (car (last (split-string (nth 1 context) "\\."))) t))
         ((eq (car context) 'object-type) (company-grab-symbol-cons "\"" 1))
         ((equal (car context) 'resource) (company-grab-symbol))
         ((equal (car context) 'data) (company-grab-symbol))
         (t (company-grab-symbol))))))

(defun company-terraform--make-candidate (candidate)
  "Annotates a completion suggestion from a name-doc list CANDIDATE."
  (let ((text (nth 0 candidate))
        (doc (nth 1 candidate)))
    (propertize text 'doc doc)))

(defun company-terraform--filterdoc (prefix lists &optional multi)
  "Filters for the PREFIX a list (or a list of LISTS, if MULTI is not nil) of name-doc pairs."
  (if (not multi) (setq lists (list lists)))
  (let (res)
    (dolist (l lists)
      (dolist (item l)
        (when (string-prefix-p prefix (car item))
          (push (company-terraform--make-candidate item) res))))
    res))

(defun company-terraform--filter (prefix lists &optional multi)
  "Filters for the PREFIX a list (or a list of LISTS, if MULTI is not nil) of candidates."
  (if (not multi) (setq lists (list lists)))
  (let (res)
    (dolist (l lists)
      (dolist (item l)
        (when (string-prefix-p prefix item)
          (push item res))))
    res))

(defun company-terraform-is-resource-n (string)
  "True IFF the string is an integer or a literal * character."
  (if (string-match "\\`\\([0-9]+\\)\\|*\\'" string) t nil))

(defun company-terraform-candidates (prefix)
  "Prepare a list of autocompletion candidates for the given PREFIX."
  (let ((context (company-terraform-get-context)))
    (cond
     ((eq 'top-level context)
      (company-terraform--filterdoc prefix company-terraform-toplevel-keywords))
     ((equal context '(object-type data))
      (company-terraform--filterdoc prefix company-terraform-resources-list))
     ((equal context '(object-type data))
      (company-terraform--filterdoc prefix company-terraform-data-list))
     ((eq (car context) 'block)
      ;; Inside a block
      (cond
       ((eq (nth 1 context) 'resource)
        (company-terraform--filterdoc prefix (list (gethash (nth 2 context) company-terraform-resource-arguments-hash)
                                                  company-terraform-resource-extra) t))
       ((eq (nth 1 context) 'data)
        (company-terraform--filterdoc prefix (list (gethash (nth 2 context) company-terraform-data-arguments-hash)
                                                  company-terraform-data-extra) t))))
     ((equal (car context) 'interpolation)
       ;; Within interpolation
      (let* ((path (split-string (nth 1 context) "\\."))
             (pathlen (length path))
             (head (nth 0 path))
             (last (nth (- pathlen 1) path)))
        (cond
         ((eq pathlen 1)
          ;; Complete function name or resource type.
          (company-terraform--filterdoc prefix (list company-terraform-interpolation-functions
                                                    company-terraform-resources-list
                                                    company-terraform-interpolation-extra) t))
         ((equal head "count")
          (if (eq pathlen 2)
              ;; Complete count metadata
              (company-terraform--filterdoc last company-terraform-count-extra)))
         ((equal head "data")
          (let ((data-type (nth 1 path)))
            (cond ((eq pathlen 2)
                   ;; Complete data source type.
                   (company-terraform--filterdoc last company-terraform-data-list))
                  ((eq pathlen 3)
                   ;; Complete data name.
                   (company-terraform--filter
                    last
                    (gethash data-type
                             (nth 0 (company-terraform-get-resource-cache)))))
                  ;; Complete data arguments/attributes
                  ((or (eq pathlen 4)
                       (and (eq pathlen 5)
                            (company-terraform-is-resource-n (nth 3 path))))
                   (company-terraform--filterdoc
                    last
                    (list (gethash data-type company-terraform-data-arguments-hash)
                          (gethash data-type company-terraform-data-attributes-hash)) t)))))
         ((equal head "var")
          (if (eq pathlen 2)
              ;; Complete variable name.
              (company-terraform--filter last
                                        (nth 2 (company-terraform-get-resource-cache)))))
         (t ; This path is directly referencing a standard resource
          (let ((resource-type (nth 0 path)))
            (cond ((eq pathlen 2)
                   ;; Complete resource name.
                   (company-terraform--filter
                    last
                    (gethash resource-type
                             (nth 1 (company-terraform-get-resource-cache)))))
                  ((or (eq pathlen 3)
                       (and (eq pathlen 4)
                            (company-terraform-is-resource-n (nth 2 path))))
                   ;; Complete resource arguments/attributes
                   (company-terraform--filterdoc
                    last
                    (list (gethash resource-type company-terraform-resource-arguments-hash)
                          (gethash resource-type company-terraform-resource-attributes-hash)) t)))))))))))

(defun company-terraform-doc (candidate)
  "Return the documentation of a completion CANDIDATE."
  (get-text-property 0 'doc candidate))

(defun company-terraform-docbuffer (candidate)
  "Prepare a temporary buffer with completion CANDIDATE documentation."
  (company-doc-buffer (company-terraform-doc candidate)))

;;;###autoload
(defun company-terraform (command &optional arg &rest ignored)
  "Main entry point for a company backend.
Read `company-mode` function docs for the semantics of this function."
  (cl-case command
    (interactive (company-begin-backend 'company-test-backend))
    (prefix (company-terraform--prefix))
    (candidates (company-terraform-candidates arg))
    (meta (company-terraform-doc arg))
    (doc-buffer (company-terraform-docbuffer arg))))


;;;###autoload
(defun company-terraform-init ()
  "Add terraform to the company backends."
  (interactive)
  (add-to-list 'company-backends 'company-terraform))

(provide 'company-terraform)

;;; company-terraform.el ends here
