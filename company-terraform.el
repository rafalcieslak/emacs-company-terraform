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
(require 'terraform-mode)

(require 'company-terraform-data)

(defun company-terraform-scan-resources (dir)
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

(defun company-terraform-get-resource-cache (dir)
  (let* ((v (gethash dir company-terraform-perdir-resource-cache))
         (cache-time (car v))
         (resource-data (cdr v)))
    (if (and v
             (< (- (float-time) cache-time) 20))
        resdata
      (progn
        (message "Regenerating company-terraform resource cache for %s..." dir)
        (let ((resource-data (company-terraform-scan-resources dir)))
          (puthash dir (cons (float-time) resource-data) company-terraform-perdir-resource-cache)
          resource-data)))))

(defun company-terraform-get-context ()
  "Guess the context in terraform description where point is."
  (let ((nest-level (nth 0 (syntax-ppss)))
        (curr-ppos (nth 1 (syntax-ppss)))
        (string-state (nth 3 (syntax-ppss)))
        (string-ppos (nth 8 (syntax-ppss))))
    (cond
     ; object kind
     ((and string-state
           (save-excursion
             (goto-char string-ppos)
             (re-search-backward "\\(resource\\|data\\)[[:space:]\n]*\\=" nil t)))
      (list 'object-type (match-string-no-properties 1)))
     ; string interpolation
     ((and (> nest-level 0)
           string-state
           (save-excursion
             (re-search-backward "\\${[^\"]*\\=" nil t)))
      (list 'interpolation (buffer-substring (point)
                                             (save-excursion
                                               (with-syntax-table (make-syntax-table (syntax-table))
                                                 ; Minus and dot characters are part of the object path.
                                                 (modify-syntax-entry ?- "w")
                                                 (modify-syntax-entry ?. "w")
                                                 (skip-syntax-backward "w")
                                                 (point))))))
     ; resource/data block
     ((and (eq ?{ (char-after curr-ppos))
           (save-excursion
             (goto-char curr-ppos)
             (re-search-backward "\\(resource\\|data\\)[[:space:]\n]*\"\\([^\"]*\\)\"[[:space:]\n]*\"[^\"]*\"[[:space:]\n]*\\=" nil t)))
      
      (list (match-string-no-properties 1) (match-string-no-properties 2)))
     ; top level
     ((eq 0 nest-level) 'top-level)
     (t 'no-idea))))

(defun company-terraform-test-context ()
  "Echoes a message naming the current context in a terraform file.  Useful for diagnostics."
  (interactive)
  (message "company-terraform-context: %s" (company-terraform-get-context)))

(defun company-terraform-prefix ()
  "Return the text before point that is part of a completable symbol.
Check function ‘company-mode’ docs for the details on how this
function's result is interpreted."
  (if (eq major-mode 'terraform-mode)
      (let ((context (company-terraform-get-context)))
        (cond
         ((eq 'top-level context) (company-grab-symbol))
         ((eq (car context) 'interpolation) (cons (car (last (split-string (nth 1 context) "\\."))) t))
         ((eq (car context) 'object-type) (company-grab-symbol-cons "\"" 1))
         ((equal (car context) "resource") (company-grab-symbol))
         ((equal (car context) "data") (company-grab-symbol))
         (t (company-grab-symbol))))))

(defun company-terraform-make-candidate (candidate)
  "Annotates a completion suggestion from a name-doc list CANDIDATE."
  (let ((text (nth 0 candidate))
        (doc (nth 1 candidate)))
    (propertize text 'doc doc)))

(defun company-terraform-filterdoc (prefix lists &optional multi)
  "Filters for the PREFIX a list (or a list of LISTS, if MULTI is not nil) of name-doc pairs."
  (if (not multi) (setq lists (list lists)))
  (let (res)
    (dolist (l lists)
      (dolist (item l)
        (when (string-prefix-p prefix (car item))
          (push (company-terraform-make-candidate item) res))))
    res))

(defun company-terraform-filter (prefix lists &optional multi)
  "Filters for the PREFIX a list (or a list of LISTS, if MULTI is not nil) of candidates."
  (if (not multi) (setq lists (list lists)))
  (let (res)
    (dolist (l lists)
      (dolist (item l)
        (when (string-prefix-p prefix item)
          (push item res))))
    res))

(defun company-terraform-candidates (prefix)
  "Prepare a list of autocompletion candidates for the given PREFIX."
  (let ((context (company-terraform-get-context)))
    (cond
     ((eq 'top-level context)
      (company-terraform-filterdoc prefix company-terraform-toplevel-keywords))
     ((and (eq    (nth 0 context) 'object-type)
           (equal (nth 1 context) "resource"))
      (company-terraform-filterdoc prefix company-terraform-resources-list))
     ((and (eq    (nth 0 context) 'object-type)
           (equal (nth 1 context) "data"))
      (company-terraform-filterdoc prefix company-terraform-data-list))
     ((equal (car context) "resource")
      (company-terraform-filterdoc prefix (list (gethash (nth 1 context) company-terraform-resource-arguments-hash)
                                                company-terraform-resource-extra) t))
     ((equal (car context) "data")
      (company-terraform-filterdoc prefix (list (gethash (nth 1 context) company-terraform-data-arguments-hash)
                                                company-terraform-data-extra) t))
     ((equal (car context) 'interpolation)
      (let* ((a (split-string (nth 1 context) "\\."))
             (last (nth (- (length a) 1) a)))
        (cond
         ; Complete function name or resource type.
         ((eq (length a) 1)
          (company-terraform-filterdoc prefix (list company-terraform-interpolation-functions
                                                    company-terraform-resources-list
                                                    company-terraform-interpolation-extra) t))
         ; Complete count metadata
         ((and (eq (length a) 2)
               (equal (nth 0 a) "count"))
          (company-terraform-filterdoc last company-terraform-count-extra))
         ; Complete data source type.
         ((and (eq (length a) 2)
               (equal (nth 0 a) "data"))
          (company-terraform-filterdoc last company-terraform-data-list))
         ; Complete variable name.
         ((and (eq (length a) 2)
               (equal (nth 0 a) "var"))
          (company-terraform-filter
           last
           (nth 2 (company-terraform-get-resource-cache
                   (file-name-directory (buffer-file-name))))))
         ; Complete resource name.
         ((and (eq (length a) 2))
          (company-terraform-filter
           last
           (gethash (nth 0 a)
                    (nth 1 (company-terraform-get-resource-cache
                            (file-name-directory (buffer-file-name)))))))
         ; Complete data name.
         ((and (eq (length a) 3)
               (equal (nth 0 a) "data"))
          (company-terraform-filter
           last
           (gethash (nth 1 a)
                    (nth 0 (company-terraform-get-resource-cache
                            (file-name-directory (buffer-file-name)))))))
         ; Complete resource arguments/attributes
         ((and (eq (length a) 3))
          (company-terraform-filterdoc last (list (gethash (nth 0 a) company-terraform-resource-arguments-hash)
                                                  (gethash (nth 0 a) company-terraform-resource-attributes-hash)) t))
         ; Complete data arguments/attributes
         ((and (eq (length a) 4)
               (equal (nth 0 a) "data"))
          (company-terraform-filterdoc last (list (gethash (nth 1 a) company-terraform-data-arguments-hash)
                                                  (gethash (nth 1 a) company-terraform-data-attributes-hash)) t))
         (t nil))))
     (t nil))))

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
    (prefix (company-terraform-prefix))
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
