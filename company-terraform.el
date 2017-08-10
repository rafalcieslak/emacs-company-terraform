;;; company-terraform.el --- A company backend for terraform

;; Copyright (C) 2017 Rafał Cieślak

;; Author: Rafał Cieślak <rafalcieslak256@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "24.4") (company "0.8.12") (terraform-mode "0.06"))
;; Created: 10 August 2017
;; Keywords: terraform, company
;; URL: https://github.com/rafalcieslak/company-terraform

;;; Commentary:

;; company-terraform provides a company backend for terraform files. It enables
;; context-aware autocompletion for terraform sources. This includes resource
;; and data arguments and attributes, both in resource and data blocks as well
;; as in interpolations, built-in functions and top-level keywords.

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'terraform-mode)

(require 'company-terraform-data)

(defun company-terraform-get-context ()
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
      (list 'object-type (match-string-no-properties 0)))
     ; string interpolation
     ((and (> nest-level 0)
           string-state
           (save-excursion
             (re-search-backward "\\${[^\"]*\\=" nil t)))
      (list 'interpolation (buffer-substring (point)
                                             (save-excursion
                                               (skip-syntax-backward "w.")
                                               (point)))))
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
  "Echoes a message naming the current context in a terraform file. Useful for diagnostics."
  (interactive)
  (message "company-terraform-context: %s" (company-terraform-get-context)))

(defun company-terraform-prefix ()
  (if (eq major-mode 'terraform-mode)
      (let ((context (company-terraform-get-context)))
        (cond
         ((eq 'top-level context) (company-grab-symbol))
         ((eq (car context) 'interpolation) (cons (car (last (split-string (nth 1 context) "\\."))) t))
         ((eq (car context) 'object-type) (company-grab-symbol-cons "\"" 1))
         ((equal (car context) "resource") (company-grab-symbol))
         ((equal (car context) "data") (company-grab-symbol))
         (t (company-grab-symbol))))
    nil))

(defun company-terraform-make-candidate (candidate)
  (let ((text (nth 0 candidate))
        (meta (nth 1 candidate)))
    (propertize text 'meta meta)))

(defun company-terraform-filterdoc (prefix lists &optional multi)
  (if (not multi) (setq lists (list lists)))
  (let (res)
    (dolist (l lists)
      (dolist (item l)
        (when (string-prefix-p prefix (car item))
          (push (company-terraform-make-candidate item) res))))
    res))

(defun company-terraform-candidates (prefix)
  (let ((context (company-terraform-get-context)))
    ;(message "%s" context)
    (cond
     ((eq 'top-level context)
      (company-terraform-filterdoc prefix company-terraform-toplevel-keywords))
     ((and (eq    (nth 0 context) 'object-type)
           (equal (nth 1 context) "resource ")) ; ??? Why is this space necessary?!
      (company-terraform-filterdoc prefix company-terraform-resources-list))
     ((and (eq    (nth 0 context) 'object-type)
           (equal (nth 1 context) "data ")) ; ??? Why is this space necessary?!
      (company-terraform-filterdoc prefix company-terraform-data-list))
     ((equal (car context) "resource")
      (company-terraform-filterdoc prefix (gethash (nth 1 context) company-terraform-resource-arguments-hash)))
     ((equal (car context) "data")
      (company-terraform-filterdoc prefix (gethash (nth 1 context) company-terraform-data-arguments-hash)))
     ((equal (car context) 'interpolation)
      (let ((a (split-string (nth 1 context) "\\.")))
        (cond
         ((eq (length a) 1)
          (company-terraform-filterdoc prefix (list company-terraform-interpolation-functions
                                                    company-terraform-resources-list
                                                    company-terraform-interpolation-extra) t))
         ((and (eq (length a) 2)
               (equal (nth 0 a) "data"))
          (company-terraform-filterdoc (nth 1 a) company-terraform-data-list))
         ((and (eq (length a) 3))
          (company-terraform-filterdoc (nth 2 a) (list (gethash (nth 0 a) company-terraform-resource-arguments-hash)
                                                       (gethash (nth 0 a) company-terraform-resource-attributes-hash)) t))
         ((and (eq (length a) 4)
               (equal (nth 0 a) "data"))
          (company-terraform-filterdoc (nth 3 a) (list (gethash (nth 1 a) company-terraform-data-arguments-hash)
                                                       (gethash (nth 1 a) company-terraform-data-attributes-hash)) t))
         (t nil))))
     (t nil))))

(defun company-terraform-meta (candidate)
  (get-text-property 0 'meta candidate))

(defun company-terraform-docstring (candidate)
  (company-doc-buffer (company-terraform-meta candidate)))

;;;###autoload
(defun company-terraform (command &optional arg &rest ignored)
  (cl-case command
    (interactive (company-begin-backend 'company-test-backend))
    (prefix (company-terraform-prefix))
    (candidates (company-terraform-candidates arg))
    (meta (company-terraform-meta arg))
    (doc-buffer (company-terraform-docstring arg))))


;;;###autoload
(defun company-terraform-init ()
  "Add terraform to the company backends."
  (interactive)
  (add-to-list 'company-backends 'company-terraform))

(provide 'company-terraform)

;;; company-terraform.el ends here
