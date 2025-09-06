;;; org-algorithm-master.el --- Export algorithm master template -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Algorithm Competitor
;; Keywords: org, algorithm, export
;; Version: 1.0

;;; Commentary:

;; Simple exporter for the algorithm master template file.
;; Uses org-mode's native INCLUDE functionality for content control.

;;; Code:

(require 'org)
(require 'ox-latex)

(defgroup org-algorithm-master nil
  "Export algorithm master template."
  :group 'org
  :prefix "org-algorithm-master-")

(defcustom org-algorithm-master-file "~/org/algorithm-template-master.org"
  "Path to the algorithm master template file."
  :type 'file
  :group 'org-algorithm-master)

(defcustom org-algorithm-master-output "~/Documents/algorithm-templates.pdf"
  "Output path for the exported PDF."
  :type 'file
  :group 'org-algorithm-master)

;;;###autoload
(defun org-algorithm-export-master ()
  "Export the algorithm master template to PDF with transclusions."
  (interactive)
  (unless (file-exists-p org-algorithm-master-file)
    (user-error "Algorithm master file not found: %s" org-algorithm-master-file))

  (let ((org-latex-pdf-process '("lualatex -interaction nonstopmode -output-directory %o %f"
                                 "lualatex -interaction nonstopmode -output-directory %o %f"))
        (default-directory (file-name-directory org-algorithm-master-file)))
    
    (with-current-buffer (find-file-noselect org-algorithm-master-file)
      ;; 激活 org-transclusion 模式
      (when (fboundp 'org-transclusion-mode)
        (org-transclusion-mode 1))
      
      ;; 添加所有 transclusions
      (when (fboundp 'org-transclusion-add-all)
        (org-transclusion-add-all))
      
      ;; 导出到 PDF
      (let ((output-file (org-latex-export-to-pdf)))
        ;; 导出后移除 transclusions 以保持文件清洁
        (when (fboundp 'org-transclusion-remove-all)
          (org-transclusion-remove-all))
        
        (when (file-exists-p output-file)
          (copy-file output-file org-algorithm-master-output t)
          (message "Algorithm templates exported to: %s" org-algorithm-master-output)
          ;; 可选：打开PDF查看
          (when (y-or-n-p "Open PDF? ")
            (org-open-file org-algorithm-master-output)))))))

;;;###autoload
(defun org-algorithm-open-master ()
  "Open the algorithm master template file."
  (interactive)
  (find-file org-algorithm-master-file))

;;;###autoload
(defun org-algorithm-add-transclude ()
  "Add an ID-based transclude directive for the current org-roam node."
  (interactive)
  (unless (and (derived-mode-p 'org-mode)
               (org-roam-file-p))
    (user-error "Not in an org-roam file"))

  (let* ((title (org-get-title))
         (node-id (or (org-id-get)
                      (progn
                        (org-id-get-create)
                        (org-id-get))))
         (transclude-line (if node-id
                              (format "** %s\n#+transclude: [[id:%s]]\n"
                                      title node-id)
                            (format "** %s\n# Could not create ID for this node\n" title))))
    (kill-new transclude-line)
    (message "Transclude directive copied to kill-ring:\n%s" transclude-line)))

;;;###autoload
(defun org-algorithm-add-transclude-subtree ()
  "Add an ID-based transclude directive for the current subtree."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  
  (let* ((subtree-title (org-get-heading t t t t))
         (subtree-id (or (org-id-get)
                         (progn
                           (org-id-get-create)
                           (org-id-get))))
         (transclude-line (if subtree-id
                              (format "** %s\n#+transclude: [[id:%s]]\n"
                                      subtree-title subtree-id)
                            (format "** %s\n# Could not create ID for this subtree\n" subtree-title))))
    (kill-new transclude-line)
    (message "Subtree transclude directive copied to kill-ring:\n%s" transclude-line)))

(provide 'org-algorithm-master)

;;; org-algorithm-master.el ends here
