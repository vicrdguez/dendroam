;;; dendroam.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Victor Rodriguez
;;
;; Author: Victor Rodriguez <https://github.com/vrodriguez>
;; Maintainer: Victor Rodriguez <vrodriguez@confluent.io>
;; Created: April 26, 2021
;; Modified: April 26, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/vrodriguez/dendroam
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:



(provide 'dendroam)

(defvar dendroam-capture-templates
  '(("t" "Time note" entry
     "* %?"
     :if-new (file+head "${current-file}.%<%Y.%m.%d.%M%S%3N>.org"
                        "#+title: %^{title}\n\n"))
    ("s" "Scratch note" entry
     "* %?"
     :if-new (file+head "scratch.%<%Y.%m.%d.%M%S%3N>.org"
                        "#+title: %^{title}\n\n")))

  "Some utils templates for different type of notes such us time notes
or sratch notes")

;;Node custom getters
(cl-defmethod org-roam-node-current-file (node)
  "Gets node file-name-base by file name"
  (file-name-base (org-roam-node-file node)))

(cl-defmethod org-roam-node-hierarchy-title (node)
  "Gets node title excluding the hierarchy and capitalize it"
  (capitalize
   (car
    (last
     (split-string
      (org-roam-node-title node)
      "\\.")))))

(defun dendroam-format-hierarchy (file)
  "Formats node's path, to get the hierarchy whithout the title
where title will be the last child of the hierarchy:
from the filename this.is.a.hierarchy.note-title.org
returns this.is.a.hierarchy"
  (let* ((base-name (file-name-base file))
         (hierarchy-no-title (file-name-base base-name)))
    hierarchy-no-title))

(cl-defmethod org-roam-node-hierarchy (node)
  "Gets node hierarchy by file name"
  (funcall 'dendroam-format-hierarchy (org-roam-node-file node)))

(cl-defmethod org-roam-node-current-file (node)
  (file-name-base (buffer-file-name)))

;; Refactor functions

(defun dendroam-fetch-same-hierarchy-files (hierarchy)
  "Gets all the nodes that share the same HIERARCHY totally or parcially"
  (let ((files
         (mapcar #'car (org-roam-db-query [:select [file]
                                           :from nodes
                                           :where (like file $r1)]
                                          (concat "%" hierarchy "%")))))
    files))

(defun dendroam-refactor-hierarchy (&optional current)
  "Prompts the user to change the hierarchy of the current file
node and updates its hierarchy and the hierarchy of all the nodes
that have it if CURRENT is t the list of updated files is just
the current file"
  (interactive)
  (let*
      ((initial-file
        (file-name-nondirectory (buffer-file-name)))
       (initial-slug
        (file-name-base initial-file))
       (new-slug (file-name-base
                  (read-string "Refactor: " initial-slug)))
       (initial-slug-no-title
        (file-name-base initial-slug))
       (files-to-upd (if current
                         `(,initial-file)
                       (dendroam-fetch-same-hierarchy-files
                        initial-slug-no-title))))

    (dolist (file files-to-upd)
      (let ((new-file
             (replace-regexp-in-string initial-slug-no-title new-slug file)))
        (rename-file file new-file)
        (if (equal buffer-file-name file)
            (progn
              (kill-current-buffer)
              (find-file new-file)))))))

(defun dendroam-refactor-file ()
  (interactive)
  (let* ((initial-file (buffer-file-name))
         (initial-slug (file-name-base initial-file))
         (new-slug (read-string "Refactor: " initial-slug))
         (new-file (concat
                    (expand-file-name new-slug org-roam-directory)
                    ".org")))
    (rename-file initial-file new-file)
    (kill-current-buffer)
    (find-file new-file)))

;; Useful notes functions
(defun dendroam-insert-time-note(&optional goto)
  "Create a time note in the current level of the hierarchy.
GOTO is an `org-capture' parameter.
Time notes have the format: current.Y.m.d.MS3N
The file is created using a template from `dendroam-capture-templates'"
  (interactive "P")
  (org-roam-capture- :goto (when goto '(4))
                     :node (org-roam-node-create)
                     :templates dendroam-capture-templates
                     :keys "t"
                     :props (list :default-time (current-time))))

(defun dendroam-insert-scratch-note(&optional goto)
  "Create a time note in the current level of the hierarchy.
GOTO is an `org-capture' parameter.
Time notes have the format: current.Y.m.d.MS3N
The file is created using a template from `dendroam-capture-templates'"
  (interactive "P")
  (org-roam-capture- :goto (when goto '(4))
                     :node (org-roam-node-create)
                     :templates dendroam-capture-templates
                     :keys "s"
                     :props (list :default-time (current-time))))


(defun dendroam-node-find ()
  "Run completions with the current note as an initual input."
  (interactive)
  (org-roam-node-find nil (if (buffer-file-name)
                              (file-name-base (buffer-file-name))
                            "")))

(defun dendroam-node-find-siblings ()
  "Run completions showing all the siblings of the current note.
The initial input is the direct parent of the current note"
  (interactive)
  (org-roam-node-find nil (if (buffer-file-name)
                              (file-name-base
                               (file-name-base (buffer-file-name)))
                            "")))
;; Org roam overrides to allow these features
(eval-after-load "org-roam"
  '(cl-defmethod org-roam-node-slug ((node org-roam-node))
     "Override. Generates a dendron-like slug from *title*
this expects an input like: lang.elisp.what is nil
and will create a file wih the name: lang.elisp.what-is-nil"
     (let ((title (org-roam-node-title node)))
       (cl-flet* ((nonspacing-mark-p (char)
                                     (memq char org-roam-slug-trim-chars))
                  (strip-nonspacing-marks (s)
                                          (ucs-normalize-NFC-string
                                           (apply #'string (seq-remove #'nonspacing-mark-p
                                                                       (ucs-normalize-NFD-string s)))))
                  (cl-replace (title pair)
                              (replace-regexp-in-string (car pair) (cdr pair) title)))
         (let* ((pairs `(("[^[:alnum:][:digit:]_.]" . "-")  ;; convert anything not alphanumeric except "."
                         (" " . "-")    ;; remove whitespaces
                         ("__*" . "-")  ;; remove sequential underscores
                         ("^_" . "")  ;; remove starting underscore
                         ("_$" . "")))  ;; remove ending underscore
                (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
           (downcase slug))))))
;;; dendroam.el ends here
