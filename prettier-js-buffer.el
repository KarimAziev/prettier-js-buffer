;;; prettier-js-buffer.el --- Run prettier-js in buffers without filename -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/prettier-js-buffer
;; Version: 0.1.0
;; Keywords: tools languages
;; Package-Requires: ((emacs "27.1") (prettier-js "0.1.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configure js buffer

;;; Code:

(require 'prettier-js)

(defcustom prettier-js-buffer-global-args '("--single-quote")
  "List of default global args to send to prettier command."
  :type '(repeat string)
  :group 'prettier-js-buffer-global-args)

(defcustom prettier-js-buffer-blacklist-regexp "/\\(tmp\\|node_modules\\)"
  "Regexp of directories to disable prettier setup."
  :type 'regexp
  :group 'prettier-js-buffer-global-args)

(defcustom prettier-js-buffer-parsers '(("md" . "markdown")
                                        ("mdx" . "markdown")
                                        ("js" . "babel")
                                        ("jsx" . "babel")
                                        ("ts" . "typescript")
                                        ("tsx" . "typescript")
                                        ("json" . "json")
                                        ("css" . "css")
                                        ("scss" . "scss")
                                        ("less" . "less")
                                        ("json5" . "json5")
                                        ("yaml" . "yaml")
                                        ("yml" . "yaml")
                                        ("html" . "html")
                                        ("heapprofile" . "json"))
  "Alist of allowed file extensions and corresponding prettier parsers."
  :group 'prettier-js
  :type '(alist
          :key-type (string :tag "File extension")
          :value-type (string :tag "Parser")))

(defcustom prettier-js-buffer-major-modes-parsers '((markdown-mode . "markdown")
                                                    (gfm-mode "markdown")
                                                    (js-mode . "babel")
                                                    (js2-mode . "babel")
                                                    (typescript-mode .
                                                                     "typescript")
                                                    (tsx-ts-mode . "typescript")
                                                    (typescript-ts-mode .
                                                                        "typescript")
                                                    (json-mode . "json")
                                                    (css-mode . "css")
                                                    (html-mode . "html")
                                                    (scss-mode . "scss")
                                                    (yaml-mode . "yaml"))
  "Alist of allowed major modes and corresponding prettier parsers."
  :group 'prettier-js
  :type '(alist
          :key-type (symbol :tag "Major mode")
          :value-type (string :tag "Parser")))

(defun prettier-js-buffer-find-exec ()
  "Return prettier executable, either from node_modules or globally."
  (let ((dir default-directory)
        (node-modules)
        (found))
    (while (setq node-modules
                 (unless found
                   (setq dir (locate-dominating-file
                              dir
                              "node_modules"))))
      (setq dir (let ((parent (file-name-directory
                               (directory-file-name
                                (expand-file-name dir default-directory)))))
                  (when (and
                         (file-exists-p dir)
                         (file-exists-p parent)
                         (not (equal
                               (file-truename (directory-file-name
                                               (expand-file-name dir)))
                               (file-truename (directory-file-name
                                               (expand-file-name parent))))))
                    (if (file-name-absolute-p dir)
                        (directory-file-name parent)
                      (file-relative-name parent)))))
      (let ((file (expand-file-name "node_modules/.bin/prettier" node-modules)))
        (setq found
              (when (and (file-exists-p file)
                         (file-executable-p file))
                file))))
    (or found (executable-find "prettier"))))

(defun prettier-js-buffer-node-modules-bin-files ()
  "Look up directory hierarchy for executable files in node_modules/.bin."
  (when-let* ((node-modules
               (locate-dominating-file
                default-directory
                "node_modules"))
              (exec-dir
               (expand-file-name "node_modules/.bin/" node-modules))
              (commands
               (seq-filter #'file-executable-p
                           (and (file-exists-p exec-dir)
                                (directory-files-recursively exec-dir ".")))))
    commands))

(defun prettier-js-buffer-local-command ()
  "Return local command for prettier."
  (seq-find (lambda (it)
              (string= "prettier"
                       (file-name-base it)))
            (prettier-js-buffer-node-modules-bin-files)))


(defun prettier-js-buffer-setup ()
  "Enable prettier from project directory."
  (unless (or (not prettier-js-buffer-blacklist-regexp)
              (string-match-p "\\(tmp\\|snippets\\|node_modules\\)"
                              default-directory))
    (let ((local-cmd (prettier-js-buffer-local-command)))
      (setq-local prettier-js-command
                  (or local-cmd
                      (executable-find "prettier")))
      (when (and prettier-js-command
                 (not local-cmd))
        (setq-local prettier-js-args prettier-js-buffer-global-args))
      (when (fboundp 'prettier-js-mode)
        (prettier-js-mode (if prettier-js-command 1 -1))))))

(defun prettier-js-buffer-string (string &rest options)
  "Apply prettier on STRING with OPTIONS.
Return list of two elements: status (t or nil) and string with result."
  (when-let ((prettier-cmd (prettier-js-buffer-find-exec)))
    (with-temp-buffer
      (insert string)
      (when (eq 0
                (apply #'call-process-region
                       (append
                        (list (point-min)
                              (point-max)
                              prettier-cmd
                              t
                              t
                              nil)
                        (flatten-list options))))
        (buffer-string)))))


;;;###autoload
(defun prettier-js-buffer (&optional arg)
  "Set and run prettier from project directory, if found othervise as global.
With prefix ARG ask which parser to use."
  (interactive "P")
  (require 'prettier-js)
  (if-let ((local-prettier (when buffer-file-name
                             (prettier-js-buffer-local-command))))
      (setq-local prettier-js-command local-prettier)
    (let ((args
           prettier-js-buffer-global-args)
          (parser (or (and buffer-file-name
                           (cdr
                            (assoc (file-name-extension buffer-file-name)
                                   prettier-js-buffer-parsers)))
                      (cdr  (assoc major-mode
                                   prettier-js-buffer-major-modes-parsers)))))
      (setq-local prettier-js-args
                  (if parser
                      (append (list "--parser" parser) args)
                    args))
      (setq-local prettier-js-command (or (prettier-js-buffer-local-command)
                                          (executable-find "prettier")))
      (setq-local prettier-js-show-errors 'echo)))
  (when prettier-js-command
    (setq-local prettier-js-args
                (delete nil (if arg
                                (append
                                 prettier-js-args
                                 (list "--parser"
                                       (completing-read
                                        "--parser "
                                        (mapcar #'cdr
                                                prettier-js-buffer-parsers))))
                              prettier-js-args)))
    (condition-case nil
        (progn (if prettier-js-command
                   (prettier-js-mode 1)
                 (prettier-js-mode -1))
               (prettier-js))
      (error (prettier-js-mode -1)
             (when-let ((formatted (prettier-js-buffer-string
                                    (buffer-substring-no-properties
                                     (point-min)
                                     (point-max))
                                    prettier-js-args)))
               (if (fboundp 'replace-region-contents)
                   (replace-region-contents (point-min)
                                            (point-max)
                                            (lambda () formatted))
                 (delete-region (point-min)
                                (point-max))
                 (insert formatted)))))))

(defun prettier-js-buffer-or-region (prettier-fn &rest args)
  "Run PRETTIER-FN with ARGS or `prettier-js'.
If value of the variable `buffer-file-name' is nil, run `prettier-js',
otherwise run prettier-fn."
  (if buffer-file-name
      (apply prettier-fn args)
    (prettier-js-buffer)))

(provide 'prettier-js-buffer)
;;; prettier-js-buffer.el ends here