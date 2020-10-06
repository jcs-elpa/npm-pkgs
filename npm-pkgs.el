;;; npm-pkgs.el --- A npm packages client  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-10-05 17:24:59

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: A npm packages client.
;; Keyword: npm packages client
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (request "0.3.0") (s "1.12.0"))
;; URL: https://github.com/jcs-elpa/npm-pkgs

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A npm packages client.
;;

;;; Code:

(require 'browse-url)
(require 'button)

(require 'dom)
(require 'request)
(require 's)

(defgroup npm-pkgs nil
  "A npm packages client."
  :prefix "npm-pkgs-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/npm-pkgs"))

(defconst npm-pkgs-url "https://www.npmjs.com/search/"
  "Npm packages url.")

(defconst npm-pkgs--url-format (format "%ssuggestions?q=%%s" npm-pkgs-url)
  "URL format to request NPM packages by search.")

(defvar npm-pkgs--request nil
  "Store request object.")

(defvar npm-pkgs--data nil
  "Use to store package list data.")

(defvar npm-pkgs--buffer nil
  "Record the buffer that calls `npm-pkgs'.")

;;; Util

(defun npm-pkgs--project-roort ()
  "Return project root path."
  (cdr (project-current)))

;;; Global

;;; Local

(defvar npm-pkgs--local-packages nil
  "List of local packages.")

(defun npm-pkgs--local-json ()
  "Return JSON from current `package.json' file."
  (let ((pkg-path (f-join (npm-pkgs--project-roort) "package.json")))
    (if (file-exists-p pkg-path) (json-read-file pkg-path) nil)))

(defun npm-pkgs--get-dep (data)
  "Get dependency list from DATA."
  (let ((dep-lst '()) p-name p-value)
    (dolist (plst data)
      (setq p-name (car plst) p-value (cdr plst))
      (when (string= p-name "dependencies")
        (dolist (dep p-value)
          (push (list :name (symbol-name (car dep))
                      :version (s-replace "^" "" (cdr dep))
                      :status "dependencies")
                dep-lst))))
    dep-lst))

(defun npm-pkgs--get-dev-dep (data)
  "Get development dependency list from DATA."
  (let ((dev-dep-lst '()) p-name p-value)
    (dolist (plst data)
      (setq p-name (car plst) p-value (cdr plst))
      (when (string= p-name "devDependencies")
        (dolist (dev-dep p-value)
          (push (list :name (symbol-name (car dev-dep))
                      :version (s-replace "^" "" (cdr dev-dep))
                      :status "devDependencies")
                dev-dep-lst))))
    dev-dep-lst))

(defun npm-pkgs--local-collect ()
  "Collect local package data."
  (if (not (npm-pkgs--project-roort))
      (user-error "[WARNINGS] No project root detected")
    (setq npm-pkgs--local-packages '())
    (let* ((data (npm-pkgs--local-json))
           (dep (npm-pkgs--get-dep data))
           (dev-dep (npm-pkgs--get-dev-dep data)))
      (setq npm-pkgs--local-packages (append dep dev-dep)))))

;;; Core

(defun npm-pkgs-reset-request ()
  "Cancel the current request and set to initialized state."
  (when npm-pkgs--request (request-abort npm-pkgs--request))
  (setq npm-pkgs--request nil))

(defun npm-pkgs--search (text)
  "Search npm suggested packages list by TEXT."
  (npm-pkgs-reset-request)
  (setq npm-pkgs--request
        (request
          (format npm-pkgs--url-format text)
          :type "GET"
          :parser 'buffer-string
          :success
          (cl-function
           (lambda (&key data &allow-other-keys)
             (setq npm-pkgs--data (json-parse-string data))
             (npm-pkgs--refresh)
             (npm-pkgs-reset-request)))
          :error
          ;; NOTE: Accept, error.
          (cl-function
           (lambda (&rest args &key _error-thrown &allow-other-keys)
             (npm-pkgs-reset-request))))))

;;; Button

(define-button-type 'npm-pkgs--name-button
  'action 'npm-pkgs--button-name)

(define-button-type 'npm-pkgs--author-button
  'action 'npm-pkgs--button-author)

(defun npm-pkgs--button-name (_button)
  "Click on name."
  (let ((pkg-name (npm-pkgs--tablist-get-value 'name)))
    (when pkg-name
      (browse-url (format "https://www.npmjs.com/package/%s" pkg-name)))))

(defun npm-pkgs--button-author (_button)
  "Click on published."
  (let ((author (npm-pkgs--tablist-get-value 'author)))
    (when author
      (browse-url (format "https://www.npmjs.com/~%s" author)))))

(defun npm-pkgs--make-buttons ()
  "Make button to npm client."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((name-pkg (npm-pkgs--tablist-get-value 'name))
            (name-author (npm-pkgs--tablist-get-value 'author)))
        (when name-pkg
          (move-to-column (npm-pkgs--tablist-column 'name))
          (make-button (save-excursion (forward-word 1) (forward-word -1) (point))
                       (+ (point) (length name-pkg))
                       :type 'npm-pkgs--name-button)
          (move-to-column (npm-pkgs--tablist-column 'author))
          (make-button (save-excursion (forward-word 1) (forward-word -1) (point))
                       (+ (point) (length name-author))
                       :type 'npm-pkgs--author-button)))
      (forward-line 1))))

;;; Buffer

(defconst npm-pkgs--tablist-format
  (vector (list "Name" 14 t)
          (list "Version" 7 t)
          (list "Status" 15)
          (list "Description" 50 t)
          (list "Published" 15)
          (list "Date" 15 t))
  "Format to assign to `tabulated-list-format' variable.")

(defconst npm-pkgs--buffer-name "*npm-pkgs*"
  "Name of the buffer to display npm packages.")

(defconst npm-pkgs--title-prefix "Keywords: "
  "Header put infront of the input string.")

(defcustom npm-pkgs-delay 0.5
  "Input delay to refresh buffer."
  :type 'float
  :group 'npm-pkgs)

(defconst npm-pkgs--key-list
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
    "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
    "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" "`"
    "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "\\" "~"
    "{" "}" "[" "]" ";" ":" "'" "\"" "," "." "<" ">" "/"
    "?" "|" " ")
  "List of key to bind.")

(defvar-local npm-pkgs--refresh-timer nil
  "Store refresh timer function.")

(defvar npm-pkgs-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (key-str npm-pkgs--key-list)
      (define-key map key-str
        (lambda () (interactive) (npm-pkgs--input key-str))))
    (define-key map (kbd "<backspace>")
      (lambda () (interactive) (npm-pkgs--input "" -1)))
    map)
  "Kaymap for `npm-pkgs-mode'.")

(defun npm-pkgs--tablist-index (sym)
  "Return index id by SYM."
  (cl-case sym
    (name 0) (version 1) (status 2) (description 3) (author 4) (date 5)))

(defun npm-pkgs--tablist-symbol (index)
  "Return symbol id by INDEX."
  (cl-case index
    (0 'name) (1 'version) (2 'status) (3 'description) (4 'author) (5 'date)))

(defun npm-pkgs--tablist-get-value (sym)
  "Get value by SYM."
  (let ((entry (tabulated-list-get-entry)) idx)
    (when entry (setq idx (npm-pkgs--tablist-index sym)))
    (if entry (aref entry idx) nil)))

(defun npm-pkgs--tablist-format-width (sym)
  "Return the width for SYM in `npm-pkgs--tablist-format'."
  (nth 1 (aref npm-pkgs--tablist-format (npm-pkgs--tablist-index sym))))

(defun npm-pkgs--tablist-width (sym)
  "Return the width for SYM in buffer."
  (let ((format-width (npm-pkgs--tablist-format-width sym))
        (buffer-width (length (npm-pkgs--tablist-get-value sym))))
    (+ (max format-width buffer-width) tabulated-list-padding)))

(defun npm-pkgs--tablist-width-index (index)
  "The same as `npm-pkgs--tablist-width' but passing INDEX instead of symbol."
  (npm-pkgs--tablist-width (npm-pkgs--tablist-symbol index)))

(defun npm-pkgs--tablist-column (sym)
  "Return column by SYM."
  (let ((cnt (npm-pkgs--tablist-index sym)) (index 0) (col tabulated-list-padding))
    (while (< index cnt)
      (setq col (+ col (npm-pkgs--tablist-width-index index)))
      (setq index (1+ index)))
    col))

(defun npm-pkgs--get-input ()
  "Get the input string."
  (substring tabulated-list--header-string
             (length npm-pkgs--title-prefix)
             (length tabulated-list--header-string)))

(defun npm-pkgs--refresh ()
  "Do refresh list."
  (when (get-buffer npm-pkgs--buffer-name)
    (with-current-buffer npm-pkgs--buffer-name
      (setq tabulated-list-entries (npm-pkgs--get-entries))
      (tabulated-list-revert)
      (tabulated-list-print-fake-header)
      (npm-pkgs--make-buttons))))

(defun npm-pkgs--input (key-input &optional add-del-num)
  "Insert key KEY-INPUT for fake header for input bar.
ADD-DEL-NUM : Addition or deletion number."
  (unless add-del-num (setq add-del-num (length key-input)))
  (if (< 0 add-del-num)
      (setq tabulated-list--header-string
            (concat tabulated-list--header-string key-input))
    (setq tabulated-list--header-string
          (substring tabulated-list--header-string 0 (1- (length tabulated-list--header-string)))))
  ;; NOTE: Ensure title exists.
  (when (> (length npm-pkgs--title-prefix) (length tabulated-list--header-string))
    (setq tabulated-list--header-string npm-pkgs--title-prefix))
  (tabulated-list-revert)
  (tabulated-list-print-fake-header)
  (when (timerp npm-pkgs--refresh-timer) (cancel-timer npm-pkgs--refresh-timer))
  (setq npm-pkgs--refresh-timer
        (run-with-timer npm-pkgs-delay nil #'npm-pkgs--confirm)))

(defun npm-pkgs--confirm ()
  "Confirm to search for npm packages."
  (interactive)
  (let ((input (npm-pkgs--get-input))) (npm-pkgs--search input)))

(defun npm-pkgs--get-entries ()
  "Get entries for `tabulated-list'."
  (progn  ; Initialize
    (with-current-buffer npm-pkgs--buffer
      (npm-pkgs--local-collect)))
  (let ((entries '()) (id-count 0))
    (mapc
     (lambda (item)
       (let ((new-entry '()) (new-entry-value '())
             (name (gethash "name" item))
             (version (gethash "version" item))
             (status "available")
             (description (gethash "description" item))
             (date (gethash "date" item))
             (publisher (gethash "publisher" item)))
         (setq publisher (gethash "username" publisher))
         (push date new-entry-value)  ; Date
         (push publisher new-entry-value)  ; Published
         (push description new-entry-value)  ; Description
         (push status new-entry-value)  ; Status
         (push version new-entry-value)  ; Version
         (push name new-entry-value)  ; Name
         ;; ---
         (push (vconcat new-entry-value) new-entry)  ; Turn into vector.
         (push (number-to-string id-count) new-entry)  ; ID
         (push new-entry entries)
         (setq id-count (1+ id-count))))
     npm-pkgs--data)
    (dolist (item npm-pkgs--local-packages)
      (let ((new-entry '()) (new-entry-value '())
            (name (plist-get item :name))
            (version (plist-get item :version))
            (status (plist-get item :status))
            (description "")
            (date "")
            (publisher ""))
        (push date new-entry-value)  ; Date
        (push publisher new-entry-value)  ; Published
        (push description new-entry-value)  ; Description
        (push status new-entry-value)  ; Status
        (push version new-entry-value)  ; Version
        (push name new-entry-value)  ; Name
        ;; ---
        (push (vconcat new-entry-value) new-entry)  ; Turn into vector.
        (push (number-to-string id-count) new-entry)  ; ID
        (push new-entry entries)
        (setq id-count (1+ id-count))))
    (reverse entries)))

(define-derived-mode npm-pkgs-mode tabulated-list-mode
  "npm-pkgs-mode"
  "Major mode for npm pkgs."
  :group 'npm-pkgs
  (setq tabulated-list-format npm-pkgs--tablist-format)
  (setq tabulated-list-padding 1)
  (setq tabulated-list--header-string npm-pkgs--title-prefix)
  (setq tabulated-list-sort-key (cons "Status" nil))
  (tabulated-list-init-header)
  (setq tabulated-list-entries (npm-pkgs--get-entries))
  (tabulated-list-print t)
  (tabulated-list-print-fake-header))

;;;###autoload
(defun npm-pkgs ()
  "Search npm packages."
  (interactive)
  (setq npm-pkgs--buffer (current-buffer))
  (pop-to-buffer npm-pkgs--buffer-name nil)
  (npm-pkgs-mode))

(provide 'npm-pkgs)
;;; npm-pkgs.el ends here