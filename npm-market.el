;;; npm-market.el --- A npm packages client  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-10-05 17:24:59

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: A npm packages client.
;; Keyword: npm packages client
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (request "0.3.0"))
;; URL: https://github.com/jcs-elpa/npm-market

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

(defgroup npm-market nil
  "A npm packages client."
  :prefix "npm-market-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/npm-market"))

(defconst npm-market-url "https://www.npmjs.com/search/"
  "npm packages url.")

(defconst npm-market--url-format (format "%ssuggestions?q=%%s" npm-market-url)
  "URL format to request NPM packages by search.")

(defvar npm-market--request nil
  "Store request object.")

(defvar npm-market--data nil
  "Use to store package list data.")

;;; Local

;;; Core

(defun npm-market-reset-request ()
  "Cancel the current request and set to initialized state."
  (when npm-market--request (request-abort npm-market--request))
  (setq npm-market--request nil))

(defun npm-market--search (text)
  "Search npm suggested packages list."
  (npm-market-reset-request)
  (setq npm-market--request
        (request
          (format npm-market--url-format text)
          :type "GET"
          :parser 'buffer-string
          :success
          (cl-function
           (lambda (&key data &allow-other-keys)
             (setq npm-market--data (json-parse-string data))
             (npm-market--refresh)
             (npm-market-reset-request)))
          :error
          ;; NOTE: Accept, error.
          (cl-function
           (lambda (&rest _args &key _error-thrown &allow-other-keys)
             (npm-market-reset-request))))))

;;; Button

(define-button-type 'npm-market--name-button
  'action 'npm-market--button-name)

(define-button-type 'npm-market--author-button
  'action 'npm-market--button-author)

(defun npm-market--button-name (_button)
  "Click on name."
  (let ((pkg-name (npm-market--tablist-get-value 'name)))
    (when pkg-name
      (browse-url (format "https://www.npmjs.com/package/%s" pkg-name)))))

(defun npm-market--button-author (_button)
  "Click on published."
  (let ((author (npm-market--tablist-get-value 'author)))
    (when author
      (browse-url (format "https://www.npmjs.com/~%s" author)))))

(defun npm-market--make-buttons ()
  "Make button to npm client."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (move-to-column 0)
      (let ((pkg-name (npm-market--tablist-get-value 'name))
            (author (npm-market--tablist-get-value 'author)))
        (when pkg-name
          (make-button (1+ (point)) (+ 1 (point) (length pkg-name)) :type 'npm-market--name-button)
          ;;(make-button (1+ (point)) (+ (point) (length author)) :type 'npm-market--author-button)
          ))
      (forward-line 1))))

;;; Buffer

(defconst npm-market--tablist-format
  (vector (list "Name" 15 t)
          (list "Version" 8 t)
          (list "Status" 10)
          (list "Description" 40 t)
          (list "Published" 15)
          (list "Date" 15 t))
  "Format to assign to `tabulated-list-format' variable.")

(defconst npm-market--buffer-name "*npm-market*"
  "Name of the buffer to display npm packages.")

(defconst npm-market--title-prefix "Search packages: "
  "Header put infront of the input string.")

(defcustom npm-market-delay 0.5
  "Input delay to refresh buffer."
  :type 'float
  :group 'npm-market)

(defconst npm-market--key-list
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
    "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
    "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" "`"
    "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "\\" "~"
    "{" "}" "[" "]" ";" ":" "'" "\"" "," "." "<" ">" "/"
    "?" "|" " ")
  "List of key to bind.")

(defvar-local npm-market--refresh-timer nil
  "Store refresh timer function.")

(defvar npm-market-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (key-str npm-market--key-list)
      (define-key map key-str
        (lambda () (interactive) (npm-market--input key-str))))
    (define-key map (kbd "<backspace>")
      (lambda () (interactive) (npm-market--input "" -1)))
    (define-key map (kbd "<return>") #'npm-market--confirm)
    map)
  "Kaymap for `npm-market-mode'.")

(defun npm-market--tablist-index (sym)
  "Return index id by SYM."
  (cl-case sym
    (name 0) (version 1) (status 2) (description 3) (author 4) (date 5)))

(defun npm-market--tablist-get-value (sym)
  "Get value by SYM."
  (let ((entry (tabulated-list-get-entry)) idx)
    (when entry (setq idx (npm-market--tablist-index sym)))
    (if entry (aref entry idx) nil)))

(defun npm-market--tablist-width (sym)
  "Return the width for SYM in `npm-market--tablist-format'."
  (nth 1 (aref npm-market--tablist-format (npm-market--tablist-index sym))))

(defun npm-market--tablist-column (sym)
  ""
  (let ((col tabulated-list-padding))
    (mapcar
     (lambda (item)
       (setq col (+ col (nth 1 item))))
     npm-market--tablist-format)
    col))

(defun npm-market--get-input ()
  "Get the input string."
  (substring tabulated-list--header-string
             (length npm-market--title-prefix)
             (length tabulated-list--header-string)))

(defun npm-market--refresh ()
  "Do refresh list."
  (when (get-buffer npm-market--buffer-name)
    (with-current-buffer npm-market--buffer-name
      (setq tabulated-list-entries (npm-market--get-entries))
      (tabulated-list-revert)
      (tabulated-list-print-fake-header)
      (npm-market--make-buttons))))

(defun npm-market--input (key-input &optional add-del-num)
  "Insert key KEY-INPUT for fake header for input bar.
ADD-DEL-NUM : Addition or deletion number."
  (unless add-del-num (setq add-del-num (length key-input)))
  (if (< 0 add-del-num)
      (setq tabulated-list--header-string
            (concat tabulated-list--header-string key-input))
    (setq tabulated-list--header-string
          (substring tabulated-list--header-string 0 (1- (length tabulated-list--header-string)))))
  ;; NOTE: Ensure title exists.
  (when (> (length npm-market--title-prefix) (length tabulated-list--header-string))
    (setq tabulated-list--header-string npm-market--title-prefix))
  (tabulated-list-revert)
  (tabulated-list-print-fake-header))

(defun npm-market--confirm ()
  "Confirm to search for npm packages."
  (interactive)
  (let ((input (npm-market--get-input)))
    (unless (string-empty-p input) (npm-market--search input))))

(defun npm-market--get-entries ()
  "Get entries for `tabulated-list'."
  (let ((entries '()) (id-count 0))
    (mapcar
     (lambda (item)
       (let ((new-entry '()) (new-entry-value '())
             (name (gethash "name" item))
             (version (gethash "version" item))
             (status "available")
             (description (gethash "description" item))
             (date (gethash "date" item))
             (author (gethash "author" item)))
         (setq author (gethash "name" author))
         (push date new-entry-value)  ; Date
         (push author new-entry-value)  ; Published
         (push description new-entry-value)  ; Description
         (push status new-entry-value)  ; Status
         (push version new-entry-value)  ; Version
         (push name new-entry-value)  ; Name
         ;; ---
         (push (vconcat new-entry-value) new-entry)  ; Turn into vector.
         (push (number-to-string id-count) new-entry)  ; ID
         (push new-entry entries)
         (setq id-count (1+ id-count))))
     npm-market--data)
    (reverse entries)))

(define-derived-mode npm-market-mode tabulated-list-mode
  "npm-market-mode"
  "Major mode for npm market."
  :group 'npm-market
  (setq tabulated-list-format npm-market--tablist-format)
  (setq tabulated-list-padding 1)
  (setq tabulated-list--header-string npm-market--title-prefix)
  (setq tabulated-list-sort-key (cons "Status" nil))
  (tabulated-list-init-header)
  ;;(setq tabulated-list-entries (npm-market--get-entries))
  (setq tabulated-list-entries nil)
  (tabulated-list-print t)
  (tabulated-list-print-fake-header))

;;;###autoload
(defun npm-market ()
  "Search npm packages."
  (interactive)
  (pop-to-buffer npm-market--buffer-name nil)
  (npm-market-mode))

(provide 'npm-market)
;;; npm-market.el ends here
