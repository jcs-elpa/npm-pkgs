;;; npm-pkgs.el --- An npm packages client  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-10-05 17:24:59

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: An npm packages client.
;; Keyword: npm packages client
;; Version: 1.0.0
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
;; An npm packages client.
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'browse-url)
(require 'button)

(require 'dom)
(require 'request)
(require 's)

(defgroup npm-pkgs nil
  "An npm packages client."
  :prefix "npm-pkgs-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/npm-pkgs"))

(defconst npm-pkgs-url "https://www.npmjs.com/search/"
  "Npm packages url.")

(defconst npm-pkgs--url-format (format "%ssuggestions?q=%%s" npm-pkgs-url)
  "URL format to request NPM packages by search.")

(defvar npm-pkgs--version ""
  "Store npm version.")

(defvar npm-pkgs--request nil
  "Store request object.")

(defvar npm-pkgs--data nil
  "Use to store package list data.")

(defvar npm-pkgs--buffer nil
  "Record the buffer that calls `npm-pkgs'.")

(defvar npm-pkgs--all-entries nil
  "List of all entries.")

;;; Util

(defun npm-pkgs--project-roort ()
  "Return project root path."
  (cdr (project-current)))

(defun npm-pkgs--async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the background.

Return the temporary output buffer which command is writing to during execution.

When the command is finished, call CALLBACK with the resulting output as a string."
  (lexical-let
      ((output-buffer (generate-new-buffer " *temp*"))
       (callback-fun callback))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process _signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties (point-min) (point-max))))
             (funcall callback-fun output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(defun npm-pkgs--collect (output global)
  "Collect data from shell OUTPUT.

If argument GLOBAL is no-nil, we find global packages instead of local packages."
  (let ((ln-str (split-string output "\n")) (result '())
        sec-lst pkg-name pkg-version str-len ver-len)
    (dolist (ln ln-str)
      (when (and (string-match-p "[+`]-- " ln)
                 (not (string-match-p "UNMET" ln))
                 (not (string-match-p "(empty)" ln)))
        (setq ln (s-replace-regexp "[+`]-- " "" ln)
              sec-lst (split-string ln "@" t)
              str-len (length ln)
              ver-len (length sec-lst)
              pkg-version (nth (1- ver-len) sec-lst)
              pkg-name (substring ln 0 (- str-len (length pkg-version) 1)))
        (push (list :name pkg-name :version pkg-version
                    :status (if global "global" "workspace"))
              result)))
    result))

;;; Global

(defconst npm-pkgs--cmd-list-pkgs-global "npm list -g --depth 0"
  "List of global packages.")

(defvar npm-pkgs--global-packages nil
  "List of global packages.")

(defvar npm-pkgs--global-processing-p nil
  "Flag to see if we are currently getting global packages information.")

(defun npm-pkgs--global-collect ()
  "Collect global package data."
  (unless npm-pkgs--global-processing-p
    (setq npm-pkgs--global-processing-p t)
    (npm-pkgs--async-shell-command-to-string
     npm-pkgs--cmd-list-pkgs-global
     (lambda (output)
       (let ((result (npm-pkgs--collect output t)))
         (unless (equal npm-pkgs--global-packages result)
           (setq npm-pkgs--global-packages result)
           (npm-pkgs--refresh)
           (setq npm-pkgs--global-processing-p nil)))))))

;;; Local

(defconst npm-pkgs--cmd-list-pkgs-local "npm list --depth 0"
  "List of global packages.")

(defvar npm-pkgs--local-packages nil
  "List of local packages.")

(defvar npm-pkgs--local-processing-p nil
  "Flag to see if we are currently getting local packages information.")

(defun npm-pkgs--local-collect ()
  "Collect local package data."
  (unless npm-pkgs--local-processing-p
    (setq npm-pkgs--local-processing-p t)
    (npm-pkgs--async-shell-command-to-string
     npm-pkgs--cmd-list-pkgs-local
     (lambda (output)
       (let ((result (npm-pkgs--collect output nil)))
         (unless (equal npm-pkgs--local-packages result)
           (setq npm-pkgs--local-packages result)
           (npm-pkgs--refresh)
           (setq npm-pkgs--local-processing-p nil)))))))

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

(defun npm-pkgs--beg-column ()
  "Return beginning position of the column point."
  (save-excursion
    (search-backward " " (line-beginning-position) t)
    (forward-char 1)
    (point)))

(defun npm-pkgs--end-symbol ()
  "Return point at the end of symbol."
  (save-excursion
    (re-search-forward "[ \t\r\n]" nil t)
    (forward-char -1)
    (point)))

(defun npm-pkgs--make-buttons ()
  "Make button to npm client."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((name-pkg (npm-pkgs--tablist-get-value 'name)))
        (when name-pkg
          (move-to-column (npm-pkgs--tablist-column 'name))
          (make-button (npm-pkgs--beg-column)
                       (npm-pkgs--end-symbol)
                       :type 'npm-pkgs--name-button)
          (move-to-column (npm-pkgs--tablist-column 'author))
          (when (thing-at-point 'symbol)
            (make-button (npm-pkgs--beg-column)
                         (npm-pkgs--end-symbol)
                         :type 'npm-pkgs--author-button))))
      (forward-line 1))))

;;; Functions

(defconst npm-pkgs--cmd-install-global "npm install -g %s"
  "Command to install package for global use.")
(defconst npm-pkgs--cmd-install-local "npm install %s"
  "Command to install a package.")
(defconst npm-pkgs--cmd-install-dev "npm install --save-dev %s"
  "Command to install package as devDependency.")

(defconst npm-pkgs--cmd-uninstall-global "npm uninstall -g %s"
  "Command to uninstall a global installed package.")
(defconst npm-pkgs--cmd-uninstall-local "npm uninstall %s"
  "Command to uninstall a local installed package.")
(defconst npm-pkgs--cmd-uninstall-dev "npm uninstall -D %s"
  "Command to uninstall a devDependency installed package.")

(defconst npm-pkgs--cmd-update-global "npm upgrade -g"
  "Command to upgrad global installed packages.")
(defconst npm-pkgs--cmd-update-local "npm upgrade"
  "Command to upgrade production packages.")
(defconst npm-pkgs--cmd-update-dev "npm upgrade --dev"
  "Command to upgrade devDependency package.")

(defvar npm-pkgs--executing-p nil
  "Flag to see if we are currently executing commands.")

(defvar npm-pkgs--count-installed nil
  "Count installed packages.")

(defvar npm-pkgs--count-uninstalled nil
  "Count installed packages.")

(defun npm-pkgs-upgrade-all ()
  "Upgrade all installed packages."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to upgrade all packages? ")
    (npm-pkgs--async-shell-command-to-string
     (npm-pkgs--update-command 'local)
     (lambda (output) (message "[npm-pkgs::production] %s" output)))
    (npm-pkgs--async-shell-command-to-string
     (npm-pkgs--update-command 'dev)
     (lambda (output) (message "[npm-pkgs::dev] %s" output)))
    (npm-pkgs--async-shell-command-to-string
     (npm-pkgs--update-command 'global)
     (lambda (output) (message "[npm-pkgs::global] %s" output)))))

(defvar npm-pkgs--entry-table nil
  "Store entries that have been marked.")

(defun npm-pkgs--symbol-to-tag (sym)
  "Convert SYM to tag."
  (cl-case sym (global "G") (local "L") (dev "V") (delete "D")))

(defun npm-pkgs--get-tag ()
  "Get current tag."
  (let ((tag (substring (thing-at-point 'line) 0 1)))
    (setq tag (string-trim tag))
    (if (string-empty-p tag) nil tag)))

(defun npm-pkgs--push-entry (tag &optional entry)
  "Push ENTRY by TAG."
  (unless entry (setq entry (tabulated-list-get-entry)))
  (push entry (gethash (intern tag) npm-pkgs--entry-table)))

(defun npm-pkgs--remove-entry (tag &optional entry)
  "Remove ENTRY by TAG."
  (unless entry (setq entry (tabulated-list-get-entry)))
  (when (stringp tag) (setq tag (intern tag)))
  (setf (gethash tag npm-pkgs--entry-table)
        (remove entry (gethash tag npm-pkgs--entry-table))))

(defun npm-pkgs--clean-hash (&optional tag)
  "Clean the hash by TAG.
If TAG is nil; clean all instead."
  (if tag (delete-dups (gethash (intern tag) npm-pkgs--entry-table))
    (dolist (tag (hash-table-keys npm-pkgs--entry-table))
      (delete-dups (gethash tag npm-pkgs--entry-table)))))

(defun npm-pkgs-unmark-mark ()
  "Mark or Unmark package."
  (interactive)
  (let ((tag (npm-pkgs--get-tag)))
    (when tag (npm-pkgs--remove-entry tag)))
  (tabulated-list-put-tag ""))

(defun npm-pkgs-mark-install ()
  "Mark install package."
  (interactive)
  (let ((status (npm-pkgs--tablist-get-value 'status)) tag)
    (when (string= status "available")
      (setq tag (npm-pkgs--symbol-to-tag
                 (npm-pkgs--ask-install-pkg (npm-pkgs--tablist-get-value 'name))))
      (tabulated-list-put-tag tag)
      (npm-pkgs--push-entry tag)
      (npm-pkgs--clean-hash tag))))

(defun npm-pkgs-mark-delete ()
  "Mark delete package."
  (interactive)
  (let ((status (npm-pkgs--tablist-get-value 'status))
        (tag (npm-pkgs--symbol-to-tag 'delete)))
    (when (or (string= status "workspace") (string= status "global"))
      (tabulated-list-put-tag tag)
      (npm-pkgs--push-entry tag)
      (npm-pkgs--clean-hash tag))))

(defun npm-pkgs-execute ()
  "Execute all marked packages."
  (interactive)
  (unless npm-pkgs--executing-p
    (setq npm-pkgs--executing-p t)
    (npm-pkgs--clean-hash)
    (dolist (tag (hash-table-keys npm-pkgs--entry-table))
      (dolist (entry (gethash tag npm-pkgs--entry-table))
        (let ((pkg-name (npm-pkgs--tablist-get-value 'name entry))
              (status (npm-pkgs--tablist-get-value 'status entry)))
          (cl-case tag
            (D (npm-pkgs--uninstall-pkg pkg-name
                                        (cond ((string= status "workspace") 'local)
                                              ((string= status "global") 'global))
                                        tag entry))
            (L (npm-pkgs--install-pkg pkg-name 'local tag entry))
            (G (npm-pkgs--install-pkg pkg-name 'global tag entry))
            (V (npm-pkgs--install-pkg pkg-name 'dev tag entry))))))))

(defun npm-pkgs--ask-install-pkg (pkg-name)
  "Ask to mark install package by PKG-NAME."
  (let ((answer (completing-read
                 (format "Install package (%s): " pkg-name)
                 '("workspace" "globally" "development"))))
    (cond ((string= answer "globally") 'global)
          ((string= answer "workspace") 'local)
          ((string= answer "development") 'dev))))

(defun npm-pkgs--install-command (type)
  "Return proper install command by TYPE."
  (cl-case type
    (global npm-pkgs--cmd-install-global)
    (local npm-pkgs--cmd-install-local)
    (dev npm-pkgs--cmd-install-dev)))

(defun npm-pkgs--uninstall-command (type)
  "Return proper uninstall command by TYPE."
  (cl-case type
    (global npm-pkgs--cmd-uninstall-global)
    (local npm-pkgs--cmd-uninstall-local)
    (dev npm-pkgs--cmd-uninstall-dev)))

(defun npm-pkgs--update-command (type)
  "Return proper update command by TYPE."
  (cl-case type
    (global npm-pkgs--cmd-update-global)
    (local npm-pkgs--cmd-update-local)
    (dev npm-pkgs--cmd-update-dev)))

(defun npm-pkgs--install-pkg (pkg-name type tag entry)
  "Install package by PKG-NAME and TYPE.
Arguments TAG and ENTRY are for searching entry table."
  (message "[NPM-PKGS] Installing package `%s` as `%s` dependency" pkg-name type)
  (npm-pkgs--async-shell-command-to-string
   (format (npm-pkgs--install-command type) pkg-name)
   (lambda (_output)
     (message "[NPM-PKGS] Installed package %s" pkg-name)
     (setq npm-pkgs--count-installed (1+ npm-pkgs--count-installed))
     (npm-pkgs--remove-entry tag entry)
     (npm-pkgs--end-execution))))

(defun npm-pkgs--uninstall-pkg (pkg-name type tag entry)
  "Uninstall package by PKG-NAME and TYPE.
Arguments TAG and ENTRY are for searching entry table."
  (message "[NPM-PKGS] Uninstalling package `%s` from `%s` dependency" pkg-name type)
  (npm-pkgs--async-shell-command-to-string
   (format (npm-pkgs--uninstall-command type) pkg-name)
   (lambda (_output)
     (message "[NPM-PKGS] Uninstalled package %s" pkg-name)
     (setq npm-pkgs--count-uninstalled (1+ npm-pkgs--count-uninstalled))
     (npm-pkgs--remove-entry tag entry)
     (npm-pkgs--end-execution))))

(defun npm-pkgs--table-empty-p ()
  "Check if entry table empty."
  (let ((empty t))
    (dolist (tag (hash-table-keys npm-pkgs--entry-table))
      (when (< 0 (length (gethash tag npm-pkgs--entry-table)))
        (setq empty nil)))
    empty))

(defun npm-pkgs--end-execution ()
  "Callback to see if end the execution."
  (when (npm-pkgs--table-empty-p)
    (setq npm-pkgs--executing-p nil)
    (npm-pkgs--refresh t)
    (message "[NPM-PKGS] Done execution; %s package%s installed, %s package%s removed"
             npm-pkgs--count-installed
             (if (<= npm-pkgs--count-installed 1) "" "s")
             npm-pkgs--count-uninstalled
             (if (<= npm-pkgs--count-uninstalled 1) "" "s"))
    (setq npm-pkgs--count-installed 0
          npm-pkgs--count-uninstalled 0)))

;;; Buffer

(defconst npm-pkgs--tablist-format
  (vector (list "Name" 20 t)
          (list "Version" 8 t)
          (list "Status" 10)
          (list "Description" 50 t)
          (list "Published" 15))
  "Format to assign to `tabulated-list-format' variable.")

(defconst npm-pkgs--buffer-name "*npm-pkgs*"
  "Name of the buffer to display npm packages.")

(defvar npm-pkgs--title-prefix "Keywords: "
  "Header put infront of the input string.")

(defcustom npm-pkgs-delay 0.5
  "Input delay to refresh buffer."
  :type 'float
  :group 'npm-pkgs)

(defconst npm-pkgs--key-list
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
    "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" "`"
    "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "\\" "~"
    "{" "}" "[" "]" ";" ":" "'" "\"" "," "." "<" ">" "/"
    "?" "|" " ")
  "List of key to bind.")

(defvar-local npm-pkgs--refresh-timer nil
  "Store refresh timer function.")

(defvar npm-pkgs--tablist-id 0
  "Tabulated List Id.")

(defvar npm-pkgs--tablist-refresh-p nil
  "Check if currently tablist refreshing.")

(defvar npm-pkgs-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (key-str npm-pkgs--key-list)
      (define-key map key-str
        (lambda () (interactive) (npm-pkgs--input key-str))))
    (define-key map (kbd "<backspace>")
      (lambda () (interactive) (npm-pkgs--input "" -1)))
    (define-key map (kbd "U") #'npm-pkgs-upgrade-all)
    (define-key map (kbd "<delete>") #'npm-pkgs-unmark-mark)
    (define-key map (kbd "I") #'npm-pkgs-mark-install)
    (define-key map (kbd "D") #'npm-pkgs-mark-delete)
    (define-key map (kbd "X") #'npm-pkgs-execute)
    map)
  "Kaymap for `npm-pkgs-mode'.")

(defun npm-pkgs--get-title-prefix ()
  "Return the full title prefix."
  (when (string-empty-p npm-pkgs--version)
    (setq npm-pkgs--version "??.??.??")  ; NOTE: Set to something else than empty
    (npm-pkgs--async-shell-command-to-string
     "npm --version"
     (lambda (output)
       (setq npm-pkgs--version (string-trim output))
       (setq tabulated-list--header-string (npm-pkgs--get-title-prefix))
       (tabulated-list-print-fake-header))))
  (format "[npm %s] %s" npm-pkgs--version npm-pkgs--title-prefix))

(defun npm-pkgs--tablist-index (sym)
  "Return index id by SYM."
  (cl-case sym
    (name 0) (version 1) (status 2) (description 3) (author 4)))

(defun npm-pkgs--tablist-symbol (index)
  "Return symbol id by INDEX."
  (cl-case index
    (0 'name) (1 'version) (2 'status) (3 'description) (4 'author)))

(defun npm-pkgs--tablist-get-value (sym &optional entry)
  "Get value by SYM.
If optional argument ENTRY is nil; use current entry at point instead."
  (unless entry (setq entry (tabulated-list-get-entry)))
  (let (idx)
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
             (length (npm-pkgs--get-title-prefix))
             (length tabulated-list--header-string)))

(defun npm-pkgs--refresh (&optional hl)
  "Do refresh list.
If optional argument HL is non-nil; do make buttons."
  (when (get-buffer npm-pkgs--buffer-name)
    (with-current-buffer npm-pkgs--buffer-name
      (setq tabulated-list-entries (npm-pkgs--get-entries))
      (tabulated-list-revert)
      (tabulated-list-print-fake-header)
      (when (or npm-pkgs--tablist-refresh-p hl)
        (npm-pkgs--make-buttons)
        (setq npm-pkgs--tablist-refresh-p nil)))))

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
  (when (> (length (npm-pkgs--get-title-prefix)) (length tabulated-list--header-string))
    (setq tabulated-list--header-string (npm-pkgs--get-title-prefix)))
  (tabulated-list-revert)
  (tabulated-list-print-fake-header)
  (npm-pkgs--make-buttons)
  (when (timerp npm-pkgs--refresh-timer) (cancel-timer npm-pkgs--refresh-timer))
  (setq npm-pkgs--refresh-timer
        (run-with-idle-timer npm-pkgs-delay nil #'npm-pkgs--confirm)))

(defun npm-pkgs--confirm ()
  "Confirm to search for npm packages."
  (interactive)
  (let ((input (npm-pkgs--get-input))) (npm-pkgs--search input)))

(defun npm-pkgs--market-entries ()
  "Return the market entries."
  (let ((entries '()))
    (mapc
     (lambda (item)
       (let ((new-entry '()) (new-entry-value '())
             (name (gethash "name" item))
             (version (gethash "version" item))
             (status "available")
             (description (gethash "description" item))
             (publisher (gethash "publisher" item)))
         (setq publisher (gethash "username" publisher))
         (push publisher new-entry-value)  ; Published
         (push description new-entry-value)  ; Description
         (push status new-entry-value)  ; Status
         (push version new-entry-value)  ; Version
         (push name new-entry-value)  ; Name
         ;; ---
         (push (vconcat new-entry-value) new-entry)  ; Turn into vector.
         (push (number-to-string npm-pkgs--tablist-id) new-entry)  ; ID
         (push new-entry entries)
         (setq npm-pkgs--tablist-id (1+ npm-pkgs--tablist-id))))
     npm-pkgs--data)
    (reverse entries)))

(defun npm-pkgs--machine-entries (pkg-lst)
  "Get list of entries depends on PKG-LST."
  (let ((entries '()))
    (dolist (item pkg-lst)
      (let ((new-entry '()) (new-entry-value '())
            (name (plist-get item :name))
            (version (plist-get item :version))
            (status (plist-get item :status))
            (description "")
            (publisher ""))
        (setq version (propertize version 'face (list :inherit font-lock-comment-face)))
        (setq status (propertize status 'face (list :inherit font-lock-comment-face)))
        (push publisher new-entry-value)  ; Published
        (push description new-entry-value)  ; Description
        (push status new-entry-value)  ; Status
        (push version new-entry-value)  ; Version
        (push name new-entry-value)  ; Name
        ;; ---
        (push (vconcat new-entry-value) new-entry)  ; Turn into vector.
        (push (number-to-string npm-pkgs--tablist-id) new-entry)  ; ID
        (push new-entry entries)
        (setq npm-pkgs--tablist-id (1+ npm-pkgs--tablist-id))))
    entries))

(defun npm-pkgs--global-entries ()
  "Get list of global packages entries."
  (with-current-buffer npm-pkgs--buffer (npm-pkgs--global-collect))
  (npm-pkgs--machine-entries npm-pkgs--global-packages))

(defun npm-pkgs--local-entries ()
  "Get list of local packages entries."
  (with-current-buffer npm-pkgs--buffer (npm-pkgs--local-collect))
  (npm-pkgs--machine-entries npm-pkgs--local-packages))

(defun npm-pkgs--get-entries ()
  "Get entries for `tabulated-list'."
  (setq npm-pkgs--tablist-id 0
        npm-pkgs--tablist-refresh-p nil)
  (let ((new-entries (append (npm-pkgs--market-entries)
                             (npm-pkgs--local-entries)
                             (npm-pkgs--global-entries))))
    (unless (equal npm-pkgs--all-entries new-entries)
      (setq npm-pkgs--all-entries new-entries
            npm-pkgs--tablist-refresh-p t)))
  npm-pkgs--all-entries)

(define-derived-mode npm-pkgs-mode tabulated-list-mode
  "npm-pkgs-mode"
  "Major mode for npm pkgs."
  :group 'npm-pkgs
  (setq tabulated-list-format npm-pkgs--tablist-format
        tabulated-list-padding 2
        tabulated-list--header-string (npm-pkgs--get-title-prefix)
        tabulated-list-sort-key (cons "Status" nil))
  (tabulated-list-init-header)
  (setq tabulated-list-entries (npm-pkgs--get-entries))
  (tabulated-list-print t)
  (tabulated-list-print-fake-header)
  (npm-pkgs--make-buttons))

;;;###autoload
(defun npm-pkgs ()
  "Search npm packages."
  (interactive)
  (setq npm-pkgs--buffer (current-buffer)
        npm-pkgs--data nil
        npm-pkgs--version ""
        npm-pkgs--entry-table (make-hash-table)
        npm-pkgs--executing-p nil
        npm-pkgs--count-installed 0
        npm-pkgs--count-uninstalled 0)
  (setq npm-pkgs--global-processing-p nil
        npm-pkgs--local-processing-p nil
        npm-pkgs--global-packages nil
        npm-pkgs--local-packages nil)
  (pop-to-buffer npm-pkgs--buffer-name nil)
  (npm-pkgs-mode))

(provide 'npm-pkgs)
;;; npm-pkgs.el ends here
