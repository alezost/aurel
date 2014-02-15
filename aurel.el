;;; aurel.el --- Search, get info and download AUR packages

;; Copyright (C) 2014 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 6 Feb 2014
;; Version: 0.1.1
;; URL: http://github.com/alezost/aurel
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The package provides an interface for searching, getting information
;; and downloading packages from the Arch User Repository (AUR)
;; <https://aur.archlinux.org/>.

;; To manually install the package, add the following to your init-file:
;;
;;   (add-to-list 'load-path "/path/to/aurel-dir")
;;   (autoload 'aurel-package-info "aurel" nil t)
;;   (autoload 'aurel-package-search "aurel" nil t)
;;   (autoload 'aurel-maintainer-search "aurel" nil t)

;; Also set a directory where downloaded packages will be put:
;;
;;   (setq aurel-download-directory "~/aur")

;; To search for packages, use `aurel-package-search' or
;; `aurel-maintainer-search' commands.  If you know the name of a
;; package, use `aurel-package-info' command.

;; Information about the packages is represented in a list-like buffer
;; similar to a buffer containing emacs packages.  To get more info
;; about a package, press "RET" on a package line.  To download a
;; package, press "d" (don't forget to set `aurel-download-directory'
;; before).

;; For full description, see <http://github.com/alezost/aurel>.

;;; Code:

(require 'url-expand)
(require 'url-handlers)
(require 'json)
(require 'tabulated-list)

(defgroup aurel nil
  "Search and download AUR (Arch User Repository) packages."
  :group 'applications)


;;; Backend

(defvar aurel-base-url "https://aur.archlinux.org/"
  "Root URL of the AUR service.")

;; Avoid compilation warning about `url-http-response-status'
(defvar url-http-response-status)

(defun aurel-receive-parse-info (url)
  "Return received output from URL processed with `json-read'."
  (let ((buf (url-retrieve-synchronously url)))
    (with-current-buffer buf
      (if (or (null (numberp url-http-response-status))
              (> url-http-response-status 299))
          (error "Error during request: %s" url-http-response-status))
      (goto-char (point-min))
      (search-forward-regexp "^{") ;; is there a better way?
      (beginning-of-line)
      (let ((json-key-type 'string)
            (json-array-type 'list)
            (json-object-type 'alist))
        (json-read)))))

(defun aurel-receive-packages-info (url)
  "Return information about the packages from URL.

Output from URL should be a json data.  It is parsed with
`json-read', then information about the packages is passed
through `aurel-apply-filters'.

Returning value is alist, each element of which has a form:

  (ID . INFO)

ID is a package id (number).
INFO is alist of package parameters and values (see `aurel-info')."
  (let* ((full-info (aurel-receive-parse-info url))
         (type      (cdr (assoc "type" full-info)))
         (count     (cdr (assoc "resultcount" full-info)))
         (results   (cdr (assoc "results" full-info))))
    (cond
     ((string= type "error")
      (error "%s" results))
     ((= count 0)
      nil)
     (t
      (when (string= type "info")
        (setq results (list results)))
      (mapcar (lambda (info)
                (let ((info (aurel-apply-filters info)))
                  (cons (aurel-get-param-val 'id info)
                        info)))
              results)))))


;;; Package parameters

(defvar aurel-param-alist
  '((pkg-url     . "URLPath")
    (home-url    . "URL")
    (last-date   . "LastModified")
    (first-date  . "FirstSubmitted")
    (outdated    . "OutOfDate")
    (votes       . "NumVotes")
    (license     . "License")
    (description . "Description")
    (category    . "CategoryID")
    (version     . "Version")
    (name        . "Name")
    (id          . "ID")
    (maintainer  . "Maintainer"))
  "Association list of symbols and names of package info parameters.
Car of each assoc is a symbol used in code of this package.
Cdr - is a parameter name (string) returned by the server.")

(defvar aurel-param-description-alist
  '((pkg-url     . "Download URL")
    (home-url    . "Home Page")
    (aur-url     . "AUR Page")
    (last-date   . "Last Modified")
    (first-date  . "Submitted")
    (outdated    . "Out Of Date")
    (votes       . "Votes")
    (license     . "License")
    (description . "Description")
    (category    . "Category")
    (version     . "Version")
    (name        . "Name")
    (id          . "ID")
    (maintainer  . "Maintainer"))
  "Association list of symbols and descriptions of parameters.
Descriptions are used for displaying package information.
Symbols are either from `aurel-param-alist' or are added by
filter functions.  See `aurel-apply-filters' for details.")

(defun aurel-get-param-name (param-symbol)
  "Return a name of a parameter PARAM-SYMBOL."
  (cdr (assoc param-symbol aurel-param-alist)))

(defun aurel-get-param-symbol (param-name)
  "Return a symbol of a parameter PARAM-NAME."
  (car (rassoc param-name aurel-param-alist)))

(defun aurel-get-param-description (param-symbol)
  "Return a description of a parameter PARAM-SYMBOL."
  (cdr (assoc param-symbol aurel-param-description-alist)))

(defun aurel-get-param-val (param info)
  "Return a value of a parameter PARAM from a package INFO."
  (cdr (assoc param info)))


;;; Filters for processing package info

(defcustom aurel-empty-string "(None)"
  "String used for empty values of package parameters."
  :type 'string
  :group 'aurel)

(defcustom aurel-date-format "%Y-%m-%d %T"
  "Time format used to represent submit and last dates of a package.
For information about time formats, see `format-time-string'."
  :type 'string
  :group 'aurel)

(defvar aurel-categories
  [nil "None" "daemons" "devel" "editors"
       "emulators" "games" "gnome" "i18n" "kde" "lib"
       "modules" "multimedia" "network" "office"
       "science" "system" "x11" "xfce" "kernels" "fonts"]
  "Vector of package categories.
Index of an element is a category ID.")

(defvar aurel-filters
  '(aurel-filter-intern aurel-filter-empty aurel-filter-date
    aurel-filter-outdated aurel-filter-category
    aurel-filter-pkg-url aurel-filter-aur-url)
  "List of default filter functions applied to a package INFO.

Each filter function should accept a single argument - info alist
with package parameters and should return info alist.  Functions
may modify associations or add the new ones to the alist.  In the
latter case you might want to add descriptions of the added
symbols into `aurel-param-description-alist'.

`aurel-filter-intern' should be the first symbol in the list as
other filters use symbols for working with info parameters (see
`aurel-param-alist').")

(defun aurel-apply-filters (info &optional filters)
  "Apply functions from FILTERS list to a package INFO.

INFO is alist with package parameters.  It is passed as an
argument to the first function from FILTERS, the returned result
is passed to the second function from that list and so on.

If FILTERS is nil, use `aurel-filters'.

Return filtered info (result of the last filter)."
  (or filters
      (setq filters aurel-filters))
  (mapc (lambda (fun)
          (setq info (funcall fun info)))
        filters)
  info)

(defun aurel-filter-intern (info)
  "Replace names of parameters with symbols in a package INFO.
INFO is alist of parameter names (strings) and values.
Return modified info.
For names and symbols of parameters, see `aurel-param-alist'."
  (delq nil
        (mapcar
         (lambda (param)
           (let* ((param-name (car param))
                  (param-symbol (aurel-get-param-symbol param-name))
                  (param-val (cdr param)))
             (if param-symbol
                 (cons param-symbol param-val)
               (message "Warning: unknown parameter '%s'. It will be omitted."
                        param-name)
               nil)))
         info)))

(defun aurel-filter-empty (info)
  "Replace nil in parameter values with `aurel-empty-string' in a package INFO.
INFO is alist of parameter names (strings) and values.
Return modified info."
  (dolist (param info info)
    (unless (cdr param)
      (setcdr param aurel-empty-string))))

(defun aurel-filter-date (info)
  "Format date parameters of a package INFO with `aurel-date-format'.
INFO is alist of parameter symbols and values.
Return modified info."
  (dolist (param info info)
    (let ((param-name (car param))
          (param-val  (cdr param)))
      (when (or (equal param-name 'first-date)
                (equal param-name 'last-date))
        (setcdr param
                (format-time-string aurel-date-format
                                    (seconds-to-time param-val)))))))

(defun aurel-filter-outdated (info)
  "Replace 1/0 with Yes/No in `outdated' parameter of a package INFO.
INFO is alist of parameter symbols and values.
Return modified info."
  (let ((param (assoc 'outdated info)))
    (setcdr param (if (= 0 (cdr param)) "No" "Yes")))
  info)

(defun aurel-filter-category (info)
  "Replace category ID with category name in a package INFO.
INFO is alist of parameter symbols and values.
Return modified info."
  (let ((param (assoc 'category info)))
    (setcdr param (aref aurel-categories (cdr param))))
  info)

(defun aurel-filter-pkg-url (info)
  "Update `pkg-url' parameter in a package INFO.
INFO is alist of parameter symbols and values.
Return modified info."
  (let ((param (assoc 'pkg-url info)))
    (setcdr param (url-expand-file-name (cdr param) aurel-base-url)))
  info)

(defun aurel-filter-aur-url (info)
  "Add `aur-url' parameter to a package INFO.
INFO is alist of parameter symbols and values.
Return modified info."
  (add-to-list
   'info
   (cons 'aur-url
         (url-expand-file-name
          (concat "packages/" (aurel-get-param-val 'name info))
          aurel-base-url))))


;;; Downloading

(defcustom aurel-download-directory "/tmp"
  "Default directory for downloading AUR packages."
  :type 'directory
  :group 'aurel)

(defcustom aurel-directory-prompt "Download to: "
  "Default directory prompt for downloading AUR packages."
  :type 'string
  :group 'aurel)

(defun aurel-download (url dir)
  "Download AUR package from URL to a directory DIR.
Return a path to the downloaded file."
  ;; FIXME this is redundant; is there a simple way to download a file?
  (let* ((file-name-handler-alist
          (cons (cons url-handler-regexp 'url-file-handler)
                file-name-handler-alist))
         (buf (find-file-noselect url nil 'raw)))
    (with-current-buffer buf
      (let ((file (expand-file-name (url-file-nondirectory url) dir)))
        (write-file file)
        file))))

;; Code for working with `tar-mode' came from `package-untar-buffer'

;; Avoid compilation warnings about tar functions and variables
(defvar tar-parse-info)
(defvar tar-data-buffer)
(declare-function tar-data-swapped-p "tar-mode" ())
(declare-function tar-untar-buffer "tar-mode" ())
(declare-function tar-header-name "tar-mode" (tar-header) t)
(declare-function tar-header-link-type "tar-mode" (tar-header) t)

(defun aurel-download-unpack (url dir)
  "Download AUR package from URL and unpack it into a directory DIR.

Use `tar-untar-buffer' from Tar mode.  All files should be placed
in one directory; otherwise, signal an error.

Return a path to the unpacked directory."
  (let* ((file-name-handler-alist
          (cons (cons url-handler-regexp 'url-file-handler)
                file-name-handler-alist))
         (buf (find-file-noselect url)))
    (with-current-buffer buf
      (let ((file (expand-file-name (url-file-nondirectory url) dir)))
        (write-file file)
        (or (eq major-mode 'tar-mode)
            (error "For some reason, the buffer '%s' is not in tar-mode"
                   (buffer-name buf)))
        (when (tar-data-swapped-p)
          ;; Without it, `tar-untar-buffer' tries to extract files in URL
          (with-current-buffer tar-data-buffer
            (setq default-directory dir)))
        ;; Make sure the first header is a dir and all files are
        ;; placed in it (is it correct?)
        (let* ((tar-car-data (car tar-parse-info))
               (tar-dir (tar-header-name tar-car-data))
               (tar-dir-re (regexp-quote tar-dir)))
          (or (eq (tar-header-link-type tar-car-data) 5)
              (error "The first entry '%s' in tar file is not a directory"
                     tar-dir))
          (dolist (tar-data (cdr tar-parse-info))
            (or (string-match tar-dir-re (tar-header-name tar-data))
                (error "Not all files are extracted into directory '%s'"
                       tar-dir)))
          (tar-untar-buffer)
          (expand-file-name tar-dir dir))))))

(defun aurel-download-unpack-dired (url dir)
  "Download and unpack AUR package, and open the unpacked directory.
For the meaning of URL and DIR, see `aurel-download-unpack'."
  (dired (aurel-download-unpack url dir)))

(defun aurel-download-unpack-pkgbuild (url dir)
  "Download and unpack AUR package, and open PKGBUILD file.
For the meaning of URL and DIR, see `aurel-download-unpack'."
  (let* ((pkg-dir (aurel-download-unpack url dir))
         (file (expand-file-name "PKGBUILD" pkg-dir)))
    (if (file-exists-p file)
        (find-file file)
      (error "File '%s' doesn't exist" file))))

;; Avoid compilation warning about `eshell/cd'
(declare-function eshell/cd "em-dirs" (&rest args))

(defun aurel-download-unpack-eshell (url dir)
  "Download and unpack AUR package, switch to eshell.
For the meaning of URL and DIR, see `aurel-download-unpack'."
  (let ((pkg-dir (aurel-download-unpack url dir)))
    (eshell)
    (eshell/cd pkg-dir)))


;;; Defining URL

(defun aurel-get-rpc-url (type arg)
  "Return URL for getting info about AUR packages.
TYPE is the name of an allowed method.
ARG is the argument to the call."
  (url-expand-file-name
   (format "rpc.php?type=%s&arg=%s"
           (url-hexify-string type)
           (url-hexify-string arg))
   aurel-base-url))

(defun aurel-get-package-info-url (package)
  "Return URL for getting info about a PACKAGE.
PACKAGE can be either a string (name) or a number (ID)."
  (aurel-get-rpc-url "info"
                     (if (numberp package)
                         (number-to-string package)
                       package)))

(defun aurel-get-package-search-url (str)
  "Return URL for searching a package by string STR."
  (aurel-get-rpc-url "search" str))

(defun aurel-get-maintainer-search-url (str)
  "Return URL for searching a maintainer by string STR."
  (aurel-get-rpc-url "msearch" str))


;;; UI

;; FIXME `aurel-package-search' and `aurel-maintainer-search' are very
;; similar, make a macro perhaps.

(defcustom aurel-list-single-package nil
  "If non-nil, list a package even if it is the one matching result.
If nil, show a single matching package in info buffer."
  :type 'boolean
  :group 'aurel)

;;;###autoload
(defun aurel-package-info (name-or-id &optional arg)
  "Display information about AUR package NAME-OR-ID.
NAME-OR-ID may be a string or a number.
The buffer for showing results is defined by `aurel-info-buffer-name'.
With prefix (if ARG is non-nil), show results in a new info buffer."
  (interactive "sName or ID: \nP")
  (when (numberp name-or-id)
    (setq name-or-id (number-to-string name-or-id)))
  (let ((packages (aurel-receive-packages-info
                   (aurel-get-package-info-url name-or-id))))
    (if packages
        (aurel-info-show (cdar packages)
                         (when arg (generate-new-buffer
                                    aurel-info-buffer-name)))
      (message "Package %s not found." name-or-id))))

;;;###autoload
(defun aurel-package-search (str &optional arg)
  "Search for AUR packages matching a string STR.
The buffer for showing results is defined by `aurel-list-buffer-name'.
With prefix (if ARG is non-nil), show results in a new buffer."
  (interactive "sSearch by name/description: \nP")
  (let ((packages (aurel-receive-packages-info
                   (aurel-get-package-search-url str))))
    (cond
     ((null packages)
      (message "No packages matching '%s'." str))
     ((and (null (cdr packages))
           (null aurel-list-single-package))
      (aurel-info-show (cdar packages)
                       (when arg (generate-new-buffer
                                  aurel-info-buffer-name)))
      (message "A single package matching '%s' found." str))
     (t
      (aurel-list-show packages
                       (when arg (generate-new-buffer
                                  aurel-list-buffer-name)))))))

;;;###autoload
(defun aurel-maintainer-search (name &optional arg)
  "Search for AUR packages by maintainer NAME.
The buffer for showing results is defined by `aurel-list-buffer-name'.
With prefix (if ARG is non-nil), show results in a new buffer."
  (interactive "sSearch by maintainer: \nP")
  (let ((packages (aurel-receive-packages-info
                   (aurel-get-maintainer-search-url name))))
    (cond
     ((null packages)
      (message "No packages matching maintainer '%s'." name))
     ((and (null (cdr packages))
           (null aurel-list-single-package))
      (aurel-info-show (cdar packages)
                       (when arg (generate-new-buffer
                                  aurel-info-buffer-name)))
      (message "A single package by maintainer '%s' found." name))
     (t
      (aurel-list-show packages
                       (when arg (generate-new-buffer
                                  aurel-list-buffer-name)))))))


;;; Package list

(defgroup aurel-list nil
  "Buffer with a list of AUR packages."
  :group 'aurel)

(defcustom aurel-list-buffer-name "*AUR Package List*"
  "Default name of the buffer with a list of AUR packages."
  :type 'string
  :group 'aurel-list)

(defcustom aurel-list-download-function 'aurel-download-unpack
  "Function used for downloading AUR package from package list buffer.
It should accept 2 arguments: URL of a downloading file and a
destination directory."
  :type 'function
  :group 'aurel-list)

(defvar aurel-list nil
  "Alist with packages info.

Car of each assoc is a package ID (number).
Cdr - is alist of package info of the form of `aurel-info'.")

(defvar aurel-list-column-format
  '((name 25 t)
    (version 20 nil)
    (description 30 nil))
  "List specifying columns used in the buffer with a list of packages.
Each element of the list should have the form (NAME WIDTH SORT . PROPS).
NAME is a parameter symbol from `aurel-param-description-alist'.
For the meaning of WIDTH, SORT and PROPS, see `tabulated-list-format'.")

(defvar aurel-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\C-m" 'aurel-list-describe-package)
    (define-key map "d" 'aurel-list-download-package)
    map)
  "Keymap for `aurel-list-mode'.")

(define-derived-mode aurel-list-mode
  tabulated-list-mode "AURel-list"
  "Major mode for browsing AUR packages.

\\{aurel-list-mode-map}"
  (make-local-variable 'aurel-list)
  (setq default-directory aurel-download-directory)
  (setq tabulated-list-format
        (apply #'vector
               (mapcar (lambda (col-spec)
                         (cons (aurel-get-param-name (car col-spec))
                               (cdr col-spec)))
                       aurel-list-column-format)))
  (setq tabulated-list-sort-key
        (list (aurel-get-param-description 'name)))
  (tabulated-list-init-header))

(defun aurel-list-show (list &optional buffer)
  "Display a LIST of packages in BUFFER.

LIST should have the form of `aurel-list'.

If BUFFER is nil, use (create if needed) buffer with the name
`aurel-list-buffer-name'."
  (let ((buf (get-buffer-create
              (or buffer aurel-list-buffer-name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (aurel-list-mode)
      (setq aurel-list list)
      (setq tabulated-list-entries
            (aurel-list-get-entries list))
      (tabulated-list-print)
      (pop-to-buffer-same-window buf))))

(defun aurel-list-get-entries (list)
  "Return list of values suitable for `tabulated-list-entries'.
Values are taken from LIST which should have the form of
`aurel-list'.
Use parameters from `aurel-list-column-format'."
  (mapcar
   (lambda (pkg)
     (let ((id   (car pkg))
           (info (cdr pkg)))
       (list id
             (apply #'vector
                    (mapcar (lambda (col-spec)
                              (let ((val (aurel-get-param-val
                                          (car col-spec) info)))
                                (if (numberp val)
                                    (number-to-string val)
                                  val)))
                            aurel-list-column-format)))))
   list))

(defun aurel-list-get-package-info ()
  "Return package info for the current package."
  (let ((id (tabulated-list-get-id)))
    (if id
	(cdr (assoc id aurel-list))
      (user-error "No package here"))))

(defun aurel-list-describe-package (&optional arg)
  "Describe the current package.
With prefix (if ARG is non-nil), show results in a new info buffer."
  (interactive "P")
  (aurel-info-show (aurel-list-get-package-info)
                   (when arg (generate-new-buffer
                              aurel-info-buffer-name))))

(defun aurel-list-download-package ()
  "Download current package.

With prefix, prompt for a directory with `aurel-directory-prompt'
to save the package; without prefix, save to
`aurel-download-directory' without prompting.

Use `aurel-list-download-function'."
  (interactive)
  (or (derived-mode-p 'aurel-list-mode)
      (user-error "Current buffer is not in aurel-list-mode"))
  (let ((dir (if current-prefix-arg
                 (read-directory-name aurel-directory-prompt
                                      aurel-download-directory)
               aurel-download-directory)))
    (funcall aurel-list-download-function
             (aurel-get-param-val 'pkg-url (aurel-list-get-package-info))
             dir)))


;;; Package info

(defgroup aurel-info nil
  "Buffer with information about AUR package."
  :group 'aurel)

(defface aurel-info-id
  '((t))
  "Face used for ID of a package."
  :group 'aurel-info)

(defface aurel-info-name
  '((t :inherit font-lock-keyword-face))
  "Face used for a name of a package."
  :group 'aurel-info)

(defface aurel-info-maintainer
  '((t :inherit font-lock-string-face))
  "Face used for a maintainer of a package."
  :group 'aurel-info)

(defface aurel-info-url
  '((t :inherit button))
  "Face used for URLs."
  :group 'aurel-info)

(defface aurel-info-version
  '((t))
  "Face used for a version of a package."
  :group 'aurel-info)

(defface aurel-info-category
  '((t :inherit font-lock-comment-face))
  "Face used for a category of a package."
  :group 'aurel-info)

(defface aurel-info-description
  '((t))
  "Face used for a description of a package."
  :group 'aurel-info)

(defface aurel-info-license
  '((t))
  "Face used for a license of a package."
  :group 'aurel-info)

(defface aurel-info-votes
  '((t :weight bold))
  "Face used for a number of votes of a package."
  :group 'aurel-info)

(defface aurel-info-outdated
  '((t :inherit font-lock-warning-face))
  "Face used if a package is out of date."
  :group 'aurel-info)

(defface aurel-info-not-outdated
  '((t))
  "Face used if a package is not out of date."
  :group 'aurel-info)

(defface aurel-info-date
  '((t :inherit font-lock-constant-face))
  "Face used for dates."
  :group 'aurel-info)

(defcustom aurel-info-buffer-name "*AUR Package Info*"
  "Default name of the buffer with information about an AUR package."
  :type 'string
  :group 'aurel-info)

(defcustom aurel-info-format "%-14s: "
  "String used to format a description of each package parameter.
It should be a '%s'-sequence.  After inserting a description
formatted with this string, a value of the paramter is inserted."
  :type 'string
  :group 'aurel-info)

(defcustom aurel-info-fill-column 60
  "Column used for filling (word wrapping) a description of a package.
This value does not include the length of a description of the
parameter, it is added to it; see `aurel-info-format'."
  :type 'integer
  :group 'aurel-info)

(defcustom aurel-info-download-function 'aurel-download-unpack-dired
  "Function used for downloading AUR package from package info buffer.
It should accept 2 arguments: URL of a downloading file and a
destination directory."
  :type 'function
  :group 'aurel-info)

(defvar aurel-info-insert-params-alist
  '((id          . aurel-info-id)
    (name        . aurel-info-name)
    (maintainer  . aurel-info-maintainer)
    (version     . aurel-info-version)
    (category    . aurel-info-category)
    (license     . aurel-info-license)
    (votes       . aurel-info-votes)
    (first-date  . aurel-info-date)
    (last-date   . aurel-info-date)
    (description . aurel-info-insert-description)
    (outdated    . aurel-info-insert-outdated)
    (pkg-url     . aurel-info-insert-url)
    (home-url    . aurel-info-insert-url)
    (aur-url     . aurel-info-insert-url))
  "Alist for inserting parameters into info buffer.
Car of each assoc is a symbol from `aurel-param-description-alist'.
Cdr is a symbol for inserting a value of a parameter.  If the
symbol is a face name, use it for the value; if it is a function,
it is called with the value of the parameter.")

(defvar aurel-info-parameters
  '(id name version maintainer description home-url aur-url
    license category votes outdated first-date last-date)
  "List of parameters displayed in package info buffer.
Each parameter should be a symbol from `aurel-param-description-alist'.
The order of displayed parameters is the same as in this list.
If nil, display all parameters with no particular order.")

(defvar aurel-info nil
  "Alist with package info.

Car of each assoc is a symbol from `aurel-param-description-alist'.
Cdr - is a value (number or string) of that parameter.")

(defvar aurel-info-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    (define-key map "d" 'aurel-info-download-package)
    (define-key map "\t" 'forward-button)
    (define-key map [backtab] 'backward-button)
    map)
  "Keymap for `aurel-info-mode'.")

(define-derived-mode aurel-info-mode nil "AURel-info"
  "Major mode for displaying information about an AUR package.

\\{aurel-info-mode-map}"
  (make-local-variable 'aurel-info)
  (setq buffer-read-only t)
  (setq default-directory aurel-download-directory))

(defun aurel-info-show (info &optional buffer)
  "Display package information INFO in BUFFER.
INFO should have the form of `aurel-info'.
If BUFFER is nil, use (create if needed) buffer with the name
`aurel-info-buffer-name'."
  (let ((buf (get-buffer-create
              (or buffer aurel-info-buffer-name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (aurel-info-print info))
      (goto-char (point-min))
      (aurel-info-mode)
      (setq aurel-info info)
      (pop-to-buffer-same-window buf))))

(defun aurel-info-print (info)
  "Insert (pretty print) package INFO into current buffer.
Insert parameters from `aurel-info-parameters'."
  (let ((params (or aurel-info-parameters
                    (mapcar #'car info))))
    (mapc (lambda (param)
            (aurel-info-print-param
             param (aurel-get-param-val param info)))
          params)))

(defun aurel-info-print-param (param val)
  "Insert description and value VAL of a parameter PARAM at point.
PARAM is a symbol from `aurel-param-description-alist'.
VAL can be a string or a number.
Use `aurel-info-format' to format descriptions of parameters."
  (when (numberp val)
    (setq val (number-to-string val)))
  (let ((desc (aurel-get-param-description param))
        (insert-val (cdr (assoc param
                                aurel-info-insert-params-alist))))
    (insert (format aurel-info-format desc))
    (cond
     ((functionp insert-val)
      (funcall insert-val val))
     ((facep insert-val)
      (insert (propertize val 'face insert-val)))
     (t (insert val)))
    (insert "\n")))

(defun aurel-info-insert-url (url)
  "Make button from URL and insert it at point."
  (insert-button
   url
   'face 'aurel-info-url
   'action (lambda (btn) (browse-url (button-label btn)))
   'follow-link t
   'help-echo "mouse-2, RET: Browse URL"))

(defun aurel-info-insert-outdated (val)
  "Insert string VAL at point.
If VAL is \"yes\", use `aurel-info-outdated' face.
If VAL is \"no\", use `aurel-info-not-outdated' face."
  (let ((face (if (string= "yes" (downcase val))
                  'aurel-info-outdated
                'aurel-info-not-outdated)))
    (insert (propertize val 'face face))))

(defun aurel-info-get-filled-string (str col)
  "Return string by filling a string STR.
COL controls the width for filling."
  (with-temp-buffer
    (insert str)
    (let ((fill-column col)) (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun aurel-info-insert-description (str)
  "Format and insert string STR at point.
Use `aurel-info-fill-column'."
  (let ((parts (split-string (aurel-info-get-filled-string
                              str aurel-info-fill-column)
                             "\n")))
    (insert (propertize (car parts) 'face 'aurel-info-description))
    (dolist (part (cdr parts))
      (insert "\n"
              (format aurel-info-format "")
              (propertize part 'face 'aurel-info-description)))))

(defun aurel-info-download-package ()
  "Download current package.

With prefix, prompt for a directory with `aurel-directory-prompt'
to save the package; without prefix, save to
`aurel-download-directory' without prompting.

Use `aurel-info-download-function'."
  (interactive)
  (or (derived-mode-p 'aurel-info-mode)
      (user-error "Current buffer is not in aurel-info-mode"))
  (let ((dir (if current-prefix-arg
                 (read-directory-name aurel-directory-prompt
                                      aurel-download-directory)
               aurel-download-directory)))
    (funcall aurel-info-download-function
             (aurel-get-param-val 'pkg-url aurel-info)
             dir)))

(provide 'aurel)

;;; aurel.el ends here
