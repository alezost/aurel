;;; aurel.el --- Search, get info, vote and download AUR packages

;; Copyright (C) 2014 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 6 Feb 2014
;; Version: 0.6
;; URL: https://github.com/alezost/aurel
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

;; The package provides an interface for searching, getting information,
;; voting, subscribing and downloading packages from the Arch User
;; Repository (AUR) <https://aur.archlinux.org/>.

;; To manually install the package, add the following to your init-file:
;;
;;   (add-to-list 'load-path "/path/to/aurel-dir")
;;   (autoload 'aurel-package-info "aurel" nil t)
;;   (autoload 'aurel-package-search "aurel" nil t)
;;   (autoload 'aurel-maintainer-search "aurel" nil t)
;;   (autoload 'aurel-installed-packages "aurel" nil t)

;; Also set a directory where downloaded packages will be put:
;;
;;   (setq aurel-download-directory "~/aur")

;; To search for packages, use `aurel-package-search' or
;; `aurel-maintainer-search' commands.  If you know the name of a
;; package, use `aurel-package-info' command.  Also you can display a
;; list of installed AUR packages with `aurel-installed-packages'.

;; Information about the packages is represented in a list-like buffer
;; similar to a buffer containing emacs packages.  To get more info
;; about a package, press "RET" on a package line.  To download a
;; package, press "d" (don't forget to set `aurel-download-directory'
;; before).  In a list buffer, you can mark several packages for
;; downloading with "m"/"M" (and unmark with "u"/"U" and "DEL"); also
;; you can perform filtering (press "f f" to enable a filter and "f d"
;; to disable all filters) of a current list to hide particular
;; packages.

;; It is possible to move to the previous/next displayed results with
;; "l"/"r" (each aurel buffer has its own history) and to refresh
;; information with "g".

;; After receiving information about the packages, pacman is called to
;; find what packages are installed.  To disable that, set
;; `aurel-installed-packages-check' to nil.

;; To vote/subscribe for a package, press "v"/"s" (with prefix
;; unvote/unsubscribe) in a package info buffer (you should have an AUR
;; account for that).  To add information about "Voted"/"Subscribed"
;; status, use the following:
;;
;;   (setq aurel-aur-user-package-info-check t)

;; For full description and screenshots, see
;; <https://github.com/alezost/aurel>.

;;; Code:

(require 'url)
(require 'url-handlers)
(require 'json)
(require 'tabulated-list)
(require 'cl-macs)

(defgroup aurel nil
  "Search and download AUR (Arch User Repository) packages."
  :group 'applications)

(defcustom aurel-empty-string "-"
  "String used for empty (or none) values of package parameters."
  :type 'string
  :group 'aurel)

(defcustom aurel-true-string "Yes"
  "String used if a parameter value is t."
  :type 'string
  :group 'aurel)

(defcustom aurel-false-string "No"
  "String used if a parameter value is nil."
  :type 'string
  :group 'aurel)

(defcustom aurel-date-format "%Y-%m-%d %T"
  "Time format used to represent time parameters of a package.
For information about time formats, see `format-time-string'."
  :type 'string
  :group 'aurel)

(defcustom aurel-list-single-package nil
  "If non-nil, list a package even if it is the one matching result.
If nil, show a single matching package in info buffer."
  :type 'boolean
  :group 'aurel)

(defcustom aurel-aur-user-package-info-check nil
  "If non-nil, check additional info before displaying a package info.
Additional info is an AUR user specific information (whether the user
voted for the package or subscribed to receive comments)."
  :type 'boolean
  :group 'aurel)

(defvar aurel-unknown-string "Unknown"
  "String used if a value of the parameter is unknown.")

(defvar aurel-none-string "None"
  "String saying that a parameter has no value.")

(defvar aurel-package-name-re
  "[-+_[:alnum:]]+"
  "Regexp matching a valid package name.")

(defun aurel-get-string (val &optional face)
  "Return string from VAL.
If VAL is `aurel-none-string' return `aurel-empty-string'.
If VAL is nil, return `aurel-false-string'.
If VAL is t, return `aurel-true-string'.
If VAL is a number, use `number-to-string'.
If VAL is a time value, format it with `aurel-date-format'.
Otherwise, if VAL is not string, use `prin1-to-string'.
If FACE is non-nil, propertize returned string with this FACE."
  (if (equal val aurel-none-string)
      aurel-empty-string
    (setq val
          (cond
           ((stringp val) val)
           ((null val) aurel-false-string)
           ((eq t val) aurel-true-string)
           ((numberp val) (number-to-string val))
           ((aurel-time-p val)
            (format-time-string aurel-date-format val))
           (t (prin1-to-string val))))
    (if face
        (propertize val 'face face)
      val)))

(defun aurel-time-p (val)
  "Return non-nil, if VAL is a time value; return nil otherwise."
  (condition-case nil
      (decode-time val)
    (error nil)))


;;; Debugging

(defvar aurel-debug-level 0
  "If > 0, display debug messages in `aurel-debug-buffer'.
The greater the number, the more messages is printed.
Max level is 9.")

(defvar aurel-debug-buffer "*aurel debug*"
  "Name of a buffer containing debug messages.")

(defvar aurel-debug-time-format "%T.%3N"
  "Time format used for debug mesages.")

(defun aurel-debug (level msg &rest args)
  "Print debug message if needed.
If `aurel-debug-level' >= LEVEL, print debug message MSG with
arguments ARGS into `aurel-debug-buffer'.
Return nil."
  (when (>= aurel-debug-level level)
    (with-current-buffer (get-buffer-create aurel-debug-buffer)
      (goto-char (point-max))
      (insert (format-time-string aurel-debug-time-format (current-time)))
      (insert " " (apply 'format msg args) "\n")))
  nil)


;;; Interacting with AUR server

(defcustom aurel-aur-user-name ""
  "User name for AUR."
  :type 'string
  :group 'aurel)

(defvar aurel-aur-host "aur.archlinux.org"
  "AUR domain.")

(defvar aurel-aur-base-url (concat "https://" aurel-aur-host)
  "Root URL of the AUR service.")

(defvar aurel-aur-login-url
  (url-expand-file-name "login" aurel-aur-base-url)
  "Login URL.")

(defconst aurel-aur-cookie-name "AURSID"
  "Cookie name used for AUR login.")

;; Avoid compilation warning about `url-http-response-status'
(defvar url-http-response-status)

(defun aurel-check-response-status (buffer &optional noerror)
  "Return t, if URL response status in BUFFER is 2XX or 3XX.
Otherwise, throw an error or return nil, if NOERROR is nil."
  (with-current-buffer buffer
    (aurel-debug 3 "Response status: %s" url-http-response-status)
    (if (or (null (numberp url-http-response-status))
            (> url-http-response-status 399))
        (unless noerror (error "Error during request: %s"
                               url-http-response-status))
      t)))

(defun aurel-receive-parse-info (url)
  "Return received output from URL processed with `json-read'."
  (aurel-debug 3 "Retrieving %s" url)
  (let ((buf (url-retrieve-synchronously url)))
    (aurel-check-response-status buf)
    (with-current-buffer buf
      (goto-char (point-min))
      (re-search-forward "^{") ;; is there a better way?
      (beginning-of-line)
      (let ((json-key-type 'string)
            (json-array-type 'list)
            (json-object-type 'alist))
        (json-read)))))

(defun aurel-get-aur-packages-info (url)
  "Return information about the packages from URL.
Output from URL should be a json data.  It is parsed with
`json-read'.
Returning value is alist of AUR package parameters (strings from
`aurel-aur-param-alist') and their values."
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
      results))))

;; Because of the bug #16960, we can't use `url-retrieve-synchronously'
;; (or any other simple call of `url-retrieve', as the callback is never
;; called) to login to <https://aur.archlinux.org>.  So we use
;; `aurel-url-retrieve-synchronously' - it is almost the same, except it
;; can exit from the waiting loop when a buffer with received data
;; appears in `url-dead-buffer-list'.  This hack is currently possible,
;; because `url-http-parse-headers' marks the buffer as dead when it
;; returns nil.

(defun aurel-url-retrieve-synchronously (url &optional silent inhibit-cookies)
  "Retrieve URL synchronously.
Return the buffer containing the data, or nil if there are no data
associated with it (the case for dired, info, or mailto URLs that need
no further processing).  URL is either a string or a parsed URL.
See `url-retrieve' for SILENT and INHIBIT-COOKIES."
  (url-do-setup)
  (let (asynch-buffer retrieval-done)
    (setq asynch-buffer
          (url-retrieve url
                        (lambda (&rest ignored)
                          (url-debug 'retrieval
                                     "Synchronous fetching done (%S)"
                                     (current-buffer))
                          (setq retrieval-done t
                                asynch-buffer (current-buffer)))
                        nil silent inhibit-cookies))
    (when asynch-buffer
      (let ((proc (get-buffer-process asynch-buffer)))
        (while (not (or retrieval-done
                        ;; retrieval can be done even if
                        ;; `retrieval-done' is nil (see the comment
                        ;; above)
                        (memq asynch-buffer url-dead-buffer-list)))
          (url-debug 'retrieval
                     "Spinning in url-retrieve-synchronously: %S (%S)"
                     retrieval-done asynch-buffer)
          (if (buffer-local-value 'url-redirect-buffer asynch-buffer)
              (setq proc (get-buffer-process
                          (setq asynch-buffer
                                (buffer-local-value 'url-redirect-buffer
                                                    asynch-buffer))))
            (if (and proc (memq (process-status proc)
                                '(closed exit signal failed))
                     ;; Make sure another process hasn't been started.
                     (eq proc (or (get-buffer-process asynch-buffer) proc)))
                (progn ;; Call delete-process so we run any sentinel now.
                  (delete-process proc)
                  (setq retrieval-done t)))
            (unless (or (with-local-quit
                          (accept-process-output proc))
                        (null proc))
              (when quit-flag
                (delete-process proc))
              (setq proc (and (not quit-flag)
                              (get-buffer-process asynch-buffer)))))))
      asynch-buffer)))

(defun aurel-url-post (url args &optional inhibit-cookies)
  "Send ARGS to URL as a POST request.
ARGS is alist of field names and values to send.
Return the buffer with the received data.
If INHIBIT-COOKIES is non-nil, do not use saved cookies."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (aurel-get-fields-string args)))
    (aurel-debug 2 "POSTing to %s" url)
    (aurel-url-retrieve-synchronously url inhibit-cookies)))

(defun aurel-get-aur-cookie ()
  "Return cookie for AUR login.
Return nil, if there is no such cookie or it is expired."
  (url-do-setup) ; initialize cookies
  (let* ((cookies (url-cookie-retrieve aurel-aur-host "/" t))
         (cookie (car (cl-member-if
                       (lambda (cookie)
                         (equal (url-cookie-name cookie)
                                aurel-aur-cookie-name))
                       cookies))))
    (if (null cookie)
        (aurel-debug 4 "AUR login cookie not found")
      (if (url-cookie-expired-p cookie)
          (aurel-debug 4 "AUR login cookie is expired")
        (aurel-debug 4 "AUR login cookie is valid")
        cookie))))

(declare-function auth-source-search "auth-source" t)

(defun aurel-aur-login-maybe (&optional force noerror)
  "Login to AUR, use cookie if possible.
If FORCE is non-nil (interactively, with prefix), prompt for
credentials and login without trying the cookie.
See `aurel-aur-login' for the meaning of NOERROR and returning value."
  (interactive "P")
  (if (aurel-get-aur-cookie)
      (progn
        (aurel-debug 2 "Using cookie instead of a real login")
        t)
    (let (user password)
      (let ((auth (car (auth-source-search :host aurel-aur-host))))
        (when auth
          (let ((secret (plist-get auth :secret)))
            (setq user (plist-get auth :user)
                  password (if (functionp secret)
                               (funcall secret)
                             secret)))))
      (when (or force (null user))
        (setq user (read-string "AUR user name: " aurel-aur-user-name)))
      (when (or force (null password))
        (setq password (read-passwd "Password: ")))
      (aurel-aur-login user password t noerror))))

(defun aurel-aur-login (user password &optional remember noerror)
  "Login to AUR with USER and PASSWORD.
If REMEMBER is non-nil, remember a cookie.
Return t, if login was successful, otherwise throw an error or
return nil, if NOERROR is non-nil."
  (let ((buf (aurel-url-post
              aurel-aur-login-url
              (list (cons "user" user)
                    (cons "passwd" password)
                    (cons "remember_me" (if remember "on" "off")))
              'inhibit-cookie)))
    (when (aurel-check-response-status buf noerror)
      (with-current-buffer buf
        (if (re-search-forward "errorlist.+<li>\\(.+\\)</li>" nil t)
            (let ((err (match-string 1)))
              (aurel-debug 1 "Error during login: %s" )
              (or noerror (error "%s" err))
              nil)
          (url-cookie-write-file)
          (aurel-debug 1 "Login for %s is successful" user)
          t)))))

(defun aurel-add-aur-user-package-info (info)
  "Append additional info to a package INFO.
INFO should have a form of `aurel-info'.
See `aurel-aur-user-package-info-check' for the meaning of
additional info."
  (let ((add (aurel-get-aur-user-package-info
              (aurel-get-aur-package-url
               (aurel-get-param-val 'name info)))))
    (when add
      (setcdr (last info) add))))

(defun aurel-get-aur-user-package-info (url)
  "Return AUR user specific information about a package from URL.
Returning value is alist of package parameters specific for AUR
user (`voted' and `subscribed') and their values.
Return nil, if information is not found."
  (when (aurel-aur-login-maybe nil t)
    (aurel-debug 3 "Retrieving %s" url)
    (let ((buf (url-retrieve-synchronously url)))
      (aurel-debug 4 "Searching in %S for voted/subscribed params" buf)
      (list (cons 'voted
                  (aurel-aur-package-voted buf))
            (cons 'subscribed
                  (aurel-aur-package-subscribed buf))))))

(defun aurel-aur-package-voted (buffer)
  "Return `voted' parameter value from BUFFER with fetched data.
Return non-nil if a package is voted by the user; nil if it is not;
`aurel-unknown-string' if the information is not found.
BUFFER should contain html data about the package."
  (cond
   ((aurel-search-in-buffer
     (aurel-get-aur-user-action-name 'vote) buffer)
    nil)
   ((aurel-search-in-buffer
     (aurel-get-aur-user-action-name 'unvote) buffer)
    t)
   (t aurel-unknown-string)))

(defun aurel-aur-package-subscribed (buffer)
  "Return `subscribed' parameter value from BUFFER with fetched data.
Return non-nil if a package is subscribed by the user; nil if it is not;
`aurel-unknown-string' if the information is not found.
BUFFER should contain html data about the package."
  (cond
   ((aurel-search-in-buffer
     (aurel-get-aur-user-action-name 'subscribe) buffer)
    nil)
   ((aurel-search-in-buffer
     (aurel-get-aur-user-action-name 'unsubscribe) buffer)
    t)
   (t aurel-unknown-string)))

(defun aurel-search-in-buffer (regexp buffer)
  "Return non-nil if BUFFER contains REGEXP; return nil otherwise."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((res (re-search-forward regexp nil t)))
      (aurel-debug 7 "Searching for %s in %S: %S" regexp buffer res)
      res)))

(defvar aurel-aur-user-actions
  '((vote        "do_Vote"     "vote"     "Vote for the current package?")
    (unvote      "do_UnVote"   "unvote"   "Do you really want to unvote?")
    (subscribe   "do_Notify"   "notify"   "Subscribe to the new comments?")
    (unsubscribe "do_UnNotify" "unnotify" "Unsubscribe from notifying about the new comments?"))
  "Alist of the available actions.
Each association has the following form:

  (SYMBOL NAME URL-END CONFIRM)

SYMBOL is a name of the action used internally in code of this package.
NAME is a name (string) used in the html-code of AUR package page.
URL-END is appended to the package URL; used for posting the action.
CONFIRM is a prompt to confirm the action or nil if it is not required.")

(defun aurel-get-aur-user-action-name (action)
  "Return the name of an ACTION."
  (cadr (assoc action aurel-aur-user-actions)))

(defun aurel-aur-user-action (action package-base)
  "Perform AUR user ACTION on the PACKAGE-BASE.
ACTION is a symbol from `aurel-aur-user-actions'.
PACKAGE-BASE is a name of the package base (string).
Return non-nil, if ACTION was performed; return nil otherwise."
  (let ((assoc (assoc action aurel-aur-user-actions)))
    (let ((action-name (nth 1 assoc))
          (url-end     (nth 2 assoc))
          (confirm     (nth 3 assoc)))
      (when (or (null confirm)
                (y-or-n-p confirm))
        (aurel-aur-login-maybe)
        (aurel-url-post
         (aurel-get-package-action-url package-base url-end)
         (list (cons "token" (url-cookie-value (aurel-get-aur-cookie)))
               (cons action-name "")))
        t))))


;;; Interacting with pacman

(defcustom aurel-pacman-program "pacman"
  "Absolute or relative name of `pacman' program."
  :type 'string
  :group 'aurel)

(defvar aurel-pacman-locale "en_US.UTF8"
  "Default locale used to start pacman.")

(defcustom aurel-installed-packages-check t
  "If non-nil, check if the found packages are installed.
If nil, searching works faster, because `aurel-pacman-program' is not
called, but it stays unknown if a package is installed or not."
  :type 'boolean
  :group 'aurel)

(defvar aurel-pacman-buffer-name " *aurel-pacman*"
  "Name of the buffer used internally for pacman output.")

(defvar aurel-pacman-info-line-re
  (rx line-start
      (group (+? (any word " ")))
      (+ " ") ":" (+ " ")
      (group (+ any) (* (and "\n " (+ any))))
      line-end)
  "Regexp matching a line of pacman query info output.
Contain 2 parenthesized groups: parameter name and its value.")

(defun aurel-call-pacman (&optional buffer &rest args)
  "Call `aurel-pacman-program' with arguments ARGS.
Insert output in BUFFER.  If it is nil, use `aurel-pacman-buffer-name'.
Return numeric exit status."
  (let ((pacman (executable-find aurel-pacman-program)))
    (or pacman
        (error (concat "Couldn't find '%s'.\n"
                       "Set aurel-pacman-program to a proper value")
               aurel-pacman-program))
    (with-current-buffer
        (or buffer (get-buffer-create aurel-pacman-buffer-name))
      (erase-buffer)
      (let ((process-environment
             (cons (concat "LANG=" aurel-pacman-locale)
                   process-environment)))
        (apply #'call-process pacman nil t nil args)))))

(defun aurel-get-foreign-packages ()
  "Return list of names of installed foreign packages."
  (let ((buf (get-buffer-create aurel-pacman-buffer-name)))
    (aurel-call-pacman buf "--query" "--foreign")
    (aurel-pacman-query-names-buffer-parse buf)))

(defun aurel-pacman-query-names-buffer-parse (&optional buffer)
  "Parse BUFFER with packages names.
BUFFER should contain an output returned by 'pacman -Q' command.
If BUFFER is nil, use `aurel-pacman-buffer-name'.
Return list of names of packages."
  (with-current-buffer
      (or buffer (get-buffer-create aurel-pacman-buffer-name))
    (goto-char (point-min))
    (let (names)
      (while (re-search-forward
              (concat "^\\(" aurel-package-name-re "\\) ") nil t)
        (setq names (cons (match-string 1) names)))
      names)))

(defun aurel-get-installed-packages-info (&rest names)
  "Return information about installed packages NAMES.
Each name from NAMES should be a string (a name of a package).
Returning value is a list of alists with installed package
parameters (strings from `aurel-installed-param-alist') and their
values."
  (let ((buf (get-buffer-create aurel-pacman-buffer-name)))
    (apply 'aurel-call-pacman buf "--query" "--info" names)
    (aurel-pacman-query-buffer-parse buf)))

(defun aurel-pacman-query-buffer-parse (&optional buffer)
  "Parse BUFFER with packages info.
BUFFER should contain an output returned by 'pacman -Qi' command.
If BUFFER is nil, use `aurel-pacman-buffer-name'.
Return list of alists with parameter names and values."
  (with-current-buffer
      (or buffer (get-buffer-create aurel-pacman-buffer-name))
    (let ((max (point-max))
          (beg (point-min))
          end info)
      ;; Packages info are separated with empty lines, search for those
      ;; till the end of buffer
      (cl-loop
       do (progn
            (goto-char beg)
            (setq end (re-search-forward "^\n" nil t))
            (and end
                 (setq info (aurel-pacman-query-region-parse beg end)
                       beg end)))
       while end
       if info collect info))))

(defun aurel-pacman-query-region-parse (beg end)
  "Parse text (package info) in current buffer from BEG to END.
Parsing region should be an output for one package returned by
'pacman -Qi' command.
Return alist with parameter names and values."
  (goto-char beg)
  (let (point)
    (cl-loop
     do (setq point (re-search-forward
                     aurel-pacman-info-line-re end t))
     while point
     collect (cons (match-string 1) (match-string 2)))))


;;; Package parameters

(defvar aurel-aur-param-alist
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
    (base-name   . "PackageBase")
    (base-id     . "PackageBaseID")
    (maintainer  . "Maintainer"))
  "Association list of symbols and names of package info parameters.
Car of each assoc is a symbol used in code of this package.
Cdr - is a parameter name (string) returned by the AUR server.")

(defvar aurel-pacman-param-alist
  '((installed-name    . "Name")
    (installed-version . "Version")
    (architecture      . "Architecture")
    (provides          . "Provides")
    (depends           . "Depends On")
    (depends-opt       . "Optional Deps")
    (script            . "Install Script")
    (reason            . "Install Reason")
    (validated         . "Validated By")
    (required          . "Required By")
    (optional-for      . "Optional For")
    (conflicts         . "Conflicts With")
    (replaces          . "Replaces")
    (installed-size    . "Installed Size")
    (packager          . "Packager")
    (build-date        . "Build Date")
    (install-date      . "Install Date"))
  "Association list of symbols and names of package info parameters.
Car of each assoc is a symbol used in code of this package.
Cdr - is a parameter name (string) returned by pacman.")

(defvar aurel-param-description-alist
  '((pkg-url           . "Download URL")
    (home-url          . "Home Page")
    (aur-url           . "AUR Page")
    (base-url          . "Package Base")
    (last-date         . "Last Modified")
    (first-date        . "Submitted")
    (outdated          . "Out Of Date")
    (votes             . "Votes")
    (license           . "License")
    (description       . "Description")
    (category          . "Category")
    (version           . "Version")
    (name              . "Name")
    (id                . "ID")
    (base-name         . "Package Base")
    (base-id           . "Package Base ID")
    (maintainer        . "Maintainer")
    (installed-name    . "Name")
    (installed-version . "Version")
    (architecture      . "Architecture")
    (provides          . "Provides")
    (depends           . "Depends On")
    (depends-opt       . "Optional Deps")
    (script            . "Install Script")
    (reason            . "Install Reason")
    (validated         . "Validated By")
    (required          . "Required By")
    (optional-for      . "Optional For")
    (conflicts         . "Conflicts With")
    (replaces          . "Replaces")
    (installed-size    . "Size")
    (packager          . "Packager")
    (build-date        . "Build Date")
    (install-date      . "Install Date")
    (voted             . "Voted")
    (subscribed        . "Subscribed"))
  "Association list of symbols and descriptions of parameters.
Descriptions are used for displaying package information.
Symbols are either from `aurel-aur-param-alist', from
`aurel-pacman-param-alist' or are added by filter functions.  See
`aurel-apply-filters' for details.")

(defun aurel-get-aur-param-name (param-symbol)
  "Return a name (string) of a parameter.
PARAM-SYMBOL is a symbol from `aurel-aur-param-alist'."
  (cdr (assoc param-symbol aurel-aur-param-alist)))

(defun aurel-get-aur-param-symbol (param-name)
  "Return a symbol name of a parameter.
PARAM-NAME is a string from `aurel-aur-param-alist'."
  (car (rassoc param-name aurel-aur-param-alist)))

(defun aurel-get-pacman-param-name (param-symbol)
  "Return a name (string) of a parameter.
PARAM-SYMBOL is a symbol from `aurel-pacman-param-alist'."
  (cdr (assoc param-symbol aurel-pacman-param-alist)))

(defun aurel-get-pacman-param-symbol (param-name)
  "Return a symbol name of a parameter.
PARAM-NAME is a string from `aurel-pacman-param-alist'."
  (car (rassoc param-name aurel-pacman-param-alist)))

(defun aurel-get-param-description (param-symbol)
  "Return a description of a parameter PARAM-SYMBOL."
  (let ((desc (cdr (assoc param-symbol
                          aurel-param-description-alist))))
    (or desc
        (progn
          (setq desc (symbol-name param-symbol))
          (message "Couldn't find '%s' in aurel-param-description-alist."
                   desc)
          desc))))

(defun aurel-get-param-val (param info)
  "Return a value of a parameter PARAM from a package INFO."
  (cdr (assoc param info)))


;;; Filters for processing package info

(defvar aurel-categories
  [nil "None" "daemons" "devel" "editors"
       "emulators" "games" "gnome" "i18n" "kde" "lib"
       "modules" "multimedia" "network" "office"
       "science" "system" "x11" "xfce" "kernels" "fonts"]
  "Vector of package categories.
Index of an element is a category ID.")

(defvar aurel-filter-params nil
  "List of parameters (symbols), that should match specified strings.
Used in `aurel-filter-contains-every-string'.")

(defvar aurel-filter-strings nil
  "List of strings, a package info should match.
Used in `aurel-filter-contains-every-string'.")

(defvar aurel-aur-filters
  '(aurel-aur-filter-intern aurel-filter-contains-every-string
    aurel-aur-filter-date aurel-filter-outdated
    aurel-filter-category aurel-filter-pkg-url)
  "List of filter functions applied to a package info got from AUR.

Each filter function should accept a single argument - info alist
with package parameters and should return info alist or
nil (which means: ignore this package info).  Functions may
modify associations or add the new ones to the alist.  In the
latter case you might want to add descriptions of the added
symbols into `aurel-param-description-alist'.

`aurel-aur-filter-intern' should be the first symbol in the list as
other filters use symbols for working with info parameters (see
`aurel-aur-param-alist').

For more information, see `aurel-receive-packages-info'.")

(defvar aurel-pacman-filters
  '(aurel-pacman-filter-intern aurel-pacman-filter-date)
"List of filter functions applied to a package info got from pacman.

`aurel-pacman-filter-intern' should be the first symbol in the list as
other filters use symbols for working with info parameters (see
`aurel-pacman-param-alist').

For more information, see `aurel-aur-filters' and
`aurel-receive-packages-info'.")

(defvar aurel-final-filters
  '()
  "List of filter functions applied to a package info.
For more information, see `aurel-receive-packages-info'.")

(defun aurel-apply-filters (info filters)
  "Apply functions from FILTERS list to a package INFO.

INFO is alist with package parameters.  It is passed as an
argument to the first function from FILTERS, the returned result
is passed to the second function from that list and so on.

Return filtered info (result of the last filter).  Return nil, if
one of the FILTERS returns nil (do not call the rest filters)."
  (cl-loop for fun in filters
           do (setq info (funcall fun info))
           while info
           finally return info))

(defun aurel-filter-intern (info param-fun &optional warning)
  "Replace names of parameters with symbols in a package INFO.
INFO is alist of parameter names (strings) and values.
PARAM-FUN is a function for getting parameter internal symbol by
its name (string).
If WARNING is non-nil, show a message if unknown parameter is found.
Return modified info."
  (delq nil
        (mapcar
         (lambda (param)
           (let* ((param-name (car param))
                  (param-symbol (funcall param-fun param-name))
                  (param-val (cdr param)))
             (if param-symbol
                 (cons param-symbol param-val)
               (and warning
                    (message "Warning: unknown parameter '%s'. It will be omitted."
                             param-name))
               nil)))
         info)))

(defun aurel-aur-filter-intern (info)
  "Replace names of parameters with symbols in a package INFO.
INFO is alist of parameter names (strings) from
`aurel-aur-param-alist' and their values.
Return modified info."
  (aurel-filter-intern info 'aurel-get-aur-param-symbol t))

(defun aurel-pacman-filter-intern (info)
  "Replace names of parameters with symbols in a package INFO.
INFO is alist of parameter names (strings) from
`aurel-pacman-param-alist' and their values.
Return modified info."
  (aurel-filter-intern info 'aurel-get-pacman-param-symbol))

(defun aurel-filter-contains-every-string (info)
  "Check if a package INFO contains all necessary strings.

Return INFO, if values of parameters from `aurel-filter-params'
contain all strings from `aurel-filter-strings', otherwise return nil.

Pass the check (return INFO), if `aurel-filter-strings' or
`aurel-filter-params' is nil."
  (when (or (null aurel-filter-params)
            (null aurel-filter-strings)
            (let ((str (mapconcat (lambda (param)
                                    (aurel-get-param-val param info))
                                  aurel-filter-params
                                  "\n")))
              (cl-every (lambda (substr)
                          (string-match-p (regexp-quote substr) str))
                        aurel-filter-strings)))
    info))

(defun aurel-filter-date (info fun &rest params)
  "Convert date parameters PARAMS of a package INFO to time values.
INFO is alist of parameter symbols and values.
FUN is a function taking parameter value as an argument and
returning time value.
Return modified info."
  (dolist (param info info)
    (let ((param-name (car param))
          (param-val  (cdr param)))
      (when (memq param-name params)
        (setcdr param
                (funcall fun param-val))))))

(defun aurel-aur-filter-date (info)
  "Convert date parameters PARAMS of a package INFO to time values.
Converted parameters: `first-date', `last-date'.
INFO is alist of parameter symbols and values.
Return modified info."
  (aurel-filter-date info 'seconds-to-time 'first-date 'last-date))

(defun aurel-pacman-filter-date (info)
  "Convert date parameters PARAMS of a package INFO to time values.
Converted parameters: `install-date', `build-date'.
INFO is alist of parameter symbols and values.
Return modified info."
  (aurel-filter-date info 'date-to-time 'install-date 'build-date))

(defun aurel-filter-outdated (info)
  "Change `outdated' parameter of a package INFO.
Replace 1/0 with t/nil.
INFO is alist of parameter symbols and values.
Return modified info."
  (let ((param (assoc 'outdated info)))
    (setcdr param (null (= 0 (cdr param)))))
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
    (setcdr param (url-expand-file-name (cdr param) aurel-aur-base-url)))
  info)


;;; Searching/showing packages

(defun aurel-receive-packages-info (url)
  "Return information about the packages from URL.

Information is received with `aurel-get-aur-packages-info', then
it is passed through `aurel-aur-filters' with
`aurel-apply-filters'.  If `aurel-installed-packages-check' is
non-nil, additional information about installed packages is
received with `aurel-get-installed-packages-info' and is passed
through `aurel-installed-filters'.  Finally packages info is passed
through `aurel-final-filters'.

Returning value has a form of `aurel-list'."
  ;; To speed-up the process, pacman should be called once with the
  ;; names of found packages (instead of calling it for each name).  So
  ;; we need to know the names at first, that's why we don't use a
  ;; single filters variable: at first we filter info received from AUR,
  ;; then we add information about installed packages from pacman and
  ;; finally filter the whole info.
  (let (aur-info-list aur-info-alist
        pac-info-list pac-info-alist
        info-list)
    ;; Receive and process information from AUR server
    (setq aur-info-list  (aurel-get-aur-packages-info url)
          aur-info-alist (aurel-get-filtered-alist
                          aur-info-list aurel-aur-filters 'name))
    ;; Receive and process information from pacman
    (when aurel-installed-packages-check
      (setq pac-info-list  (apply 'aurel-get-installed-packages-info
                                  (mapcar #'car aur-info-alist))
            pac-info-alist (aurel-get-filtered-alist
                            pac-info-list
                            aurel-pacman-filters
                            'installed-name)))
    ;; Join info and do final processing
    (setq info-list
          (mapcar (lambda (aur-info-assoc)
                    (let* ((name (car aur-info-assoc))
                           (pac-info-assoc (assoc name pac-info-alist)))
                      (append (cdr aur-info-assoc)
                              (cdr pac-info-assoc))))
                  aur-info-alist))
    (aurel-get-filtered-alist info-list aurel-final-filters 'id)))

(defun aurel-get-filtered-alist (info-list filters param)
  "Return alist with filtered packages info.
INFO-LIST is a list of packages info.  Each info is passed through
FILTERS with `aurel-apply-filters'.

Each association of a returned value has a form:

  (PARAM-VAL . INFO)

PARAM-VAL is a value of a parameter PARAM.
INFO is a filtered package info."
  (delq nil                             ; ignore filtered (empty) info
        (mapcar (lambda (info)
                  (let ((info (aurel-apply-filters info filters)))
                    (and info
                         (cons (aurel-get-param-val param info) info))))
                info-list)))

(defun aurel-get-packages-by-name-or-id (&rest names)
  "Return packages by NAMES.
Each element from NAMES should be a string.  If there is only one
element, it can also be a number (package ID).
Returning value has a form of `aurel-list'."
  (aurel-receive-packages-info
   ;; AUR RPC service do not support specifying multiple packages by IDs
   ;; (only by names), so we can't use `aurel-get-package-multiinfo-url'
   ;; as a common case: if there is only one requested package, it can
   ;; be either a name or ID.
   (if (cdr names)
       (aurel-get-package-multiinfo-url names)
     (aurel-get-package-info-url (car names)))))

(defun aurel-get-packages-by-string (&rest strings)
  "Return packages matching STRINGS.
Returning value has a form of `aurel-list'."
  ;; A hack for searching by multiple strings: the actual server search
  ;; is done by the biggest string and the rest strings are searched in
  ;; the results returned by the server
  (let* ((str-list
          ;; sort to search by the biggest (first) string
          (sort strings
                (lambda (a b)
                  (> (length a) (length b)))))
         (aurel-filter-params '(name description))
         (aurel-filter-strings (cdr str-list)))
    (aurel-receive-packages-info
     (aurel-get-package-search-url (car str-list)))))

(defun aurel-get-packages-by-maintainer (name)
  "Return packages by maintainer NAME.
Returning value has a form of `aurel-list'."
  (aurel-receive-packages-info
   (aurel-get-maintainer-search-url name)))

(defvar aurel-search-type-alist
  '((name-or-id . aurel-get-packages-by-name-or-id)
    (string     . aurel-get-packages-by-string)
    (maintainer . aurel-get-packages-by-maintainer))
  "Alist of available search types and search functions.")

(defun aurel-search-packages (type &rest vals)
  "Search for AUR packages and return results.
TYPE is a type of search - symbol from `aurel-search-type-alist'.
It defines a search function which is called with VALS as
arguments.
Returning value has a form of `aurel-list'."
  (let ((fun (cdr (assoc type aurel-search-type-alist))))
    (or fun
        (error "Wrong search type '%s'" type))
    (apply fun vals)))

(defun aurel-search-show-packages
    (search-type search-vals &optional buffer history)
  "Search for packages and show results in BUFFER.

See `aurel-search-packages' for the meaning of SEARCH-TYPE and
SEARCH-VALS.

See `aurel-show-packages' for the meaning of BUFFER and HISTORY."
  (aurel-show-packages
   (apply 'aurel-search-packages search-type search-vals)
   buffer history search-type search-vals))

(defun aurel-show-packages
    (packages &optional buffer history search-type search-vals)
  "Show PACKAGES in BUFFER.

PACKAGES should have a form of `aurel-list'.

If BUFFER is a buffer object, use it; if BUFFER is nil, use a
default buffer; otherwise, use a unique buffer.

If HISTORY is nil, do not save current item in history; if it is
`add', add item to history; if `replace', replace current item.
History item is a proper call of `aurel-show-packages' itself.

If SEARCH-TYPE and SEARCH-VALS are non-nils, they are used for
setting reverting action.  See `aurel-set-revert-action' for
details."
  (let ((count (length packages)))
    (when (> count 0)
      (if (and (= count 1)
               (or (eq search-type 'name-or-id)
                   (null aurel-list-single-package)))
          (let ((info (cdar packages)))
            ;; Add (maybe) AUR user info if the buffer is reverted or a
            ;; new info is shown; if we are moving by a history
            ;; (`history' is nil), do not add it.
            (and history
                 aurel-aur-user-package-info-check
                 (aurel-add-aur-user-package-info info))
            (aurel-info-show info
                             (if (bufferp buffer)
                                 buffer
                               (aurel-info-get-buffer-name buffer))))
        (aurel-list-show packages
                         (if (bufferp buffer)
                             buffer
                           (aurel-list-get-buffer-name buffer))))
      (when (and search-type search-vals)
        (when history
          (aurel-history-add
           (list (lambda (packages type vals)
                   (aurel-show-packages
                    packages (current-buffer) nil type vals))
                 packages search-type search-vals)
           (eq history 'replace)))
        (aurel-set-revert-action search-type search-vals)))
    (aurel-found-message packages search-type search-vals)))

(defvar aurel-found-messages
  '((name-or-id (0    "The package \"%s\" not found." "Packages not found.")
                (1    "The package \"%s\"."))
    (string     (0    "No packages matching %s.")
                (1    "A single package matching %s.")
                (many "%d packages matching %s."))
    (maintainer (0    "No packages by maintainer %s.")
                (1    "A single package by maintainer %s.")
                (many "%d packages by maintainer %s.")))
  "Alist used by `aurel-found-message'.")

(defun aurel-found-message (packages search-type search-vals)
  "Display a proper message about found PACKAGES.
SEARCH-TYPE and SEARCH-VALS are arguments for
`aurel-search-packages', by which the PACKAGES were found."
  (let* ((count (length packages))
         (found-key (if (> count 1) 'many count))
         (type-alist (cdr (assoc search-type aurel-found-messages)))
         (found-list (cdr (assoc found-key type-alist)))
         (msg (if (or (= 1 (length search-vals))
                      (null (cdr found-list)))
                  (car found-list)
                (cadr found-list)))
         (args (delq nil
                     (list
                      (and (eq found-key 'many) count)
                      (cond
                       ((eq search-type 'string)
                        (mapconcat (lambda (str) (concat "\"" str "\""))
                                   search-vals " "))
                       ((and (= count 1) (eq search-type 'name-or-id))
                        (aurel-get-param-val 'name (cdar packages)))
                       (t (car search-vals)))))))
    (and msg (apply 'message msg args))))


;;; History

(defvar-local aurel-history-stack-item nil
  "Current item of the history.
A list of the form (FUNCTION [ARGS ...]).
The item is used by calling (apply FUNCTION ARGS).")
(put 'aurel-history-stack-item 'permanent-local t)

(defvar-local aurel-history-back-stack nil
  "Stack (list) of visited items.
Each element of the list has a form of `aurel-history-stack-item'.")
(put 'aurel-history-back-stack 'permanent-local t)

(defvar-local aurel-history-forward-stack nil
  "Stack (list) of items visited with `aurel-history-back'.
Each element of the list has a form of `aurel-history-stack-item'.")
(put 'aurel-history-forward-stack 'permanent-local t)

(defvar aurel-history-size 0
  "Maximum number of items saved in history.
If 0, the history is disabled.")

(defun aurel-history-add (item &optional replace)
  "Add ITEM to history.
If REPLACE is non-nil, replace the current item instead of adding."
  (if replace
      (setq aurel-history-stack-item item)
    (and aurel-history-stack-item
         (push aurel-history-stack-item aurel-history-back-stack))
    (setq aurel-history-forward-stack nil
          aurel-history-stack-item item)
    (when (>= (length aurel-history-back-stack)
              aurel-history-size)
      (setq aurel-history-back-stack
            (cl-loop for elt in aurel-history-back-stack
                     for i from 1 to aurel-history-size
                     collect elt)))))

(defun aurel-history-goto (item)
  "Go to the ITEM of history.
ITEM should have the form of `aurel-history-stack-item'."
  (or (listp item)
      (error "Wrong value of history element"))
  (setq aurel-history-stack-item item)
  (apply (car item) (cdr item)))

(defun aurel-history-back ()
  "Go back to the previous element of history in the current buffer."
  (interactive)
  (or aurel-history-back-stack
      (user-error "No previous element in history"))
  (push aurel-history-stack-item aurel-history-forward-stack)
  (aurel-history-goto (pop aurel-history-back-stack)))

(defun aurel-history-forward ()
  "Go forward to the next element of history in the current buffer."
  (interactive)
  (or aurel-history-forward-stack
      (user-error "No next element in history"))
  (push aurel-history-stack-item aurel-history-back-stack)
  (aurel-history-goto (pop aurel-history-forward-stack)))


;;; Reverting buffers

(defcustom aurel-revert-no-confirm nil
  "If non-nil, do not ask to confirm for reverting aurel buffer."
  :type 'boolean
  :group 'aurel)

(defvar aurel-revert-action nil
  "Action for refreshing information in the current aurel buffer.
A list of the form (FUNCTION [ARGS ...]).
The action is performed by calling (apply FUNCTION ARGS).")

(defun aurel-revert-buffer (ignore-auto noconfirm)
  "Refresh information in the current aurel buffer.
The function is suitable for `revert-buffer-function'.
See `revert-buffer' for the meaning of IGNORE-AUTO and NOCONFIRM."
  (when (or aurel-revert-no-confirm
            noconfirm
            (y-or-n-p "Refresh current information? "))
    (apply (car aurel-revert-action)
           (cdr aurel-revert-action))))

(defun aurel-set-revert-action (search-type search-vals)
  "Set `aurel-revert-action' to a proper value.
SEARCH-TYPE and SEARCH-VALS are arguments for
`aurel-search-show-packages' by which refreshing information is
performed."
  (setq aurel-revert-action
        (list (lambda (type vals)
                (aurel-search-show-packages
                 type vals (current-buffer) 'replace))
              search-type search-vals)))


;;; Downloading

(defcustom aurel-download-directory temporary-file-directory
  "Default directory for downloading AUR packages."
  :type 'directory
  :group 'aurel)

(defcustom aurel-directory-prompt "Download to: "
  "Default directory prompt for downloading AUR packages."
  :type 'string
  :group 'aurel)

(defvar aurel-download-functions
  '(aurel-download aurel-download-unpack aurel-download-unpack-dired
    aurel-download-unpack-pkgbuild aurel-download-unpack-eshell)
  "List of available download functions.")

(defun aurel-download-get-defcustom-type ()
  "Return `defcustom' type for selecting a download function."
  `(radio ,@(mapcar (lambda (fun) (list 'function-item fun))
                    aurel-download-functions)
          (function :tag "Other function")))

(defun aurel-download (url dir)
  "Download AUR package from URL to a directory DIR.
Return a path to the downloaded file."
  ;; Is there a simpler way to download a file?
  (let ((file-name-handler-alist
         (cons (cons url-handler-regexp 'url-file-handler)
               file-name-handler-alist)))
    (with-temp-buffer
      (insert-file-contents-literally url)
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
  (let ((file-name-handler-alist
         (cons (cons url-handler-regexp 'url-file-handler)
               file-name-handler-alist)))
    (with-temp-buffer
      (insert-file-contents url)
      (setq default-directory dir)
      (let ((file (expand-file-name (url-file-nondirectory url) dir)))
        (write-file file))
      (tar-mode)
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
        (expand-file-name tar-dir dir)))))

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

(defun aurel-get-fields-string (args)
  "Return string of names and values from ARGS alist.
Each association of ARGS has a form: (NAME . VALUE).
If NAME and VALUE are not strings, they are converted to strings
with `prin1-to-string'.
Returning string has a form: \"NAME=VALUE&...\"."
  (cl-flet ((hexify (arg)
                    (url-hexify-string
                     (if (stringp arg) arg (prin1-to-string arg)))))
    (mapconcat (lambda (arg)
                 (concat (hexify (car arg))
                         "="
                         (hexify (cdr arg))))
               args
               "&")))

(defun aurel-get-multi-args-rpc-url (type args &optional type-name arg-name)
  "Return URL for getting info about AUR packages.
TYPE is the name of an allowed method.
ARGS is a list of arguments to the call.
TYPE-NAME is the name of a type field (\"type\" by default).
ARG-NAME is the name of an arg field (\"arg[]\" by default)."
  (or type-name
      (setq type-name "type"))
  (or arg-name
      (setq arg-name "arg[]"))
  (let ((fields (cons
                 (cons type-name type)
                 (mapcar (lambda (arg) (cons arg-name arg))
                         args))))
    (url-expand-file-name
     (concat "rpc.php?" (aurel-get-fields-string fields))
     aurel-aur-base-url)))

(defun aurel-get-rpc-url (type arg)
  "Return URL for getting info about AUR packages.
TYPE is the name of an allowed method.
ARG is the argument to the call."
  (aurel-get-multi-args-rpc-url type (list arg) "type" "arg"))

(defun aurel-get-package-multiinfo-url (packages)
  "Return URL for getting info about PACKAGES.
Each package should be a string (package name)."
  (aurel-get-multi-args-rpc-url "multiinfo" packages))

(defun aurel-get-package-info-url (package)
  "Return URL for getting info about a PACKAGE.
PACKAGE can be either a string (name) or a number (ID)."
  (aurel-get-rpc-url "info" package))

(defun aurel-get-package-search-url (str)
  "Return URL for searching a package by string STR."
  (aurel-get-rpc-url "search" str))

(defun aurel-get-maintainer-search-url (str)
  "Return URL for searching a maintainer by string STR."
  (aurel-get-rpc-url "msearch" str))

(defun aurel-get-maintainer-account-url (maintainer)
  "Return URL for MAINTAINER's AUR account."
  (url-expand-file-name (concat "account/" maintainer)
                        aurel-aur-base-url))

(defun aurel-get-aur-package-url (package)
  "Return AUR URL of a PACKAGE."
  (url-expand-file-name (concat "packages/" package)
                        aurel-aur-base-url))

(defun aurel-get-package-base-url (package-base)
  "Return AUR URL of a PACKAGE-BASE."
  (url-expand-file-name (concat "pkgbase/" package-base)
                        aurel-aur-base-url))

(defun aurel-get-package-action-url (package-base action)
  "Return URL for the PACKAGE-BASE ACTION."
  (concat (aurel-get-package-base-url package-base)
          "/" action))


;;; UI

(defvar aurel-package-info-history nil
  "A history list for `aurel-package-info'.")

(defvar aurel-package-search-history nil
  "A history list for `aurel-package-search'.")

(defvar aurel-maintainer-search-history nil
  "A history list for `aurel-maintainer-search'.")

;;;###autoload
(defun aurel-package-info (name-or-id &optional arg)
  "Display information about AUR package NAME-OR-ID.
NAME-OR-ID may be a string or a number.
The buffer for showing results is defined by `aurel-info-buffer-name'.
With prefix (if ARG is non-nil), show results in a new info buffer."
  (interactive
   (list (read-string "Name or ID: "
                      nil 'aurel-package-info-history)
         current-prefix-arg))
  (aurel-search-show-packages
   'name-or-id (list name-or-id) arg 'add))

;;;###autoload
(defun aurel-package-search (string &optional arg)
  "Search for AUR packages matching STRING.

STRING can be a string of multiple words separated by spaces.  To
search for a string containing spaces, quote it with double
quotes.  For example, the following search is allowed:

  \"python library\" plot

The buffer for showing results is defined by
`aurel-list-buffer-name'.  With prefix (if ARG is non-nil), show
results in a new buffer."
  (interactive
   (list (read-string "Search by name/description: "
                      nil 'aurel-package-search-history)
         current-prefix-arg))
  (aurel-search-show-packages
   'string (split-string-and-unquote string) arg 'add))

;;;###autoload
(defun aurel-maintainer-search (name &optional arg)
  "Search for AUR packages by maintainer NAME.
The buffer for showing results is defined by `aurel-list-buffer-name'.
With prefix (if ARG is non-nil), show results in a new buffer."
  (interactive
   (list (read-string "Search by maintainer: "
                      nil 'aurel-maintainer-search-history)
         current-prefix-arg))
  (aurel-search-show-packages
   'maintainer (list name) arg 'add))

;;;###autoload
(defun aurel-installed-packages (&optional arg)
  "Display information about AUR packages installed in the system.
The buffer for showing results is defined by `aurel-list-buffer-name'.
With prefix (if ARG is non-nil), show results in a new buffer."
  (interactive "P")
  (aurel-search-show-packages
   'name-or-id (aurel-get-foreign-packages) arg 'add))


;;; Package list

(defgroup aurel-list nil
  "Buffer with a list of AUR packages."
  :group 'aurel)

(defface aurel-list-marked
  '((t :inherit dired-marked))
  "Face used for the marked packages."
  :group 'aurel-list)

(defcustom aurel-list-buffer-name "*AUR Package List*"
  "Default name of the buffer with a list of AUR packages."
  :type 'string
  :group 'aurel-list)

(defcustom aurel-list-mode-name "AURel-List"
  "Default name of `aurel-list-mode', displayed in the mode line."
  :type 'string
  :group 'aurel-list)

(defcustom aurel-list-download-function 'aurel-download-unpack
  "Function used for downloading a single AUR package from list buffer.
It should accept 2 arguments: URL of a downloading file and a
destination directory."
  :type (aurel-download-get-defcustom-type)
  :group 'aurel-list)

(defcustom aurel-list-multi-download-function 'aurel-download-unpack
  "Function used for downloading multiple AUR packages from list buffer.
It should accept 2 arguments: URL of a downloading file and a
destination directory."
  :type (aurel-download-get-defcustom-type)
  :group 'aurel-list)

(defcustom aurel-list-multi-download-no-confirm nil
  "If non-nil, do not ask to confirm if multiple packages are downloaded."
  :type 'boolean
  :group 'aurel-list)

(defcustom aurel-list-history-size 10
  "Maximum number of items saved in history of package list buffer.
If 0, the history is disabled."
  :type 'integer
  :group 'aurel-list)

(defvar aurel-list-column-name-alist
  '((installed-version . "Installed"))
  "Alist of parameter names used as titles for columns.
Each association is a cons of parameter symbol and column name.
If no parameter is not found in this alist, the value from
`aurel-param-description-alist' is used for a column name.")

(defvar aurel-list-column-value-alist
  '((name              . aurel-list-get-name)
    (maintainer        . aurel-list-get-maintainer)
    (installed-version . aurel-list-get-installed-version))
  "Alist for parameter values inserted in columns.
Each association is a cons of parameter symbol from
`aurel-param-description-alist' and a function returning a value
that will be inserted.  The function should take a package info
of the form of `aurel-info' as an argument.")

(defvar aurel-list nil
  "Alist with packages info.

Car of each assoc is a package ID (number).
Cdr - is alist of package info of the form of `aurel-info'.")

(defvar aurel-list-filters nil
  "List of filter functions applied to a package info.

Each filter function should accept a single argument - info alist
with package parameters and should return info alist or
nil (which means: do not display this package).  These filters
are applied before displaying the list of packages.")

(defvar aurel-list-available-filters
  '(aurel-list-filter-maintained aurel-list-filter-unmaintained
    aurel-list-filter-outdated aurel-list-filter-not-outdated
    aurel-list-filter-match-regexp aurel-list-filter-not-match-regexp
    aurel-list-filter-different-versions aurel-list-filter-same-versions)
  "List of commands that can be called for filtering a package list.
Used by `aurel-list-enable-filter'.
Each function should make a proper filter function and should
take one argument and pass those to `aurel-list-apply-filter'.")

(defvar aurel-list-marks nil
  "Alist of current marks.
Each association is a cons cell of a package ID and overlay used
to highlight a line with this package.")

(defvar aurel-list-votes-column nil
  "The number of column with votes in the current tabulated-list.")

(defvar aurel-list-column-format
  '((name 20 t)
    (version 12 nil)
    (installed-version 12 t)
    (maintainer 13 t)
    ;; We cannot use simple sort for votes as they will be sorted as
    ;; strings, e.g.: (1, 13, 2, 200, 3) instead of (1, 2, 3, 13, 200).
    ;; So we use a special function to compare votes as numbers.
    (votes 8 aurel-list-sort-by-votes)
    (description 30 nil))
  "List specifying columns used in the buffer with a list of packages.
Each element of the list should have the form (NAME WIDTH SORT . PROPS).
NAME is a parameter symbol from `aurel-param-description-alist'.
For the meaning of WIDTH, SORT and PROPS, see `tabulated-list-format'.")

(defvar aurel-list-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'aurel-list-enable-filter)
    (define-key map (kbd "d") 'aurel-list-disable-filters)
    (define-key map (kbd "v") 'aurel-list-filter-same-versions)
    (define-key map (kbd "V") 'aurel-list-filter-different-versions)
    (define-key map (kbd "m") 'aurel-list-filter-unmaintained)
    (define-key map (kbd "M") 'aurel-list-filter-maintained)
    (define-key map (kbd "o") 'aurel-list-filter-outdated)
    (define-key map (kbd "O") 'aurel-list-filter-not-outdated)
    (define-key map (kbd "r") 'aurel-list-filter-not-match-regexp)
    (define-key map (kbd "R") 'aurel-list-filter-match-regexp)
    map)
  "Keymap with filter commands for `aurel-list-mode'.")
(fset 'aurel-list-filter-map aurel-list-filter-map)

(defvar aurel-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'aurel-list-describe-package)
    (define-key map (kbd "d")   'aurel-list-download-package)
    (define-key map (kbd "l")   'aurel-history-back)
    (define-key map (kbd "r")   'aurel-history-forward)
    (define-key map (kbd "m")   'aurel-list-mark)
    (define-key map (kbd "u")   'aurel-list-unmark)
    (define-key map (kbd "M")   'aurel-list-mark-all)
    (define-key map (kbd "U")   'aurel-list-unmark-all)
    (define-key map (kbd "DEL") 'aurel-list-unmark-backward)
    (define-key map (kbd "s")   'aurel-list-sort)
    (define-key map (kbd "S")   'aurel-list-sort)
    (define-key map (kbd "f")   'aurel-list-filter-map)
    (define-key map (kbd "g")   'revert-buffer)
    map)
  "Keymap for `aurel-list-mode'.")

(defun aurel-list-sort-by-votes (a b)
  "Compare tabulated entries A and B by the number of votes.
It is a sort predicate for `tabulated-list-format'.
Return non-nil, if A has more votes than B."
  (cl-flet ((votes (entry)
                   (string-to-number (aref (cadr entry)
                                           aurel-list-votes-column))))
    (> (votes a) (votes b))))

(defun aurel-list-get-buffer-name (&optional unique)
  "Return a name of a list buffer.
If UNIQUE is non-nil, make the name unique."
  (if unique
      (generate-new-buffer aurel-list-buffer-name)
    aurel-list-buffer-name))

(define-derived-mode aurel-list-mode
  tabulated-list-mode aurel-list-mode-name
  "Major mode for browsing AUR packages.

\\{aurel-list-mode-map}"
  (make-local-variable 'aurel-list)
  (make-local-variable 'aurel-list-filters)
  (make-local-variable 'aurel-list-marks)
  (make-local-variable 'aurel-revert-action)
  (setq-local revert-buffer-function 'aurel-revert-buffer)
  (setq-local aurel-history-size aurel-list-history-size)
  (setq-local aurel-list-votes-column
              (cl-loop
               for col-spec in aurel-list-column-format
               for i from 0
               until (eq (car col-spec) 'votes)
               finally return i))
  (setq default-directory aurel-download-directory)
  (setq tabulated-list-format
        (apply #'vector
               (mapcar
                (lambda (col-spec)
                  (let ((name (car col-spec)))
                    (cons (or (cdr (assoc name aurel-list-column-name-alist))
                              (aurel-get-param-description name))
                          (cdr col-spec))))
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
      (aurel-list-show-in-current-buffer list))
    (pop-to-buffer-same-window buf)))

(defun aurel-list-show-in-current-buffer (list)
  "Display a LIST of packages in current buffer.
LIST should have the form of `aurel-list'."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (aurel-list-mode)
  (setq aurel-list list)
  (aurel-list-print))

(defun aurel-list-print (&optional list)
  "Filter and print package LIST into the current tabulated-list buffer.
If LIST is nil, use `aurel-list'."
  ;; TODO restore marks for the packages that survive filtering
  (aurel-list-unmark-all)
  (setq tabulated-list-entries
        (aurel-list-get-entries
         (aurel-list-apply-filters (or list aurel-list))))
  (tabulated-list-print))

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
                    (mapcar
                     (lambda (col-spec)
                       (let* ((param (car col-spec))
                              (fun (cdr (assq param
                                              aurel-list-column-value-alist))))
                         (if fun
                             (funcall fun info)
                           (aurel-get-string
                            (aurel-get-param-val param info)))))
                     aurel-list-column-format)))))
   list))

(defun aurel-list-get-name (info)
  "Return name of the package from a package INFO.
Colorize the name with `aurel-info-outdated' if the package is
out of date."
  (aurel-get-string
   (aurel-get-param-val 'name info)
   (when (aurel-get-param-val 'outdated info)
     'aurel-info-outdated)))

(defun aurel-list-get-maintainer (info)
  "Return maintainer name from a package INFO."
  (or (aurel-get-param-val 'maintainer info)
      aurel-empty-string))

(defun aurel-list-get-installed-version (info)
  "Return installed version from a package INFO."
  (or (aurel-get-param-val 'installed-version info)
      aurel-empty-string))

(defun aurel-list-get-current-id ()
  "Return ID of the current package."
  (or (tabulated-list-get-id)
      (user-error "No package here")))

(defun aurel-list-get-package-info (&optional id)
  "Return info for a package with ID or for the current package."
  (or id
      (setq id (aurel-list-get-current-id)))
  (or (cdr (assoc id aurel-list))
      (error "No package with ID %s in aurel-list" id)))

(defun aurel-list-describe-package (&optional arg)
  "Describe the current package.
With prefix (if ARG is non-nil), show results in a new info buffer."
  (interactive "P")
  (let* ((id (aurel-list-get-current-id))
         (info (aurel-list-get-package-info id))
         (list (list (cons id info))))
    (aurel-show-packages list arg 'add 'name-or-id (list id))))

(defun aurel-list-download-package ()
  "Download marked packages or the current package if nothing is marked.

With prefix, prompt for a directory with `aurel-directory-prompt'
to save the package; without prefix, save to
`aurel-download-directory' without prompting.

Use `aurel-list-download-function' if a single package is
downloaded or `aurel-list-multi-download-function' otherwise."
  (interactive)
  (or (derived-mode-p 'aurel-list-mode)
      (user-error "Current buffer is not in aurel-list-mode"))
  (let ((dir (if current-prefix-arg
                 (read-directory-name aurel-directory-prompt
                                      aurel-download-directory)
               aurel-download-directory))
        (count (length aurel-list-marks))
        (ids (mapcar #'car aurel-list-marks)))
    (if (> count 1)
        (when (or aurel-list-multi-download-no-confirm
                  (y-or-n-p (format "Download %d marked packages? "
                                    count)))
          (mapcar (lambda (id)
                    (funcall aurel-list-multi-download-function
                             (aurel-get-param-val
                              'pkg-url (aurel-list-get-package-info id))
                             dir))
                  ids))
      (funcall aurel-list-download-function
               (aurel-get-param-val
                'pkg-url (aurel-list-get-package-info (car ids)))
               dir))))

(defun aurel-list-sort (&optional n)
  "Sort aurel list entries by the column at point.
With a numeric prefix argument N, sort the Nth column.
Same as `tabulated-list-sort', but also restore marks after sorting."
  (interactive "P")
  (let ((marks (mapcar #'car aurel-list-marks)))
    (aurel-list-unmark-all)
    (tabulated-list-sort n)
    (when marks
      (aurel-list-mark-packages marks))))

;;; Marking packages

(defun aurel-list-mark ()
  "Mark a package for downloading and move to the next line."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (unless (overlays-at beg)
          (let ((overlay (make-overlay beg end)))
            (overlay-put overlay 'face 'aurel-list-marked)
            (add-to-list 'aurel-list-marks
                         (cons id overlay))))))
    (forward-line)))

(defun aurel-list-mark-packages (ids)
  "Mark specified packages.
IDS is a list of packages ID to mark.  If IDS is t, mark all packages."
  (save-excursion
    (goto-char (point-min))
    (while (not (= (point) (point-max)))
      (if (or (eq ids t)
              (member (tabulated-list-get-id) ids))
          (aurel-list-mark)
        (forward-line)))))

(defun aurel-list-mark-all ()
  "Mark all packages for downloading."
  (interactive)
  (aurel-list-mark-packages t))

(defun aurel-list--unmark ()
  "Unmark a package on the current line."
  (let ((id (tabulated-list-get-id)))
    (setq aurel-list-marks
          (cl-delete-if (lambda (assoc)
                          (when (equal id (car assoc))
                            (delete-overlay (cdr assoc))
                            t))
                        aurel-list-marks))))

(defun aurel-list-unmark ()
  "Unmark a package and move to the next line."
  (interactive)
  (aurel-list--unmark)
  (forward-line))

(defun aurel-list-unmark-backward ()
  "Move up one line and unmark a package on that line."
  (interactive)
  (forward-line -1)
  (aurel-list--unmark))

(defun aurel-list-unmark-all ()
  "Unmark all packages."
  (interactive)
  (dolist (assoc aurel-list-marks)
    (delete-overlay (cdr assoc)))
  (setq aurel-list-marks nil))

;;; Filtering package list

(defun aurel-list-apply-filters (list &optional filters)
  "Apply FILTERS to the LIST of packages.

LIST should have the form of `aurel-list'.
If FILTERS is nil, use `aurel-list-filters'.

Each package info from LIST is passed as an argument to the first
function from FILTERS, the returned result is passed to the
second function from that list and so on.  If one of the FILTERS
returns nil, this package info is not passed (do not call the
rest filters in this case).

Return a list containing all passed packages info."
  (if (setq filters (or filters aurel-list-filters))
      (cl-remove-if-not
       (lambda (package)
         (aurel-apply-filters (cdr package) filters))
       list)
    list))

(defun aurel-list-apply-filter (filter &optional replace)
  "Apply FILTER to the current package list and print results.
If REPLACE is nil, add FILTER to the existing ones; if it is
non-nil, remove other filters and make FILTER the only active
one."
  (if replace
      (setq aurel-list-filters (list filter))
    (add-to-list 'aurel-list-filters filter))
  (aurel-list-print))

(defun aurel-list-enable-filter (arg)
  "Prompt for a function for filtering package list and call it.
Choose candidates from `aurel-list-available-filters'.
If ARG is non-nil (with prefix), make selected filter the only
active one (remove other filters)."
  (interactive "P")
  (let ((fun (intern (completing-read
                      (if current-prefix-arg
                          "Add filter: "
                        "Enable filter: ")
                      aurel-list-available-filters))))
    (or (fboundp fun)
        (error "Wrong function %s" fun))
    (funcall fun arg)))

(defun aurel-list-disable-filters ()
  "Disable all current filters and redisplay packages."
  (interactive)
  (setq aurel-list-filters nil)
  (aurel-list-print))

(defun aurel-list-filter-maintained (arg)
  "Filter current list by hiding maintained packages.
See `aurel-list-enable-filter' for the meaning of ARG."
  (interactive "P")
  (aurel-list-apply-filter
   (lambda (info)
     (unless (aurel-get-param-val 'maintainer info)
       info))
   arg))

(defun aurel-list-filter-unmaintained (arg)
  "Filter current list by hiding unmaintained packages.
See `aurel-list-enable-filter' for the meaning of ARG."
  (interactive "P")
  (aurel-list-apply-filter
   (lambda (info)
     (when (aurel-get-param-val 'maintainer info)
       info))
   arg))

(defun aurel-list-filter-outdated (arg)
  "Filter current list by hiding outdated packages.
See `aurel-list-enable-filter' for the meaning of ARG."
  (interactive "P")
  (aurel-list-apply-filter
   (lambda (info)
     (unless (aurel-get-param-val 'outdated info)
       info))
   arg))

(defun aurel-list-filter-not-outdated (arg)
  "Filter current list by hiding not outdated packages.
See `aurel-list-enable-filter' for the meaning of ARG."
  (interactive "P")
  (aurel-list-apply-filter
   (lambda (info)
     (when (aurel-get-param-val 'outdated info)
       info))
   arg))

(defun aurel-list-filter-same-versions (arg)
  "Hide packages with the same installed and available AUR versions.
See `aurel-list-enable-filter' for the meaning of ARG."
  (interactive "P")
  (aurel-list-apply-filter
   (lambda (info)
     (unless (equal (aurel-get-param-val 'version info)
                    (aurel-get-param-val 'installed-version info))
       info))
   arg))

(defun aurel-list-filter-different-versions (arg)
  "Hide packages with different installed and available AUR versions.
See `aurel-list-enable-filter' for the meaning of ARG."
  (interactive "P")
  (aurel-list-apply-filter
   (lambda (info)
     (when (equal (aurel-get-param-val 'version info)
                  (aurel-get-param-val 'installed-version info))
       info))
   arg))

(defun aurel-list-filter-match-regexp (arg)
  "Hide packages with names or descriptions matching prompted regexp.
See `aurel-list-enable-filter' for the meaning of ARG."
  (interactive "P")
  (let ((re (read-regexp "Hide packages matching regexp: ")))
    (aurel-list-apply-filter
     `(lambda (info)
        (unless (or (string-match-p ,re (aurel-get-param-val 'name info))
                    (string-match-p ,re (aurel-get-param-val 'description info)))
          info))
     arg)))

(defun aurel-list-filter-not-match-regexp (arg)
  "Hide packages with names or descriptions not matching prompted regexp.
See `aurel-list-enable-filter' for the meaning of ARG."
  (interactive "P")
  (let ((re (read-regexp "Hide packages not matching regexp: ")))
    (aurel-list-apply-filter
     `(lambda (info)
        (when (or (string-match-p ,re (aurel-get-param-val 'name info))
                  (string-match-p ,re (aurel-get-param-val 'description info)))
          info))
     arg)))


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
  '((t :inherit button))
  "Face used for a maintainer of a package."
  :group 'aurel-info)

(defface aurel-info-url
  '((t :inherit button))
  "Face used for URLs."
  :group 'aurel-info)

(defface aurel-info-version
  '((t :inherit font-lock-builtin-face))
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

(defface aurel-info-voted-mark
  '((t :inherit aurel-info-voted))
  "Face used for `aurel-info-voted-mark' string."
  :group 'aurel-info)

(defface aurel-info-outdated
  '((t :inherit font-lock-warning-face))
  "Face used if a package is out of date."
  :group 'aurel-info)

(defface aurel-info-not-outdated
  '((t))
  "Face used if a package is not out of date."
  :group 'aurel-info)

(defface aurel-info-voted
  '((default :weight bold)
    (((class color) (min-colors 88) (background light))
     :foreground "ForestGreen")
    (((class color) (min-colors 88) (background dark))
     :foreground "PaleGreen")
    (((class color) (min-colors 8))
     :foreground "green")
    (t :underline t))
  "Face used if a package is voted."
  :group 'aurel-info)

(defface aurel-info-not-voted
  '((t))
  "Face used if a package is not voted."
  :group 'aurel-info)

(defface aurel-info-subscribed
  '((t :inherit aurel-info-voted))
  "Face used if a package is subscribed."
  :group 'aurel-info)

(defface aurel-info-not-subscribed
  '((t :inherit aurel-info-not-voted))
  "Face used if a package is not subscribed."
  :group 'aurel-info)

(defface aurel-info-date
  '((t :inherit font-lock-constant-face))
  "Face used for dates."
  :group 'aurel-info)

(defface aurel-info-size
  '((t :inherit font-lock-variable-name-face))
  "Face used for size of installed package."
  :group 'aurel-info)

(defface aurel-info-architecture
  '((t))
  "Face used for 'Architecture' parameter."
  :group 'aurel-info)

(defface aurel-info-provides
  '((t :inherit font-lock-function-name-face))
  "Face used for 'Provides' parameter."
  :group 'aurel-info)

(defface aurel-info-replaces
  '((t :inherit aurel-info-provides))
  "Face used for 'Replaces' parameter."
  :group 'aurel-info)

(defface aurel-info-conflicts
  '((t :inherit aurel-info-provides))
  "Face used for 'Conflicts With' parameter."
  :group 'aurel-info)

(defface aurel-info-depends
  '((t))
  "Face used for 'Depends On' parameter."
  :group 'aurel-info)

(defface aurel-info-depends-opt
  '((t :inherit aurel-info-depends))
  "Face used for 'Optional Deps' parameter."
  :group 'aurel-info)

(defface aurel-info-required
  '((t))
  "Face used for 'Required By' parameter."
  :group 'aurel-info)

(defface aurel-info-optional-for
  '((t :inherit aurel-info-required))
  "Face used for 'Optional For' parameter."
  :group 'aurel-info)

(defcustom aurel-info-buffer-name "*AUR Package Info*"
  "Default name of the buffer with information about an AUR package."
  :type 'string
  :group 'aurel-info)

(defcustom aurel-info-mode-name "AURel-Info"
  "Default name of `aurel-info-mode', displayed in the mode line."
  :type 'string
  :group 'aurel-info)

(defcustom aurel-info-ignore-empty-vals nil
  "If non-nil, do not display empty values of package parameters."
  :type 'boolean
  :group 'aurel-info)

(defcustom aurel-info-format "%-16s: "
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
  :type (aurel-download-get-defcustom-type)
  :group 'aurel-info)

(defcustom aurel-info-history-size 100
  "Maximum number of items saved in history of package info buffer.
If 0, the history is disabled."
  :type 'integer
  :group 'aurel-info)

(defcustom aurel-info-voted-mark "*"
  "String inserted after the number of votes in info buffer.
See `aurel-info-display-voted-mark' for details."
  :type 'string
  :group 'aurel-info)

(defcustom aurel-info-display-voted-mark t
  "If non-nil, display `aurel-info-voted-mark' in info buffer.
It is displayed only if a package is voted by you (this
information is available if `aurel-aur-user-package-info-check'
is non-nil)."
  :type 'boolean
  :group 'aurel-info)

(defcustom aurel-info-installed-package-string
  "\n\nThe package is installed:\n\n"
  "String inserted in info buffer if a package is installed.
It is inserted after printing info from AUR and before info from pacman."
  :type 'string
  :group 'aurel-info)

(defcustom aurel-info-aur-user-string
  "\n"
  "String inserted before printing info specific for AUR user."
  :type 'string
  :group 'aurel-info)

(defcustom aurel-info-show-maintainer-account t
  "If non-nil, display a link to maintainer's AUR account."
  :type 'boolean
  :group 'aurel-info)

(defvar aurel-info-insert-params-alist
  '((id                . aurel-info-id)
    (base-id           . aurel-info-id)
    (name              . aurel-info-name)
    (base-name         . aurel-info-name)
    (maintainer        . aurel-info-insert-maintainer)
    (version           . aurel-info-version)
    (installed-version . aurel-info-version)
    (category          . aurel-info-category)
    (license           . aurel-info-license)
    (votes             . aurel-info-insert-votes)
    (first-date        . aurel-info-date)
    (last-date         . aurel-info-date)
    (install-date      . aurel-info-date)
    (build-date        . aurel-info-date)
    (description       . aurel-info-description)
    (outdated          . aurel-info-insert-outdated)
    (voted             . aurel-info-insert-voted)
    (subscribed        . aurel-info-insert-subscribed)
    (pkg-url           . aurel-info-insert-url)
    (home-url          . aurel-info-insert-url)
    (aur-url           . aurel-info-insert-aur-url)
    (base-url          . aurel-info-insert-base-url)
    (architecture      . aurel-info-architecture)
    (provides          . aurel-info-provides)
    (replaces          . aurel-info-replaces)
    (conflicts         . aurel-info-conflicts)
    (depends           . aurel-info-depends)
    (depends-opt       . aurel-info-depends-opt)
    (required          . aurel-info-required)
    (optional-for      . aurel-info-optional-for)
    (installed-size    . aurel-info-size))
  "Alist for parameters inserted into info buffer.
Car of each assoc is a symbol from `aurel-param-description-alist'.
Cdr is a symbol for inserting a value of a parameter.  If the
symbol is a face name, it is used for the value; if it is a function,
it is called with the value of the parameter.")

(defvar aurel-info-parameters
  '(id name version maintainer description home-url aur-url base-url
    license category votes outdated first-date last-date)
  "List of parameters displayed in package info buffer.
Each parameter should be a symbol from `aurel-param-description-alist'.
The order of displayed parameters is the same as in this list.
If nil, display all parameters with no particular order.")

(defvar aurel-info-installed-parameters
  '(installed-version architecture installed-size provides depends
    depends-opt required optional-for conflicts replaces packager
    build-date install-date script validated)
  "List of parameters of an installed package displayed in info buffer.
Each parameter should be a symbol from `aurel-param-description-alist'.
The order of displayed parameters is the same as in this list.
If nil, display all parameters with no particular order.")

(defvar aurel-info-aur-user-parameters
  '(voted subscribed)
  "List of parameters specific for AUR user displayed in info buffer.
Each parameter should be a symbol from `aurel-param-description-alist'.
The order of displayed parameters is the same as in this list.
If nil, display all parameters with no particular order.")

(defvar aurel-info nil
  "Alist with package info.

Car of each assoc is a symbol from `aurel-param-description-alist'.
Cdr - is a value of that parameter.")

(defvar aurel-info-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap button-buffer-map
                                                 special-mode-map))
    (define-key map (kbd "d") 'aurel-info-download-package)
    (define-key map (kbd "l") 'aurel-history-back)
    (define-key map (kbd "r") 'aurel-history-forward)
    (define-key map (kbd "v") 'aurel-info-vote-unvote)
    (define-key map (kbd "s") 'aurel-info-subscribe-unsubscribe)
    map)
  "Keymap for `aurel-info-mode'.")

(defun aurel-info-get-buffer-name (&optional unique)
  "Return a name of an info buffer.
If UNIQUE is non-nil, make the name unique."
  (if unique
      (generate-new-buffer aurel-info-buffer-name)
    aurel-info-buffer-name))

(define-derived-mode aurel-info-mode special-mode aurel-info-mode-name
  "Major mode for displaying information about an AUR package.

\\{aurel-info-mode-map}"
  (make-local-variable 'aurel-info)
  (make-local-variable 'aurel-revert-action)
  (setq-local revert-buffer-function 'aurel-revert-buffer)
  (setq-local aurel-history-size aurel-info-history-size)
  (setq default-directory aurel-download-directory))

(defun aurel-info-show (info &optional buffer)
  "Display package information INFO in BUFFER.
INFO should have the form of `aurel-info'.
If BUFFER is nil, use (create if needed) buffer with the name
`aurel-info-buffer-name'."
  (let ((buf (get-buffer-create
              (or buffer aurel-info-buffer-name))))
    (with-current-buffer buf
      (aurel-info-show-in-current-buffer info))
    (pop-to-buffer-same-window buf)))

(defun aurel-info-show-in-current-buffer (info)
  "Display package information INFO in current buffer.
INFO should have the form of `aurel-info'."
  (aurel-info-mode)
  (setq aurel-info info)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (apply 'aurel-info-print
           info aurel-info-parameters)
    (when (assoc 'voted info)
      (insert aurel-info-aur-user-string)
      (apply 'aurel-info-print
             info aurel-info-aur-user-parameters))
    (when (aurel-get-param-val 'installed-name info)
      (insert aurel-info-installed-package-string)
      (apply 'aurel-info-print
             info aurel-info-installed-parameters)))
  (goto-char (point-min)))

(defun aurel-info-print (info &rest params)
  "Insert (pretty print) package INFO into current buffer.
Each element from PARAMS is a parameter to insert (symbol from
`aurel-param-description-alist')."
  (mapc (lambda (param)
          (aurel-info-print-param
           param (aurel-get-param-val param info)))
        params))

(defun aurel-info-print-param (param val)
  "Insert description and value VAL of a parameter PARAM at point.
PARAM is a symbol from `aurel-param-description-alist'.
Use `aurel-info-format' to format descriptions of parameters."
  (unless (and aurel-info-ignore-empty-vals
               (equal val aurel-none-string))
    (let ((desc (aurel-get-param-description param))
          (insert-val (cdr (assoc param
                                  aurel-info-insert-params-alist))))
      (insert (format aurel-info-format desc))
      (if (functionp insert-val)
          (funcall insert-val val)
        (aurel-info-insert-val
         val (and (facep insert-val) insert-val)))
      (insert "\n"))))

(defun aurel-info-insert-votes (votes)
  "Insert the number of VOTES at point.
If `aurel-info-display-voted-mark' is non-nil, insert
`aurel-info-voted-mark' after."
  (aurel-info-insert-val votes 'aurel-info-votes)
  (and aurel-info-display-voted-mark
       (aurel-get-param-val 'voted aurel-info)
       (aurel-info-insert-val aurel-info-voted-mark
                              'aurel-info-voted-mark)))

(defun aurel-info-insert-maintainer (name)
  "Make button from maintainer NAME and insert it at point."
  (if (null name)
      (insert aurel-empty-string)
    (insert-button
     name
     'face 'aurel-info-maintainer
     'action (lambda (btn)
               (aurel-maintainer-search (button-label btn)
                                        current-prefix-arg))
     'follow-link t
     'help-echo "mouse-2, RET: Find the packages by this maintainer")
    (when aurel-info-show-maintainer-account
      (insert "\n"
              (format aurel-info-format ""))
      (aurel-info-insert-url (aurel-get-maintainer-account-url name)))))

(defun aurel-info-insert-aur-url (url)
  "Insert URL of the AUR package."
  (aurel-info-insert-url
   (aurel-get-aur-package-url
    (aurel-get-param-val 'name aurel-info))))

(defun aurel-info-insert-base-url (url)
  "Insert URL of the AUR package base."
  (aurel-info-insert-url
   (aurel-get-package-base-url
    (aurel-get-param-val 'base-name aurel-info))))

(defun aurel-info-insert-url (url)
  "Make button from URL and insert it at point."
  (insert-button
   url
   'face 'aurel-info-url
   'action (lambda (btn) (browse-url (button-label btn)))
   'follow-link t
   'help-echo "mouse-2, RET: Browse URL"))

(defun aurel-info-insert-boolean (val &optional t-face nil-face)
  "Insert boolean value VAL at point.
If VAL is t, use T-FACE; if VAL is nil, use NIL-FACE.
If VAL is not boolean, insert it as is."
  (let ((face (and (booleanp val)
                   (if val t-face nil-face))))
    (insert (aurel-get-string val face))))

(defun aurel-info-insert-outdated (val)
  "Insert value VAL of the `outdated' parameter at point."
  (aurel-info-insert-boolean
   val 'aurel-info-outdated 'aurel-info-not-outdated))

(defun aurel-info-insert-voted (val)
  "Insert value VAL of the `voted' parameter at point."
  (aurel-info-insert-boolean
   val 'aurel-info-voted 'aurel-info-not-voted))

(defun aurel-info-insert-subscribed (val)
  "Insert value VAL of the `subscribed' parameter at point."
  (aurel-info-insert-boolean
   val 'aurel-info-subscribed 'aurel-info-not-subscribed))

(defun aurel-info-get-filled-string (str col)
  "Return string by filling a string STR.
COL controls the width for filling."
  (with-temp-buffer
    (insert str)
    (let ((fill-column col)) (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun aurel-info-insert-strings (strings &optional face)
  "Insert STRINGS at point.
Each string is inserted on a new line after an empty string
formatted with `aurel-info-format'.
If FACE is non-nil, propertize inserted lines with this FACE."
  (dolist (str strings)
    (insert "\n"
            (format aurel-info-format "")
            (aurel-get-string str face))))

(defun aurel-info-insert-val (val &optional face)
  "Format and insert parameter value VAL at point.
If VAL is string longer than `aurel-info-fill-column', convert it
into several shorter lines.
If FACE is non-nil, propertize inserted line(s) with this FACE."
  (if (stringp val)
      (let ((strings (split-string val "\n *")))
        (and (null (cdr strings))       ; if not multi-line
             (> (length val) aurel-info-fill-column)
             (setq strings
                   (split-string (aurel-info-get-filled-string
                                  val aurel-info-fill-column)
                                 "\n")))
        (insert (aurel-get-string (car strings) face))
        (aurel-info-insert-strings (cdr strings) face))
    (insert (aurel-get-string val face))))

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

(defun aurel-info-aur-user-action (action &optional norevert)
  "Perform AUR user ACTION on the current package.
ACTION is a symbol from `aurel-aur-user-actions'.
If NOREVERT is non-nil, do not revert the buffer (i.e. do not
refresh package information) after ACTION."
  (and (aurel-aur-user-action
        action (aurel-get-param-val 'base-name aurel-info))
       (null norevert)
       (revert-buffer nil t)))

(defun aurel-info-vote-unvote (arg)
  "Vote for the current package.
With prefix (if ARG is non-nil), unvote."
  (interactive "P")
  (aurel-info-aur-user-action (if arg 'unvote 'vote)))

(defun aurel-info-subscribe-unsubscribe (arg)
  "Subscribe to the new comments of the current package.
With prefix (if ARG is non-nil), unsubscribe."
  (interactive "P")
  (aurel-info-aur-user-action (if arg 'unsubscribe 'subscribe)))

(provide 'aurel)

;;; aurel.el ends here
