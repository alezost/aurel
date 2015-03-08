## About

This is an Emacs package for searching, getting information and
downloading AUR ([Arch User Repository][aur]) packages.  Its
functionality is very similar to the one provided by [cower][cower], but
instead of the command-line interface you use Emacs interface.

Also you can use it for (un)voting for packages and (un)subscribing for
package comments (see [AUR account actions](#aur-account-actions)),
i.e. it may be a substitution for [aurvote][aurvote] and
[aurnotify][aurnotify].

The package uses [AurJson](https://wiki.archlinux.org/index.php/AurJson)
RPC interface to get information about AUR packages.

You may look at the [screenshots](#screenshots) below or at the
[gif demonstration](http://storage6.static.itmages.ru/i/14/0214/h_1392417865_1725281_347829e62b.gif)
made by [Ivaylo Kuzev](https://github.com/ivoarch) to get an idea how
aurel looks like.

## Installation

### MELPA

The package can be installed from [MELPA](http://melpa.org)
(with `M-x package-install` or `M-x list-packages`).

### Manual

Add the following lines to your `.emacs`.

- Add a directory with this package to the `load-path`:

  ```elisp
  (add-to-list 'load-path "/path/to/aurel-dir")
  ```

- Add autoloads for the interactive functions:

  ```elisp
  (autoload 'aurel-package-info "aurel" nil t)
  (autoload 'aurel-package-search "aurel" nil t)
  (autoload 'aurel-maintainer-search "aurel" nil t)
  (autoload 'aurel-installed-packages "aurel" nil t)
  ```

- Set a directory where the packages will be downloaded:

  ```elisp
  (setq aurel-download-directory "~/abs")
  ```

### AUR

If you prefer to install everything with pacman, there is
[aurel](https://aur.archlinux.org/packages/aurel) package.

## Usage

- Search for packages by name or description:

  `M-x aurel-package-search`

  Searching for multiple words (separated with spaces) is supported.  If
  you want to search for a string containing spaces, quote it with
  double quotes (`"..."`). Examples of searching:

  + `ttf`
  + `strategy game`
  + `"python library" xml`
  + `light weight "programming language"`

- Search for packages by maintainer:

  `M-x aurel-maintainer-search`

- Get an information about a package by exact name or ID:

  `M-x aurel-package-info`

- Display information about installed AUR packages:

  `M-x aurel-installed-packages`

  A particular filter (see [Filtering](#filtering)) is useful here:
  press <kbd>f</kbd><kbd>v</kbd> to hide the packages with the same
  installed and current AUR versions (i.e. to see the potential
  candidates for updating).

There are 2 kinds of buffers (major modes) for representing an
information about packages:

- `aurel-list-mode`

  It is used for listing multiple packages.  Press <kbd>RET</kbd> on a
  package line to get more information about the package.

- `aurel-info-mode`

  It is used for displaying an information about a single package.

In both modes you can press <kbd>d</kbd> to download the package, but
don't forget to set `aurel-download-directory` before.

Using prefix (<kbd>C-u</kbd>) before interactive commands creates a new
info or list buffer instead of using the existing ones.  That allows,
for example, to keep results of multiple searches or to look at
information about several packages at the same time.

Maintainer name in an info buffer is a button.  If you press
<kbd>RET</kbd> (keep in mind <kbd>C-u</kbd>) on it, a new search for
packages by this maintainer will happen.

Each aurel buffer has its own history similar to the history of the
Emacs `help` or `Info` modes.  You can move backward/forward by the
history with <kbd>l</kbd>/<kbd>r</kbd> and refresh information with
<kbd>g</kbd> (confirmation can be disabled with
`aurel-revert-no-confirm` variable).  If you want to change the number
of stored elements (or to disable the history), use
`aurel-info-history-size` and `aurel-list-history-size` variables.

### AUR account actions

If you have an [AUR][aur] account, you can use `aurel` to vote for
packages, to subscribe for new comments and to show additional
information (whether a package is voted/subscribed by you or not).

The following keys are available in a buffer with package info by
default:

- <kbd>v</kbd> to vote (<kbd>C-u v</kbd> to unvote)
- <kbd>s</kbd> to subscribe (<kbd>C-u s</kbd> to unsubscribe)

To enable receiving additional AUR user specific information (`Voted`
and `Subscribed` lines should appear in the info buffer), use the
following:

```elisp
(setq aurel-aur-user-package-info-check t)
```

The first time `aurel` needs the above information, you will be prompted
for your AUR account (you may set `aurel-aur-user-name` variable for
convenience) and a password.  The password is not saved anywhere, but a
login cookie is saved (emacs saves cookies in `~/.emacs.d/url/cookies`
by default).  The cookie is valid for about a month, so if you don't
want to be prompted for the credentials every month, you may add the
following line to your `~/.authinfo` (or `~/.authinfo.gpg`) file (see
[auth manual][auth-source] for details):

    machine aur.archlinux.org login <user> password <password>

(Substitute `<user>` and `<password>` with your credentials)

## Configuration

User options can be explored with ``M-x customize-group RET aurel``.

If you don't like the names of info and list modes and buffers, you can
change those, for example:

```elisp
(setq aurel-list-mode-name "aurel-list"
      aurel-info-mode-name "aurel-info"
      aurel-list-buffer-name "*aur-list*"
      aurel-info-buffer-name "*aur-info*")
```

By default after receiving information about the packages from AUR
server, pacman is called to get additional information about installed
packages.  If you want to disable that (to make the process a bit
faster or if you don't have pacman installed), use the following:

```elisp
(setq aurel-installed-packages-check nil)
```

### Package list

Anything you see in a buffer with a list of packages is configurable.

Columns can be configured with `aurel-list-column-format`.  Descriptions
of a package parameters (displayed in `aurel-info-mode` buffer) are
stored in `aurel-param-description-alist` variable.  Columns in
`aurel-list-mode` buffer have the same titles as these descriptions,
unless they are not set in `aurel-list-column-name-alist`.

So if you want a new column to be displayed, you need to add a proper
value to `aurel-list-column-format` and optionally add a column name to
`aurel-list-column-name-alist` and add an association to
`aurel-list-column-value-alist` if you need some special calculations of
the column values.

Suppose, you want to rearrange columns, to make them more compact (and
thus to shorten their names) and to add a new sortable column with the
"Last Modified" time.  In such case you can do something like this (see
the second [screenshot](#screenshots)):

```elisp
(setq aurel-list-column-format
      '((name 20 t)
        (maintainer 9 t)
        (votes 5 aurel-list-sort-by-votes)
        (version 8 t)
        (installed-version 8 t)
        (last-date 11 t)
        (description 30 nil))
      aurel-list-column-name-alist
      '((votes . "V.")
        (maintainer . "Maint.")
        (last-date . "Modified")
        (version . "Ver.")
        (installed-version . "Inst.")))

(eval-after-load 'aurel
  '(add-to-list 'aurel-list-column-value-alist
                '(last-date . my-aurel-list-get-last-date)))

(defun my-aurel-list-get-last-date (info)
  "Return formatted date when the package was last modified."
  (format-time-string "%Y-%m-%d"
                      (aurel-get-param-val 'last-date info)))
```

#### Filtering

You can filter the current list of packages to hide some packages.
Press <kbd>f</kbd><kbd>C-h</kbd> to see all available filter bindings.
The most useful ones are:

- <kbd>f</kbd><kbd>f</kbd> to select and enable a filter;
- <kbd>f</kbd><kbd>d</kbd> to disable all filters;

A new filter will be **added** to the enabled ones, so for example you
can hide unmaintained and outdated packages by pressing
<kbd>f</kbd><kbd>m</kbd> and <kbd>f</kbd><kbd>o</kbd>.  If you want to
make a filter **the only** active, press <kbd>C-u</kbd> before a filter
command.

If you have an idea of a new useful filter, you may
[open an issue](https://github.com/alezost/aurel/issues/new) about it.

### Package info

Anything you see in a buffer with a package info is configurable.

Various aspects of displaying information about a package can be
configured with `aurel-info-parameters`,
`aurel-info-installed-parameters`, `aurel-info-aur-user-parameters`,
`aurel-info-insert-params-alist`, `aurel-info-format`,
`aurel-info-fill-column`, `aurel-info-voted-mark`,
`aurel-info-display-voted-mark`, `aurel-info-installed-package-string`,
`aurel-info-aur-user-string`, `aurel-info-ignore-empty-vals`,
`aurel-info-show-maintainer-account` variables and with `aurel-info-...`
faces.  For example:

```elisp
(setq aurel-info-format "%-16s"
      aurel-info-ignore-empty-vals t
      aurel-info-aur-user-string "————————————————————————————————————\n"
      aurel-info-installed-package-string aurel-info-aur-user-string
      aurel-date-format "%d-%b-%Y %T"
      aurel-empty-string "")
```

### Downloading a package

You can change the default behavior of a "downloading action" with
`aurel-info-download-function`, `aurel-list-download-function` and
`aurel-list-multi-download-function` (you can mark several packages for
downloading with <kbd>m</kbd>/<kbd>M</kbd> and unmark with
<kbd>u</kbd>/<kbd>U</kbd>/<kbd>DEL</kbd>) variables.  Currently the
following functions are available:

- `aurel-download`
- `aurel-download-unpack` (default in a list buffer)
- `aurel-download-unpack-dired` (default in an info buffer)
- `aurel-download-unpack-pkgbuild`
- `aurel-download-unpack-eshell`

## Screenshots

Aurel with default settings:

![Default](http://i.imgur.com/5uiwHRc.png)

Aurel with all modifications, described above:

![Changed](http://i.imgur.com/YcJOgQF.png)

In both screenshots `alect-dark` theme from
[alect-themes](https://github.com/alezost/alect-themes) is used.


[aur]: https://aur.archlinux.org/
[cower]: http://github.com/falconindy/cower
[aurvote]: https://aur.archlinux.org/packages/aurvote
[aurnotify]: https://aur.archlinux.org/packages/aurnotify
[auth-source]: https://www.gnu.org/software/emacs/manual/html_node/auth/Help-for-users.html#Help-for-users
