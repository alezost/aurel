## About

This is an Emacs package for searching, getting information and
downloading AUR ([Arch User Repository](https://aur.archlinux.org/))
packages.  Its functionality is very similar to the one provided by
[cower](http://github.com/falconindy/cower), but instead of command-line
interface you use Emacs interface.

The package uses [AurJson](https://wiki.archlinux.org/index.php/AurJson)
RPC interface to get information about AUR packages.

You may look at the [screenshots](#screenshots) below or at the
[gif demonstration](http://storage6.static.itmages.ru/i/14/0214/h_1392417865_1725281_347829e62b.gif)
made by [Ivaylo Kuzev](https://github.com/ivoarch) to get an idea how
aurel looks like.

## Installation

### MELPA

The package can be installed from [MELPA](http://melpa.milkbox.net)
(with `M-x package-install` or `M-x list-packages`).

### Manual

Add the following lines to your `.emacs`.

- Add a directory with this package to the `load-path`:

  ```lisp
  (add-to-list 'load-path "/path/to/aurel-dir")
  ```

- Add autoloads for the interactive functions:

  ```lisp
  (autoload 'aurel-package-info "aurel" nil t)
  (autoload 'aurel-package-search "aurel" nil t)
  (autoload 'aurel-maintainer-search "aurel" nil t)
  ```

- Set a directory where the packages will be downloaded:

  ```lisp
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

## Configuration

User options can be explored with ``M-x customize-group RET aurel``.

If you don't like the names of info and list modes and buffers, you can
change those, for example:

```lisp
(setq aurel-list-mode-name "aurel-list"
      aurel-info-mode-name "aurel-info"
      aurel-list-buffer-name "*aur-list*"
      aurel-info-buffer-name "*aur-info*")
```

By default after receiving information about the packages from AUR
server, pacman is called to get additional information about installed
packages.  If you want to disable that (to make the process a bit
faster or if you don't have pacman installed), use the following:

```lisp
(setq aurel-installed-packages-check nil)
```

### Package list

Columns in a buffer with a list of packages can be configured with
`aurel-list-column-format`.

For example, if you don't want to have a column with version but want to
add **sortable** columns with maintainer and votes, try the following:

```lisp
(setq aurel-list-column-format
      '((name 20 t)
        (maintainer 13 t)
        (votes 5
         (lambda (a b)
           (> (string-to-number (aref (cadr a) 2))
              (string-to-number (aref (cadr b) 2)))))
        (installed-version 8 t)
        (description 30 nil)))
```

Descriptions of a package parameters (displayed in `aurel-info-mode`
buffer) are stored in `aurel-param-description-alist` variable.  Columns
in `aurel-list-mode` buffer have the same titles as these descriptions,
unless they are not set in `aurel-list-column-name-alist`.  For example,
the following shortened titles suit better the compact column format
shown in the above example:

```lisp
(setq aurel-list-column-name-alist
      '((votes . "V.")
        (installed-version . "Inst.")))
```

### Package info

Various aspects of displaying information about a package can be
configured with `aurel-info-parameters`,
`aurel-info-installed-parameters`, `aurel-info-insert-params-alist`,
`aurel-info-format`, `aurel-info-fill-column`,
`aurel-info-installed-package-string`, `aurel-info-ignore-empty-vals`,
`aurel-info-show-maintainer-account` variables and with `aurel-info-...`
faces.  For example:

```lisp
(setq aurel-info-format "%-16s"
      aurel-info-ignore-empty-vals t
      aurel-info-installed-package-string "\n————————————————————————————————\n\n"
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

![Default](http://i.imgur.com/okR2x9q.png)

Aurel with all modifications, described in the
[Configuration section](#configuration):

![Changed](http://i.imgur.com/1JxaebN.png)

In both screenshots `alect-dark` theme from
[alect-themes](https://github.com/alezost/alect-themes) is used.

