## About

This is an Emacs package for searching, getting information and
downloading AUR ([Arch User Repository](https://aur.archlinux.org/))
packages.  Its functionality is very similar to the one provided by
[cower](http://github.com/falconindy/cower), but instead of command-line
interface you use Emacs interface.

The package uses [AurJson](https://wiki.archlinux.org/index.php/AurJson)
RPC interface to get information about AUR packages.

## Installation

### MELPA

The package can be installed from [MELPA](http://melpa.milkbox.net).
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

## Usage

- Search for packages by name or description:

  `M-x aurel-package-search`

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

## Configuration

User options (not much currently) can be explored with
``M-x customize-group RET aurel``.

Descriptions of package parameters (displayed in `aurel-info-mode`
buffer and as column titles in `aurel-list-mode` buffer) can be changed
with `aurel-param-description-alist` variable.

Columns in a buffer with a list of packages can be configured with
`aurel-list-column-format`.

Various aspects of displaying information about a package can be
configured with `aurel-info-parameters`,
`aurel-info-insert-params-alist`, `aurel-info-format` and
`aurel-info-fill-column` variables and with `aurel-info-...` faces.

### Downloading a package

You can change the default behavior of a "downloading action" with
`aurel-info-download-function` and `aurel-list-download-function`
variables.  Currently the following functions are available:

- `aurel-download`
- `aurel-download-unpack` (default in a list buffer)
- `aurel-download-unpack-dired` (default in an info buffer)
- `aurel-download-unpack-pkgbuild`
- `aurel-download-unpack-eshell`

