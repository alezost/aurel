## About

This is Emacs package for searching, getting information and downloading
AUR ([Arch User Repository](https://aur.archlinux.org/)) packages.  Its
functionality is very similar to the one provided by
[cower](http://github.com/falconindy/cower), but instead of command-line
interface you use Emacs interface.

The package uses [AurJson](https://wiki.archlinux.org/index.php/AurJson)
RPC interface to get information about AUR packages.

## Installation

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


