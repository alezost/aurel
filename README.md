[![License GPL 3](https://img.shields.io/badge/license-GPL_3-orange.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/aurel-badge.svg)](http://melpa.org/#/aurel)
[![MELPA Stable](http://stable.melpa.org/packages/aurel-badge.svg)](http://stable.melpa.org/#/aurel)

## About

This is an Emacs package for searching, getting information and
downloading AUR ([Arch User Repository][aur]) packages.  Its
functionality is very similar to the one provided by [cower][cower], but
instead of the command-line interface you use Emacs interface.

Also you can use it to (un)vote for packages and to (un)subscribe for
package comments (see [AUR account actions](#aur-account-actions)),
i.e. it may be a substitution for [aurvote][aurvote] and
[aurnotify][aurnotify].

This package uses [AurJson](https://wiki.archlinux.org/index.php/AurJson)
RPC interface to get information about AUR packages.

You may look at the screenshot below or at
[this gif demonstration](http://storage6.static.itmages.ru/i/14/0214/h_1392417865_1725281_347829e62b.gif)
made by [Ivaylo Kuzev](https://github.com/ivoarch) to get an idea how
aurel looks like (this demo is a bit outdated as it was made for version
0.1, the interface has changed since then).

![screenshot](http://i.imgur.com/gX1Lk84.png)

(`alect-light` theme from
[alect-themes](https://github.com/alezost/alect-themes) is used in this
screenshot)

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
  (autoload 'aurel-package-search-by-name "aurel" nil t)
  (autoload 'aurel-maintainer-search "aurel" nil t)
  (autoload 'aurel-installed-packages "aurel" nil t)
  ```

- Set a directory where the packages will be downloaded:

  ```elisp
  (setq aurel-download-directory "~/abs")
  ```

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

- Search for packages only by name (i.e., only packages with names that
  contain a searched string will be displayed):

  `M-x aurel-package-search-by-name`

- Search for packages by maintainer:

  `M-x aurel-maintainer-search`

- Get an information about a package by exact name:

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

  Display packages in a form similar to a list of Emacs packages (``M-x
  list-packages``).  Press <kbd>RET</kbd> to get more information about
  the current package or the marked packages (you can mark several
  packages with <kbd>m</kbd>/<kbd>M</kbd> and unmark with
  <kbd>u</kbd>/<kbd>U</kbd>/<kbd>DEL</kbd>).

- `aurel-info-mode`

  Display more information about package(s) in a buffer with
  self-descriptive buttons.  This buffer is similar to an Emacs
  customization buffer.

In both modes you can press <kbd>d</kbd> to download the package, but
don't forget to set `aurel-download-directory` before.

Each aurel buffer has its own history similar to the history of the
Emacs `help` or `Info` modes.  You can move backward/forward by the
history with <kbd>l</kbd>/<kbd>r</kbd> and refresh information with
<kbd>g</kbd>.

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

Anything you see in `info`/`list` buffers is configurable using
`aurel-info-format` and `aurel-list-format` variables.

If you want to change column titles (displayed in `list` buffer) and
descriptions of package parameters (displayed in `info` buffer), there
are `aurel-list-titles`/`aurel-info-titles` and `aurel-titles` (general
for both buffer types) variables.

If you don't like the names of info and list buffers, you can change
them like this:

```elisp
(setq aurel-list-buffer-name "*aur-list*"
      aurel-info-buffer-name "*aur-info*")
```

By default, after receiving information about the packages from AUR
server, pacman is called to get additional information about installed
packages.  If you want to disable that (to make the process a bit
faster, for example), use the following:

```elisp
(setq aurel-installed-packages-check nil)
```

### Filtering

You can hide some of the displayed packages using filters.  Press
<kbd>f</kbd><kbd>C-h</kbd> to see all available filter bindings.  The
most useful ones are:

- <kbd>f</kbd><kbd>f</kbd> to select and enable a filter;
- <kbd>f</kbd><kbd>d</kbd> to disable all filters;

A new filter will be **added** to the enabled ones, so for example you
can hide unmaintained and outdated packages by pressing
<kbd>f</kbd><kbd>m</kbd> and <kbd>f</kbd><kbd>o</kbd>.  If you want to
make a filter **the only** active, press <kbd>C-u</kbd> before a filter
command.

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

## Acknowledgements

The following people helped to improve this package:

- Andrea De Michele: fixed the pacman localization issue.


[aur]: https://aur.archlinux.org/
[cower]: http://github.com/falconindy/cower
[aurvote]: https://aur.archlinux.org/packages/aurvote
[aurnotify]: https://aur.archlinux.org/packages/aurnotify
[auth-source]: https://www.gnu.org/software/emacs/manual/html_node/auth/Help-for-users.html#Help-for-users
