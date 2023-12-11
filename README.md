[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Release Tag](https://img.shields.io/github/tag/jcs-elpa/npm-pkgs.svg?label=release)](https://github.com/jcs-elpa/npm-pkgs/releases/latest)
[![Emacs Ver.](https://img.shields.io/badge/Emacs-27.1+-blue.svg)](https://www.gnu.org/software/emacs/)

# npm-pkgs
> An npm packages client.

[![CI](https://github.com/jcs-elpa/npm-pkgs/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/npm-pkgs/actions/workflows/test.yml)

<p align="center">
  <img src="./etc/demo.png" width="929" height="540"/>
</p>

## 🏆 Features

### Search packages online (like [npm](https://www.npmjs.com/))

You are able to search through packages from NPM and it will display them in the
tabulated list.

### Auto detect workspace

Client will automatically detect the existing `globally` and `locally` installed
packages.

### `Install`/`Uninstall`/`Update` packages visiaully

This package visually displays packages by any actions like `install`,
`uninstall`, `update`, etc.

* <kbd>Shift</kbd>+<kbd>i</kbd> - Mark package for installation.
* <kbd>Shift</kbd>+<kbd>d</kbd> - Mark package for deletion.
* <kbd>Shift</kbd>+<kbd>x</kbd> - Execute all marked packages.
* <kbd>Shift</kbd>+<kbd>u</kbd> - Update all existing packages.

## 🔧 Usage

Simply call the command `npm-pkgs` through `M-x` like this.

```
M-x npm-pkgs
```

Basically, you can call this command anywhere in the directory.
But it will probably only show the global packages without the
project definition.

## 🛠️ Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

### 🔬 Development

To run the test locally, you will need the following tools:

- [Eask](https://emacs-eask.github.io/)
- [Make](https://www.gnu.org/software/make/) (optional)

Install all dependencies and development dependencies:

```sh
$ eask install-deps --dev
```

To test package's installation:

```sh
$ eask package
$ eask install
```

To test compilation:

```sh
$ eask compile
```

**🪧 The following steps are optional, but we recommend you follow these lint results!**

The built-in `checkdoc` linter:

```sh
$ eask lint checkdoc
```

The standard `package` linter:

```sh
$ eask lint package
```

*📝 P.S. For more information, find the Eask manual at https://emacs-eask.github.io/.*

## ⚜️ License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [`LICENSE`](./LICENSE.txt) for details.
