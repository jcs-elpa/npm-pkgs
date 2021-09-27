[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Release Tag](https://img.shields.io/github/tag/jcs-elpa/npm-pkgs.svg?label=release)](https://github.com/jcs-elpa/npm-pkgs/releases/latest)
[![Emacs Ver.](https://img.shields.io/badge/Emacs-27.1+-blue.svg)](https://www.gnu.org/software/emacs/)

# npm-pkgs
> An npm packages client.

[![CI](https://github.com/jcs-elpa/npm-pkgs/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/npm-pkgs/actions/workflows/test.yml)

<p align="center">
  <img src="./etc/demo.png" width="929" height="540"/>
</p>

## Features

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

## Usage

Simply call the command `npm-pkgs` through `M-x` like this.

```
M-x npm-pkgs
```

Basically, you can call this command anywhere in the directory.
But it will probably only show the global packages without the
project definition.

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
