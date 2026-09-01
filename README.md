# sh-ts-mode

[![CI](https://github.com/konomanoasa/sh-ts-mode/actions/workflows/ci.yml/badge.svg)](https://github.com/konomanoasa/sh-ts-mode/actions/workflows/ci.yml)

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based
[Emacs](https://www.gnu.org/software/emacs/) major mode for the
POSIX.1-2024 Shell Command Language.

## Requirements

- Emacs 31.1 or later

## Installation

```elisp
(package-vc-install "https://github.com/konomanoasa/sh-ts-mode")
```

## Automatic Activation

Enabled for scripts with an `sh` shebang.

## Features

- Comment Commands
- Font Lock
- Imenu: functions
- Indentation
- Navigation: `defun`
- Syntax Table

## Font Lock

Supports `treesit-font-lock-level`.

| Level | Font Lock |
| --- | --- |
| 1 | Comments |
| 2 | Keywords, function definitions, command calls, and strings |
| 3 | Numbers, constants, variable names and uses, and escapes outside shell patterns |
| 4 | Operators, punctuation, brackets, and shell patterns |

## Grammar

[tree-sitter-sh](https://github.com/konomanoasa/tree-sitter-sh)

## License

[MIT](LICENSE)
