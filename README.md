# sh-ts-mode

[![CI](https://github.com/konomanoasa/sh-ts-mode/actions/workflows/ci.yaml/badge.svg)](https://github.com/konomanoasa/sh-ts-mode/actions/workflows/ci.yaml)

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based
[Emacs](https://www.gnu.org/software/emacs/) major mode for the
POSIX.1-2024 Shell Command Language.

## Requirement

Emacs 31.1 or later.

## Installation

```elisp
(package-vc-install "https://github.com/konomanoasa/sh-ts-mode")
```

## Automatic Activation

Enabled for `.sh` files and scripts with a `sh` shebang.

## Features

- Comment Commands
- Electric Pair
- Font Lock
- Imenu
- Indentation
- Navigation
- Syntax Table

## Font Lock

Supports `treesit-font-lock-level`.

| Level | Font Lock                                                                       |
| ----- | ------------------------------------------------------------------------------- |
| 1     | Comments                                                                        |
| 2     | Keywords, function definitions, command calls, and strings                      |
| 3     | Numbers, constants, variable names and uses, and escapes outside shell patterns |
| 4     | Operators, punctuation, brackets, and shell patterns                            |

## Grammar

[tree-sitter-sh](https://github.com/konomanoasa/tree-sitter-sh)

## License

[MIT](LICENSE)
