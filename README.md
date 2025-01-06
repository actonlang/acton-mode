# acton-mode

An Emacs major mode for the Acton programming language.

## Features

- Syntax highlighting for `.act` files
- Basic indentation support

## Installation

### Using Doom Emacs

Add to your `packages.el`:

```elisp
(package! acton-mode
  :recipe (:host github :repo "actonlang/acton-mode"))
```

Then run `doom sync`.

### Using straight.el

```elisp
(straight-use-package
 '(acton-mode :type git :host github :repo "actonlang/acton-mode"))
```

### Manual Installation

1. Clone this repository:
```console
git clone https://github.com/actonlang/acton-mode.git
```

2. Add to your Emacs init file:
```elisp
(add-to-list 'load-path "/path/to/acton-mode")
(require 'acton-mode)
```
