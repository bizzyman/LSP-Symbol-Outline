# LSP Symbol Outline
A tree like view for symbols in Emacs using the Language Server Protocol. Currently works with Javascript, Python, C/C++, PHP, Go, Rust and Java.

![LSP-Symbol-outline-sorted](https://raw.githubusercontent.com/bizzyman/LSP-Symbol-Outline/master/scrot.png)

## WARNING
This is both my first ever emacs package and an extremely early version of this package so be warned! It will have many bugs (which are welcome to be reported).

## Dependencies

My goal is to publish this on melpa some day which will auto-install dependencies, however for now you must have these packages installed:

* lsp-mode
* outline-magic
* s
* dash

(optionally, install for languages you use)
* lsp-java
* lsp-rust
* lsp-go
* lsp-python
* lsp-php
* lsp-javascript-typescript
* tern (for func arguments)
* cquery/ccls

## Install instructions

Add this project to your load path and either `require` or `use-package`.

In order to get the pretty symbol glyphs you must install the font 'atomicons.ttf' from this repo.

You must have lsp-mode installed and the required servers for the language you want to use. 
E.g. for javascript install https://github.com/emacs-lsp/lsp-javascript and https://github.com/sourcegraph/javascript-typescript-langserver

You must also enable LSP when opening buffers in which you want to use the symbol outline. For example, add

```emacs-lisp
(require 'lsp-javascript-typescript)
(add-hook 'js2-mode-hook #'lsp-javascript-typescript-enable)
```

**Javascript special instructions:**
You must have the tern package installed and a .tern-project file in the root of your project. You must have the tern package enabled.

## Usage

Bind one of the following functions, or call  from `M-:`:\
`(lsp-symbol-outline-make-outline-python)`\
`(lsp-symbol-outline-make-outline-js)`\
`(lsp-symbol-outline-make-outline-java)`\
`(lsp-symbol-outline-make-outline-C)`\
`(lsp-symbol-outline-make-outline-rust)`\
`(lsp-symbol-outline-make-outline-php)`\
`(lsp-symbol-outline-make-outline-go)`

OR use the following convenience function:
```
(defun lsp-symbol-outline-create-conditional ()
    (interactive)
    (cond ((equal major-mode 'python-mode)
           (lsp-symbol-outline-make-outline-python))
          ((equal major-mode 'go-mode)
           (lsp-symbol-outline-make-outline-go))
          ((equal major-mode 'php-mode)
           (lsp-symbol-outline-make-outline-php))
          ((equal major-mode 'rust-mode)
           (lsp-symbol-outline-make-outline-rust))
          ((or (equal major-mode 'c-mode)
                (equal major-mode 'c++-mode))
           (lsp-symbol-outline-make-outline-C))
          ((or (equal major-mode 'js2-mode)
               (equal major-mode 'js-mode)
               (equal major-mode 'typescript-mode))
           (lsp-symbol-outline-make-outline-js))
          ((equal major-mode 'java-mode)
           (lsp-symbol-outline-make-outline-java))))
```

Underlined symbols have documentation.

## Key Bindings

### emacs
* **C-n** move down
* **C-p** move up
* **TAB** hide all sublevels
* **BACKTAB** show all sublevels
* **f** fold sublevel
* **q** kill window
* **M-<** go to top
* **M->** go to bottom
* **o** go to symbol in document lose focus
* **i** cycle argument visibility
* **C-M-u** go up scope
* **M-p** go up sibling
* **M-n** go down sibling
* **w** widen to widest column
* **s** toggle sorted view (sorts by symbol category)
* **l** peek symbol (goes to location in document, but does not lose focus)
* **d** show documentation string if available
* **m** mark (select) the symbol name at point
* **h** show help message

### evil
* **j** move down
* **k** move up
* **TAB** hide all sublevels
* **BACKTAB** show all sublevels
* **f** fold sublevel
* **q** kill window
* **gg** go to top
* **G** go to bottom
* **o** go to symbol in document lose focus
* **i** cycle argument visibility
* **gh** go up scope
* **gk** go up sibling
* **gj** go down sibling
* **w** widen to widest column
* **s** toggle sorted view (sorts by symbol category)
* **l** peek symbol (goes to location in document, but does not lose focus)
* **d** show documentation string if available
* **m** mark (select) the symbol name at point
* **h** show help message

## Notes on current state of this package, lsp and bugs

As mentioned at the beginning, this is an early stage package which will undergo many changes in the future. The next priority is to structure this package properly and minimize dependencies. 

### LSP

Currently this package uses Microsoft's Language Server Protocol features and the various implementations. As this is not yet a mature project, this means there will be bugs with this package as well. One I came across many times is having more than one project open at the same time causes the server to fail. Reopening the file usually fixes this. 

Furthermore, features to this package will track LSP features. One that is on the horizon currently is real implementation of a symbol hierarchy which will enable a more precise tree structure for this package. Please follow the following issues: https://github.com/Microsoft/language-server-protocol/issues/136 https://github.com/Microsoft/language-server-protocol/issues/327 https://github.com/Microsoft/vscode/issues/34968 .

### Known Bugs

**Why are the function arguments in JS modes missing?** \
This seems to be related to tern. It seems to get confused when more than one file from different projects are opened
simultaneously, or after a long idle period. Try reverting the buffer.

**Why is symbol X missing from the outline?** \
This package relies on the symbols returned by the LSP "textDocument/documentSymbol"
method, as such, it has no control over the symbols that get displayed in the
outline tree. Therefore, missing symbols that you think should have been included
should be reported upstream to the relevant language server.


