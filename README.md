# LSP-Symbol-Outline
A tree like view for symbols in Emacs using the Language Server Protocol. Currently works with Javascript, Python and Java.

![LSP-Symbol-outline](https://i.imgur.com/mpDgD3Y.png)

## DISCLAIMER
This is both my first ever emacs package and an extremely early version of this package so be warned! It will have many bugs (which are welcome to be reported).

## Dependencies

My goal is to publish this on melpa some day which will auto-install dependencies, however for now you must have these packages installed:

* lsp-mode
* ov
* request
* outline-magic
* s
* dash
* misc-cmds

(optionally, install for languages you use)
* lsp-java
* lsp-python
* lsp-javascript-typescript
* tern (for func arguments)

## Install instructions

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

**Python special instructions:**
To infer hierarchy this package currently uses the following hack (please someone suggest an alternative for this).

The following code has been added to this implementation https://github.com/palantir/python-language-server of the python language server:

(in symbols.py of pyls package)
```python
9  + def parse_depth(string):
10 +   spcCount = len(string)-len(string.lstrip(' '))
11 +   return spcCount / 4

14   @hookimpl
15   def pyls_document_symbols(config, document):
16      all_scopes = config.plugin_settings('jedi_symbols').get('all_scopes', True)
17      definitions = document.jedi_names(all_scopes=all_scopes)
18      return [{
19          'name': d.name,
20          'kind': _kind(d),
21          'location': {'uri': document.uri, 'range': _range(d)},
22 +        'depth': parse_depth(d.get_line_code(before=1, after=-1))
23      } for d in definitions]
```
Without the above hack, there is no current way to infer hierarchy for symbols in python using lsp.

## Usage

Bind the function `(lsp-symbol-outline-create-buffer-window)` or call it from `M-:`.
Underlined symbols have documentation.

## Key Bindings

* j move down
* k move up
* TAB hide all sublevels
* BACKTAB show all sublevels
* f fold sublevel
* q kill window
* gg go to top
* G go to bottom
* o go to symbol in document lose focus
* i cycle argument visibility
* gh go up scope
* gk go up sibling
* gj do down sibling
* w widen to widest column
* s toggle sorted view (sorts by symbol category)
* l peek symbol (goes to location in document, but does not lose focus)
* d show documentation string if available

## Notes on current state of this package, lsp and bugs

As mentioned at the beginning, this is an early stage package which will undergo many changes in the future. The next priority is to structure this package properly and minimize dependencies (including evil). 

### LSP

Currently this package uses Microsoft's Language Server Protocol features and the various implementations. As this is not yet a mature project, this means there will be bugs with this package as well. One I came across many times is having more than one project open at the same time causes the server to fail. Reopening the file usually fixes this. 

Furthermore, features to this package will track LSP features. One that is on the horizon currently is real implementation of a symbol hierarchy which will enable a more precise tree structure for this package. Please follow the following issues: https://github.com/Microsoft/language-server-protocol/issues/136 https://github.com/Microsoft/language-server-protocol/issues/327 https://github.com/Microsoft/vscode/issues/34968 .


