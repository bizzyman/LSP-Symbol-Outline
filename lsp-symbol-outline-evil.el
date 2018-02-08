;;; lsp-symbol-outline-evil.el --- a symbol tree for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 bizzyman

;; Version: 0.0.2
;; Homepage: https://github.com/bizzyman/LSP-Symbol-Outline
;; Keywords: languages, lsp, outline, evil

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and-or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

;;; Commentary:
;;; Evil mode state keymap.

;;; Code:

(require 'evil)
(require 'lsp-symbol-outline)

(evil-define-state lsp-symbol-outline
                   "LSP-Sym-outline state"
                   :enable (motion))

(evil-set-initial-state 'lsp-symbol-outline-mode
                        'lsp-symbol-outline)


;;Keybindings

(define-key evil-lsp-symbol-outline-state-map
            (kbd  "j")
            #'lsp-symbol-outline-next-line)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "k")
            #'lsp-symbol-outline-previous-line)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "TAB")
            #'outline-hide-sublevels)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "<backtab>")
            #'outline-show-all)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "f")
            #'lsp-symbol-outline-toggle-folding)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "q")
            #'kill-buffer-and-window)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "gg")
            #'lsp-symbol-outline-go-top)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "G")
            #'lsp-symbol-outline-go-to-bottom)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "o")
            #'push-button)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "i")
            #'lsp-symbol-outline-cycle-arg-vis)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "gh")
            #'lsp-symbol-outline-up-scope)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "gk")
            #'lsp-symbol-outline-up-sibling)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "gj")
            #'lsp-symbol-outline-down-sibling)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "w")
            #'lsp-symbol-outline-widen-to-widest-column)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "s")
            #'lsp-symbol-outline-toggle-sorted)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "l")
            #'lsp-symbol-outline-peek)
(define-key evil-lsp-symbol-outline-state-map
            (kbd  "d")
            #'lsp-symbol-outline-show-docstring-tip)

(provide 'lsp-symbol-outline-evil)

;;; lsp-symbol-outline-evil.el ends here
