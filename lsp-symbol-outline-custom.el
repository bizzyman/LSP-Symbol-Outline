;;; lsp-symbol-outline-custom.el --- a symbol tree for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 bizzyman

;; Version: 0.0.3
;; Homepage: https://github.com/bizzyman/LSP-Symbol-Outline
;; Keywords: languages, lsp, outline, customization

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

;; Customization options for lsp symbol outline project.

;;; Code:

;; Vars

(defcustom lsp-symbol-outline-window-position
           'right
           "LSP symbol outline window position."
           :group 'lsp-symbol-outline)

(defcustom lsp-symbol-outline-window-size
           '(- 0 (/ (frame-width) 6))
           "LSP symbol outline default window size. Can be a sexp or int.
Default value divides frame width by 6."
           :group 'lsp-symbol-outline)

(defcustom lsp-symbol-outline-lock-window
           'width
           "Whether to lock window size of outlne once it has been created.
Default is `width'. Setting this to nil can cause erratic behaviour in window
sizing in outline buffer." ;; FIXME
           :group 'lsp-symbol-outline)

(defcustom lsp-symbol-outline-modeline-format
           nil
           "Local modeline format for the LSP symbol outline mode.
Default is nil, which removes modeline completely."
           :group 'lsp-symbol-outline)

(defcustom lsp-symbol-outline-start-sorted
           nil
           "Whether to start the outline in sorted mode, which groups the
symbols by category. Default is nil, which means the outline starts in sequential
mode, printing the symbols in order they appear in source buffer."
           :group 'lsp-symbol-outline)

(defcustom lsp-symbol-outline-default-arg-visibility
           0
           "At what level to set the argument visibility of symbols in outline.
0 means both types and args will be visible at start. 1 means types will be
invisible at start. 2 means both types and args will be invisible.
Default is 0."
           :group 'lsp-symbol-outline)

(defcustom lsp-symbol-outline-startup-folded
           nil
           "Whether to fold the outline at start. Default is nil."
           :group 'lsp-symbol-outline)

(defcustom lsp-symbol-outline-print-fancy-glyphs
           t
           "Whether to use the fancy glyphs from atomicons.ttf font or regular
ascii chars for symbol category representation. Default is t."
           :group 'lsp-symbol-outline)

(provide 'lsp-symbol-outline-custom)

;;; lsp-symbol-outline-custom.el ends here
