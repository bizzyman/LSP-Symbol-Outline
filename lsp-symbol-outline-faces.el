;;; lsp-symbol-outline-faces.el --- a symbol tree for lsp-mode  -*- lexical-binding: t; -*-

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

;; Faces for lsp symbol outline project.

;;; Code:

;; Faces

(defface lsp-symbol-outline-button-face
         '((t :foreground "#93a0b2"))
         "Face for outline node buttons."
         :group 'lsp-symbol-outline-faces)

(defface lsp-symbol-outline-atom-icons-face
         '((t (:inherit default
               :family "atomicons"
               :height 1.0)))
         "Face for atom-outline icons."
         :group 'lsp-symbol-outline-faces)

(defface lsp-symbol-outline-term-symbol-type-name-face
         '((t (:foreground "color-141")))
         "Face for outline symbol types node."
         :group 'lsp-symbol-outline-faces)

(defface lsp-symbol-outline-class-face-has-doc
         '((t (:inherit font-lock-type-face
               :underline t)))
         "Face for outline class nodes with docs."
         :group 'lsp-symbol-outline-faces)

(defface lsp-symbol-outline-class-face
         '((t (:inherit font-lock-type-face)))
         "Face for outline class nodes."
         :group 'lsp-symbol-outline-faces)

(defface lsp-symbol-outline-function-face-has-doc
         '((t (:inherit font-lock-function-name-face
               :underline t)))
         "Face for outline function nodes with docs."
         :group 'lsp-symbol-outline-faces)

(defface lsp-symbol-outline-function-face
         '((t :inherit font-lock-function-name-face))
         "Face for outline function nodes."
         :group 'lsp-symbol-outline-faces)

(defface lsp-symbol-outline-var-face
         '((t :inherit font-lock-variable-name-face))
         "Face for outline variable nodes."
         :group 'lsp-symbol-outline-faces)

(defface lsp-symbol-outline-arg-face
         '((t :inherit font-lock-doc-face))
         "Face for outline node arguments."
         :group 'lsp-symbol-outline-faces)

(defface lsp-symbol-outline-arg-type-face
         '((t :inherit font-lock-type-face))
         "Face for outline node arguments."
         :group 'lsp-symbol-outline-faces)

(defface lsp-symbol-outline-html-tag-props-face
         '((t :foreground "#75B5AA"))
         "Face for outline html props."
         :group 'lsp-symbol-outline-faces)

(defface lsp-symbol-outline-inside-current-symbol
         '((t :inherit default))
         "Face for ."
         :group 'lsp-symbol-outline-faces)


(provide 'lsp-symbol-outline-faces)

;;; lsp-symbol-outline-faces.el ends here
