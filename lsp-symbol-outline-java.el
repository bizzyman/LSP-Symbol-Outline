;;; lsp-symbol-outline-java.el --- a symbol tree for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 bizzyman

;; Version: 0.0.3
;; Homepage: https://github.com/bizzyman/LSP-Symbol-Outline
;; Keywords: languages, lsp, outline, java

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
;;; Java lang specific code.

;;; Code:

;; Dependencies

(require 'lsp-mode)
(require 'lsp-java)
(require 'lsp-symbol-outline)

;; Defuns

(defun lsp-symbol-outline--get-symbol-docs-java (_plist-item)
       "Move to :symbol-start-point and parse javadoc block above symbol.
Return first sentence of block as string."
       (vertical-motion -1)
       (if (and (search-forward "*/" (line-end-position) t)
                (forward-comment -1)
                (looking-at "\\/\\*\\*"))
           (progn
             (forward-char 3)
             (s-collapse-whitespace
              (s-chop-prefix "\n"
                             (s-replace-regexp
                              "<.+?>" ""
                              (s-replace-regexp
                               "/\\*\\*" ""
                               (s-replace-regexp
                                " +\\* " "" (thing-at-point 'sentence t)))))))))
;;;###autoload
(defun lsp-symbol-outline-make-outline-java ()
  "Call `lsp-symbol-outline-create-buffer-window' with java specific
functions. Creates LSP sym ouline buffer."
  (interactive)
  (lsp-symbol-outline-create-buffer-window
   #'lsp-symbol-outline--get-symbol-end-point-clike-generic
   #'lsp-symbol-outline--set-placeholder-depth
   #'lsp-symbol-outline--get-symbol-args-generic
   #'lsp-symbol-outline--get-symbol-docs-java
   #'lsp-symbol-outline--tree-sort
   #'lsp-symbol-outline--print-outline-clike-generic
   #'lsp-symbol-outline--print-outline-sorted-clike-generic
   #'lsp-symbol-outline--finalize-arg-props-clike-generic
   #'lsp-symbol-outline--cycle-arg-visibility-clike-generic))



(provide 'lsp-symbol-outline-java)

;;; lsp-symbol-outline-java.el ends here
