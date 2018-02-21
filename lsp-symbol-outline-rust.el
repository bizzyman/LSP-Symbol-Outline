;;; lsp-symbol-outline-rust.el --- a symbol tree for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 bizzyman

;; Version: 0.0.3
;; Homepage: https://github.com/bizzyman/LSP-Symbol-Outline
;; Keywords: languages, lsp, outline, rust

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
;;; Rust lang specific code.

;;; Code:

;; Dependencies

(require 'lsp-mode)
(require 'lsp-rust)
(require 'lsp-symbol-outline)

;; Defuns

(defun lsp-symbol-outline--get-symbol-docs-rust (_plist-item)
       "Move to :symbol-start-point and parse Rustdoc block above symbol.
Return first sentence of block as string."
       (vertical-motion -1)
       (when (save-excursion (search-forward "///" (line-end-position) t))
           (while (save-excursion
                    (and (vertical-motion -1)
                         (search-forward "///" (line-end-position) t)))
             (vertical-motion -1))
           (s-collapse-whitespace
            (s-replace-all
             '(("///" . ""))
             (s-trim
              (buffer-substring-no-properties
               (search-forward "///" (line-end-position) t)
               ;; (re-search-forward "[.?!\n]" nil t)
               (progn
                 (save-match-data
                   (re-search-forward "\\([?!]\\|\\(\\.[ $]\\)\\|\\(^ *[^/ ]\\)\\)")
                   (match-beginning 1)))))))))

(defun lsp-symbol-outline--find-end-of-arg-type-rust ()
       "Parse current line to find the end range of type information of current
arg. Rust specific."
       (cond
        ((looking-at " {")
         (search-forward "{")
         (backward-char 1)
         (lsp-symbol-outline--jump-paren)
         (if
             (looking-at ".|")
             (progn
               (forward-char 2)
               (cond ((lsp-symbol-outline--jump-paren)
                      (point))
                     ((search-forward "," (line-end-position) t)
                      (1- (point)))
                     ((search-forward ")" (line-end-position) t)
                      (1- (point)))))
           (1+ (point))))
        (t (progn
             (backward-char 1)
             (if (re-search-forward ": .+?," (line-end-position) t 1)
                 (1- (point))
               (re-search-forward ": .+?$" (line-end-position) t 1)
               (- (point) 1))))))

(defun lsp-symbol-outline--finalize-arg-props-rust ()
       "Parse buffer for colon char and find argument types. Position of types
is passed to `lsp-symbol-outline--set-arg-type-props' which sets different text
properties on argument type information.

Regex parsing is used to set invisible properties to toggle hiding type
information. Rust specific."
       (save-excursion
         (goto-char (point-min))
         (save-match-data
           (while (re-search-forward "(\\( ?&?mut\\) " nil 'noerror 1)
             (lsp-symbol-outline--set-arg-type-props (match-beginning 1)
                                                     (match-end 1))))
         (goto-char (point-min))
         (while (re-search-forward ":" nil 'noerror 1)
           (let ((ref (match-string-no-properties 1)))
             (lsp-symbol-outline--set-arg-type-props
              (1- (point))
              (lsp-symbol-outline--find-end-of-arg-type-rust))))))

;;;###autoload
(defun lsp-symbol-outline-make-outline-rust ()
       "Call `lsp-symbol-outline-create-buffer-window' with Rust specific
functions. Creates LSP sym ouline buffer."
       (interactive)
       (lsp-symbol-outline-create-buffer-window
        #'lsp-symbol-outline--get-symbol-end-point-clike-generic
        #'lsp-symbol-outline--set-placeholder-depth
        #'lsp-symbol-outline--get-symbol-args-generic
        #'lsp-symbol-outline--get-symbol-docs-rust
        #'lsp-symbol-outline--tree-sort-rem-args
        #'lsp-symbol-outline--print-outline-clike-generic
        #'lsp-symbol-outline--print-outline-sorted-clike-generic
        #'lsp-symbol-outline--finalize-arg-props-rust
        #'lsp-symbol-outline--cycle-arg-visibility-colon-generic))

(provide 'lsp-symbol-outline-rust)

;;; lsp-symbol-outline-rust.el ends here
