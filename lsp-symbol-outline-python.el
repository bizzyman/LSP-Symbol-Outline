;;; lsp-symbol-outline-python.el --- a symbol tree for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 bizzyman

;; Version: 0.0.2
;; Homepage: https://github.com/bizzyman/LSP-Symbol-Outline
;; Keywords: languages, lsp, outline, python

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
;;; Python lang specific code.

;;; Code:

;; Dependencies

(require 'lsp-mode)
(require 'lsp-python)
(require 'lsp-symbol-outline)

;; Defuns

(defun lsp-symbol-outline--get-symbol-depth-python (plist-item)
       "Set :depth property of PLIST-ITEM. This property is used to infer and
build a symbol hierarchy.

Set the python special :parsed-depth property first by reading the
:symbol-start-line property and parsing `current-indentation',
dividing by `python-indent'. For each symbol the previous symbol's
:parsed-depth property is then checked so that no indentation level
exceeds the previous indentation level by more than 1. This is
done for aesthetic reasons whilst still retainining the correct
impression about subscopes. The :depth property is then set.
Returns plist."
       (plist-put plist-item :parsed-depth
                  (save-excursion
                    (goto-line (plist-get plist-item :symbol-start-line))
                    (/ (current-indentation) python-indent)))
       ;; now check prev :parsed-depth and set :depth
       (if (ignore-errors (= (plist-get plist-item :parsed-depth)
                             (plist-get (car agg-items) :parsed-depth)))
           (plist-put plist-item :depth
                      (plist-get (car agg-items) :parsed-depth))
         (plist-put plist-item :depth
                    (if (ignore-errors (> (plist-get plist-item :parsed-depth)
                                          (plist-get (car agg-items)
                                                     :parsed-depth)))
                        (1+ (plist-get (car agg-items) :parsed-depth))
                      (plist-get plist-item :parsed-depth)))))

(defun lsp-symbol-outline--get-symbol-docs-python (plist-item)
       "Find position of symbol in document, parse next line for \"\"\" and
cleanup string. Return first sentence of docstring."
       (forward-line 1)
       (if (search-forward "\"\"\"" (line-end-position) t)
           (s-collapse-whitespace
            (s-replace "\"\"\"" ""
             (s-chop-prefix "\n" (buffer-substring-no-properties
                                  (point)
                                  (progn (forward-sentence) (point))))))))

(defun lsp-symbol-outline--get-symbol-args-python (plist-item hasht-range)
       "Find symbol start line, move to opening param delimiting \"(\" and
return buffer contents between parens. Saves excursion so that next operation -
:docs lookup - can continue from :symbol-start-line. Do additional checks
for python files to ensure imports do not mess up paren parsing.

Returns arg string based on whether it is empty or not."
       (goto-line (plist-get plist-item :symbol-start-line))
       (save-excursion
         (if (progn
               (and (search-forward "("
                                    (lsp--position-to-point
                                     (gethash "end" hasht-range))
                                    t)
                    (looking-back (format "\\(^\\| \\)%s\\( \\|(\\)"
                                          (plist-get plist-item :name))
                                  (line-beginning-position))))
             (pcase (buffer-substring-no-properties
                     (progn (forward-char -1) (point))
                     (progn (forward-sexp) (point)))
               ("()" nil)
               (SYMBOL (s-collapse-whitespace SYMBOL))))))

(defun lsp-symbol-outline--print-outline-python (list buf)
       "Insert indentation, icon, button and any args into symbol outline buffer.
Iterates over symbol list. Python specific argument printing."
       (dolist (item list)
         (insert "  ")
         ;; indentation
         (lsp-symbol-outline--print-indentation item)
         ;; icon
         (if window-system
             (lsp-symbol-outline--print-symbol-icon-gui item)
           (lsp-symbol-outline--print-symbol-icon-term item))
         ;; button
         (lsp-symbol-outline--print-button item buf)
         ;; args
         (if (plist-get item :args)
             (let ((arg-string
                    (car (s-match
                          "\(.+\)"
                          (s-collapse-whitespace
                           (replace-regexp-in-string
                            "\n" ""
                            (plist-get item :args)))))))
               (if arg-string
                   (progn
                     (insert (propertize arg-string
                                         'face 'lsp-symbol-outline-arg-face
                                         'font-lock-ignore 't))))))
         (insert "\n")))

(defun lsp-symbol-outline--print-outline-sorted-python (list-sorted)
       "Print a symbol outline grouped by symbol kind. Takes list of symbol
plists LIST-SORTED and prints the symbol icon, the kind name, symbol button and
any argument information.

LIST-SORTED is filtered to yield only distinct :kind values. LIST-SORTED is then
iterated over and filtered by each distinct :kind value. Symbols of that kind
are printed with no regard to indentation or hierarchy.
Python specific."
       (let ((contains-types (-distinct (-map
                                         (lambda (x) (plist-get x :kind))
                                         list-sorted))))
         (dolist (sym-kind contains-types)
           (let ((same-kind-list
                  (-filter (lambda (i) (equal (plist-get i :kind) sym-kind))
                           list-sorted)))
             ;; icon
             (insert "  ")
             (if window-system
                 (lsp-symbol-outline--print-symbol-icon-gui (car same-kind-list))
               (lsp-symbol-outline--print-symbol-icon-term (car same-kind-list)))
             ;; kind name
             (lsp-symbol-outline--insert-sym-kind-name same-kind-list)

             (dolist (item same-kind-list)
               ;; spaces
               (insert (make-string 5 32))
               ;; button
               (lsp-symbol-outline--print-button item
                                                 lsp-symbol-outline-src-buffer)
               ;; args
               (if (plist-get item :args)
                   (let ((arg-string
                          (car (s-match
                                "\(.+\)"
                                (s-collapse-whitespace
                                 (replace-regexp-in-string
                                  "\n" ""
                                  (plist-get item :args)))))))
                     (if arg-string
                         (progn
                           (insert (propertize arg-string
                                               'face 'lsp-symbol-outline-arg-face
                                               'font-lock-ignore 't))))))
               (insert "\n"))
             (insert " \t \n"))))
       (save-excursion
         (end-of-buffer)
         (set-mark (point))
         (backward-char 4)
         (delete-region (point) (mark)))
       (delete-trailing-whitespace))

(defun lsp-symbol-outline--cycle-arg-visibility-python ()
       "If `lsp-symbol-outline-args-inv' is 0, set only argument types invisible.
If `lsp-symbol-outline-args-inv' is 1, set arguments invisible.
If `lsp-symbol-outline-args-inv' is 2, set all to visible.
Python specific."
       (cond
        ;; args invisible
        ((equal lsp-symbol-outline-args-inv 0)
         (read-only-mode 0)
         (lsp-symbol-outline--set-info-inv)
         (setq-local lsp-symbol-outline-args-inv 1)
         (read-only-mode 1))
        ;; all visible
        ((equal lsp-symbol-outline-args-inv 1)
         (read-only-mode 0)
         (progn
           (remove-list-of-text-properties (point-min) (point-max) '(invisible))
           (lsp-symbol-outline--set-info-vis))
         (setq-local lsp-symbol-outline-args-inv 0)
         (read-only-mode 1))))

;;;###autoload
(defun lsp-symbol-outline-make-outline-python ()
       "Call `lsp-symbol-outline-create-buffer-window' with python specific
functions. Creates LSP sym ouline buffer."
       (interactive)
       (lsp-symbol-outline-create-buffer-window
        #'lsp-symbol-outline--get-symbol-end-line
        #'lsp-symbol-outline--get-symbol-depth-python
        #'lsp-symbol-outline--get-symbol-args-python
        #'lsp-symbol-outline--get-symbol-docs-python
        (lambda (x) x)
        #'lsp-symbol-outline--print-outline-python
        #'lsp-symbol-outline--print-outline-sorted-python
        (lambda () nil) ;??? don't run
        #'lsp-symbol-outline--cycle-arg-visibility-python))



(provide 'lsp-symbol-outline-python)

;;; lsp-symbol-outline-python.el ends here
