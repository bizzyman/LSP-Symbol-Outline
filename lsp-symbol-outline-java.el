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

(defun lsp-symbol-outline--get-symbol-end-line-java (hasht-range)
       "Get the java symbol end line by moving point to end position in
HASHT-RANGE and jumping to matching } brace. Return line number."
       (save-excursion
         (goto-line
          (1+ (gethash "line"
                       (gethash "end"
                                hasht-range))))
         (move-to-column
          (gethash "character"
                   (gethash "end"
                            hasht-range)))
         (search-forward "{" nil t)
         (backward-char)
         (lsp-symbol-outline--jump-paren)
         (point)))

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

(defun lsp-symbol-outline--print-outline-java (list buf)
       "Insert indentation, icon, button and any args into symbol outline buffer.
Iterates over symbol list. Java specific argument printing."
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
               (ignore-errors
                 (replace-regexp-in-string
                  "\n" ""
                  (plist-get item :args)))))
          (if arg-string
              (progn
                (insert (propertize arg-string
                                    'face 'lsp-symbol-outline-arg-face
                                    'font-lock-ignore 't))))))
    (insert "\n")))

(defun lsp-symbol-outline--print-outline-sorted-java (list-sorted)
       "Print a symbol outline grouped by symbol kind. Takes list of symbol
plists LIST-SORTED and prints the symbol icon, the kind name, symbol button and
any argument information.

LIST-SORTED is filtered to yield only distinct :kind values. LIST-SORTED is then
iterated over and filtered by each distinct :kind value. Symbols of that kind
are printed with no regard to indentation or hierarchy.
Java specific."
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
                          (ignore-errors
                            (replace-regexp-in-string
                             "\n" ""
                             (plist-get item :args)))))
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

(defun lsp-symbol-outline--finalize-arg-props-java ()
       "Parse buffer for comma char and find argument types. Position of types
is passed to `lsp-symbol-outline--set-arg-type-props' which sets different text
properties on argument type information.

Regex parsing is used to set invisible properties to toggle hiding type
information. Java specific."
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward ")" nil 'noerror 1)
           (search-backward " " (line-beginning-position) t)
           (lsp-symbol-outline--set-arg-type-props
            (point)
            (progn
              (or (search-backward "," (line-beginning-position) t)
                  (search-backward "(" (line-beginning-position) t))
              (+ (point) 1)))
           (while
               (save-excursion (or
                                (search-backward ","
                                                 (line-beginning-position) t)
                                (search-backward "("
                                                 (line-beginning-position) t)))
             (search-backward " " (line-beginning-position) t)
             (lsp-symbol-outline--set-arg-type-props
              (point)
              (progn
                (or (search-backward "," (line-beginning-position) t)
                    (search-backward "(" (line-beginning-position) t))
                (+ (point) 1))))
           (vertical-motion 1))))

(defun lsp-symbol-outline--set-arg-types-inv-java ()
       "Parse buffer for colon char and find argument types. Call
`lsp-symbol-outline--set-arg-props-inv' on found positions to set argument
information invisible by setting text properties. Java specific."
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward ")" nil 'noerror 1)
           (search-backward " " (line-beginning-position) t)
           (let ((p (point))
                 (e))
             (cond ((search-backward "," (line-beginning-position) t)
                    (setq e (+ (point) 1)))
                   ((search-backward "(" (line-beginning-position) t)
                    (progn (setq e (+ (point) 1)) (setq p (1+ p)))))
             (when e (lsp-symbol-outline--set-arg-props-inv
                      p e)))
           (while
               (save-excursion (or
                                (search-backward ","
                                                 (line-beginning-position) t)
                                (search-backward "("
                                                 (line-beginning-position) t)))
             (search-backward " " (line-beginning-position) t)
             (let ((p (point)) (e))
               (cond ((search-backward "," (line-beginning-position) t)
                      (setq e (+ (point) 1)))
                     ((search-backward "(" (line-beginning-position) t)
                      (progn (setq e (+ (point) 1)) (setq p (1+ p)))))
               (lsp-symbol-outline--set-arg-props-inv
                p e)))
           (vertical-motion 1))))

(defun lsp-symbol-outline--cycle-arg-visibility-java ()
       "If `lsp-symbol-outline-args-inv' is 0, set only argument types invisible.
If `lsp-symbol-outline-args-inv' is 1, set arguments invisible.
If `lsp-symbol-outline-args-inv' is 2, set all to visible.
Java specific."
       (cond
        ;; arg types invisible
        ((equal lsp-symbol-outline-args-inv 0)
         (read-only-mode 0)
         (lsp-symbol-outline--set-arg-types-inv-java)
         (setq-local lsp-symbol-outline-args-inv 1)
         (read-only-mode 1))
        ;; args invisible
        ((equal lsp-symbol-outline-args-inv 1)
         (read-only-mode 0)
         (lsp-symbol-outline--set-info-inv)
         (setq-local lsp-symbol-outline-args-inv 2)
         (read-only-mode 1))
        ;; all visible
        ((equal lsp-symbol-outline-args-inv 2)
         (read-only-mode 0)
         (progn
           (remove-list-of-text-properties (point-min) (point-max) '(invisible))
           (lsp-symbol-outline--set-info-vis)
           (lsp-symbol-outline--finalize-arg-props-java))
         (setq-local lsp-symbol-outline-args-inv 0)
         (read-only-mode 1))))


;;;###autoload
(defun lsp-symbol-outline-make-outline-java ()
  "Call `lsp-symbol-outline-create-buffer-window' with java specific
functions. Creates LSP sym ouline buffer."
  (interactive)
  (lsp-symbol-outline-create-buffer-window
   #'lsp-symbol-outline--get-symbol-end-line-java
   #'lsp-symbol-outline--set-placeholder-depth
   #'lsp-symbol-outline--get-symbol-args-generic
   #'lsp-symbol-outline--get-symbol-docs-java
   #'lsp-symbol-outline--tree-sort
   #'lsp-symbol-outline--print-outline-java
   #'lsp-symbol-outline--print-outline-sorted-java
   #'lsp-symbol-outline--finalize-arg-props-java
   #'lsp-symbol-outline--cycle-arg-visibility-java))



(provide 'lsp-symbol-outline-java)

;;; lsp-symbol-outline-java.el ends here
