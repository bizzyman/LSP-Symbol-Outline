;;; lsp-symbol-outline-C.el --- a symbol tree for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 bizzyman

;; Version: 0.0.3
;; Homepage: https://github.com/bizzyman/LSP-Symbol-Outline
;; Keywords: languages, lsp, outline, C, C++

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
;;; C/C++ lang specific code.

;;; Code:

(defun lsp-symbol-outline--get-symbol-docs-C-cleanup ()
       "Replace non doc chars and get first sentence in doc block."
       (push-mark (point) t)
       (forward-char)
       (re-search-forward "[.?!/\n]" nil t)
       (s-collapse-whitespace (s-replace-regexp
                               "@\\w+" ""
                               (s-replace-all
                                '(("*" . ""))
                                (s-replace-all
                                 '(("/" . ""))
                                 (buffer-substring-no-properties
                                  (mark) (point)))))))

(defun lsp-symbol-outline--get-symbol-docs-C-non-recursive (_plist-item)
       "Move to :symbol-start-point and parse doc block above symbol.
Return first sentence of block as string."
       (vertical-motion -1)
       (if (and (search-forward "*/" (line-end-position) t)
                (forward-comment -1)
                (looking-at "\\/\\*"))
           (lsp-symbol-outline--get-symbol-docs-C-cleanup)))

(defun lsp-symbol-outline--get-symbol-docs-C (_plist-item)
       "Move to :symbol-start-point and parse doc block above symbol.
Return first sentence of block as string."
       (vertical-motion -1)
       (if (and (search-forward "*/" (line-end-position) t)
                (forward-comment -1)
                (looking-at "\\/\\*"))
           (lsp-symbol-outline--get-symbol-docs-C-cleanup)
         (lsp-symbol-outline--get-symbol-docs-C-non-recursive _plist-item)))

(defun lsp-symbol-outline--print-outline-C (list buf)
       "Insert indentation, icon, button and any args into symbol outline buffer.
Iterates over symbol list. C specific argument printing."
       (dolist (item list)
         (plist-put item :line (string-to-number (format-mode-line "%l")))
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

(defun lsp-symbol-outline--print-outline-sorted-C (list-sorted)
       "Print a symbol outline grouped by symbol kind. Takes list of symbol
plists LIST-SORTED and prints the symbol icon, the kind name, symbol button and
any argument information.

LIST-SORTED is filtered to yield only distinct :kind values. LIST-SORTED is then
iterated over and filtered by each distinct :kind value. Symbols of that kind
are printed with no regard to indentation or hierarchy.
C specific."
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
               (plist-put item :line (string-to-number (format-mode-line "%l")))
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

(defun lsp-symbol-outline--finalize-arg-props-C ()
       "Parse buffer for comma char and find argument types. Position of types
is passed to `lsp-symbol-outline--set-arg-type-props' which sets different text
properties on argument type information.

Regex parsing is used to set invisible properties to toggle hiding type
information. C specific."
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

(defun lsp-symbol-outline--set-arg-types-inv-C ()
       "Parse buffer for colon char and find argument types. Call
`lsp-symbol-outline--set-arg-props-inv' on found positions to set argument
information invisible by setting text properties. C specific."
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

(defun lsp-symbol-outline--finalize-arg-props-C ()
       "Parse buffer for comma char and find argument types. Position of types
is passed to `lsp-symbol-outline--set-arg-type-props' which sets different text
properties on argument type information.

Regex parsing is used to set invisible properties to toggle hiding type
information. C specific."
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

(defun lsp-symbol-outline--cycle-arg-visibility-C ()
       "If `lsp-symbol-outline-args-inv' is 0, set only argument types invisible.
If `lsp-symbol-outline-args-inv' is 1, set arguments invisible.
If `lsp-symbol-outline-args-inv' is 2, set all to visible.
C specific."
       (cond
        ;; arg types invisible
        ((equal lsp-symbol-outline-args-inv 0)
         (read-only-mode 0)
         (lsp-symbol-outline--set-arg-types-inv-C) ;TODO same as java
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
           (lsp-symbol-outline--finalize-arg-props-C)) ;TODO same as java
         (setq-local lsp-symbol-outline-args-inv 0)
         (read-only-mode 1))))

(defun lsp-symbol-outline--remove-arg-indices (list)
       "Remove symbols that are already covered by the :args plist prop. "
       (let ((indices))
         (dolist (item list)
           (if (plist-get item :args)
               (let ((m (replace-regexp-in-string "\n" "" (plist-get item :args)))
                     (c))
                 (if m
                     (progn
                       (setq c (s-count-matches "," m))
                       (push (number-sequence (plist-get item :index)
                                              (+ (plist-get item :index)
                                                 (if (equal c 0) 0 c)) 1)
                             indices))))))
         (setq indices (-flatten indices))
         (setf list (-remove-at-indices indices list))))

(defun lsp-symbol-outline--tree-sort-C (list)
       "Sort list of symbol plists into a hierarchical tree. This is done in two
stages. First remove function args from symbols list. Then compare
:symbol-end-point of current symbol - the `global-counter' local var - and find
the next symbol with a higher :symbol-end-point - the `local-counter' local var.
Third, all symbol's depth properties between `global-counter' and
`local-counter' are incremented by one. Repeat for every symbol.

Return tree sorted list of plists.
C specific."
       (setq list (lsp-symbol-outline--remove-arg-indices list))
       (let ((global-counter 0)
             (local-counter 0)
             (local-end 0)
             (list-length (length list)))
         (while (< global-counter list-length)
           ;; check if end-point of current symbol greater than next symbol
           (if (if (and (memq  (plist-get (nth global-counter list)
                                          :kind)
                               '(7 13 14))
                        (memq  (plist-get (nth (1+ global-counter) list)
                                          :kind)
                               '(7 13 14)))
                   (ignore-errors (> (plist-get (nth global-counter list)
                                                :symbol-end-line)
                                     (plist-get (nth (1+ global-counter) list)
                                                :symbol-end-line)))
                 (ignore-errors (> (plist-get (nth global-counter list)
                                              :symbol-end-point)
                                   (plist-get (nth (1+ global-counter) list)
                                              :symbol-end-point))))
               ;; if it is > find the next symbol with end-point
               ;; > than symbol at index global-counter
               (let ((local-counter (1+ global-counter)))
                 (while (ignore-errors
                          (> (plist-get (nth global-counter list)
                                        :symbol-end-point)
                             (plist-get (nth local-counter list)
                                        :symbol-end-point)))
                        (setq local-counter (1+ local-counter)))
                 (setq local-end local-counter)
                 (setq local-counter (1+ global-counter))
                 (while (< local-counter local-end)
                        (plist-put (nth local-counter list)
                                   :depth
                                   (1+ (plist-get (nth local-counter list)
                                                  :depth)))
                        (setq local-counter (1+ local-counter)))))
           (setq global-counter (1+ global-counter))))
       list)



;;;###autoload
(defun lsp-symbol-outline-make-outline-C ()
       "Call `lsp-symbol-outline-create-buffer-window' with C specific
functions. Creates LSP sym ouline buffer."
       (interactive)
       (lsp-symbol-outline-create-buffer-window
        #'lsp-symbol-outline--get-symbol-end-point
        #'lsp-symbol-outline--set-placeholder-depth
        #'lsp-symbol-outline--get-symbol-args-generic
        #'lsp-symbol-outline--get-symbol-docs-C
        #'lsp-symbol-outline--tree-sort-C
        #'lsp-symbol-outline--print-outline-C ;TODO same as java
        #'lsp-symbol-outline--print-outline-sorted-C ;TODO same as java
        #'lsp-symbol-outline--finalize-arg-props-C ;TODO same as java
        #'lsp-symbol-outline--cycle-arg-visibility-C ;TODO same as java
        ))

(provide 'lsp-symbol-outline-C)

;;; lsp-symbol-outline-C.el ends here
