;;; lsp-symbol-outline-php.el --- a symbol tree for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 bizzyman

;; Version: 0.0.3
;; Homepage: https://github.com/bizzyman/LSP-Symbol-Outline
;; Keywords: languages, lsp, outline, php

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
;;; PHP specific code.

;;; Code:

;; Dependencies

(require 'lsp-mode)
(require 'lsp-php)
(require 'lsp-symbol-outline)

;; Defuns

(defun lsp-symbol-outline--get-symbol-docs-php (_plist-item)
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

(defun lsp-symbol-outline--set-arg-types-inv-php ()
       "Parse buffer for comma char and find argument types. Position of types
is passed to `lsp-symbol-outline--set-arg-type-props' which sets different text
properties on argument type information.

Regex parsing is used to set invisible properties to toggle hiding type
information. Go specific."
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward "(" nil 'noerror 1)
           (while
               (progn
                 (push-mark (point) t)
                 (or
                  (re-search-forward ","
                                     (line-end-position) t)
                  (re-search-forward ")"
                                     (line-end-position) t)))
             (if (>
                  (s-count-matches " "
                                   (buffer-substring-no-properties
                                    (mark)
                                    (point)))
                  0)
                 (condition-case err
                     (save-excursion
                       (lsp-symbol-outline--set-arg-props-inv
                        (goto-char (mark))
                        (search-forward " " (line-end-position) t)))
                  ('error nil)))
             (search-forward " " (line-end-position) t))
           (vertical-motion 1))))

(defun lsp-symbol-outline--cycle-arg-visibility-php ()
       "If `lsp-symbol-outline-args-inv' is 0, set only argument types invisible.
If `lsp-symbol-outline-args-inv' is 1, set arguments invisible.
If `lsp-symbol-outline-args-inv' is 2, set all to visible.
Go specific."
       (cond
        ;; arg types invisible
        ((equal lsp-symbol-outline-args-inv 0)
         (read-only-mode 0)
         (lsp-symbol-outline--set-arg-types-inv-php)
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
           (lsp-symbol-outline--finalize-arg-props-php))
         (setq-local lsp-symbol-outline-args-inv 0)
         (read-only-mode 1))))

(defun lsp-symbol-outline--finalize-arg-props-php ()
       "Parse buffer for comma char and find argument types. Position of types
is passed to `lsp-symbol-outline--set-arg-type-props' which sets different text
properties on argument type information.

Regex parsing is used to set invisible properties to toggle hiding type
information. Go specific."
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward "(" nil 'noerror 1)
           (while
               (progn
                 (push-mark (point) t)
                 (or
                  (re-search-forward ","
                                     (line-end-position) t)
                  (re-search-forward ")"
                                     (line-end-position) t)))
             (if (>
                  (s-count-matches " "
                                   (buffer-substring-no-properties
                                    (mark)
                                    (point)))
                  0)
                 (condition-case err
                     (save-excursion
                      (lsp-symbol-outline--set-arg-type-props
                       (goto-char (mark))
                       (search-forward " " (line-end-position) t)))
                  ('error nil)))
             (search-forward " " (line-end-position) t))
           (vertical-motion 1))))

;;;###autoload
(defun lsp-symbol-outline-make-outline-php ()
       "Call `lsp-symbol-outline-create-buffer-window' with PHP specific
functions. Creates LSP sym ouline buffer."
       (interactive)
       (lsp-symbol-outline-create-buffer-window
        #'lsp-symbol-outline--get-symbol-end-point
        #'lsp-symbol-outline--set-placeholder-depth
        #'lsp-symbol-outline--get-symbol-args-generic
        #'lsp-symbol-outline--get-symbol-docs-php
        #'lsp-symbol-outline--tree-sort
        #'lsp-symbol-outline--print-outline-clike-generic
        #'lsp-symbol-outline--print-outline-sorted-clike-generic
        #'lsp-symbol-outline--finalize-arg-props-php
        #'lsp-symbol-outline--cycle-arg-visibility-php))

(provide 'lsp-symbol-outline-php)

;;; lsp-symbol-outline-php.el ends here
