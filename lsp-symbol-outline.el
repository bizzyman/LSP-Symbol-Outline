;;; lsp-symbol-outline.el --- a symbol tree for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 bizzyman

;; Version: 0.0.3
;; Homepage: https://github.com/bizzyman/LSP-Symbol-Outline
;; Keywords: languages, lsp, outline

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

;;
;;  TODO:
;;
;;  - Refactor
;;  - Introduce configuration
;;  - Support More languages: C++, Rust, Go, Elixir, HTML
;;  - Split file into language specific parts
;;  - Debug tree sort function
;;  - Make url based functions async and parallel
;;  - Remove Ternjs and Anaconda as dependency and rely only on lsp for func
;;    arguments and hierarchy/depth parsing
;;  - Add 'context' view like in Nuclide -> shows definition, other info
;;

;;; Code:

;; Dependencies

(require 'lsp-mode)
(require 'outline)
(require 'outline-magic)
(require 's)
(require 'dash)


;; Vars

(defvar lsp-symbol-outline-cursor-sensor-overlay-priority 1000
        "Initial priority for cursor-sensor-functions overlay.")

(defcustom lsp-symbol-outline-window-position
           'right
           "LSP symbol outline window position."
           :group 'lsp-symbol-outline)

(defcustom lsp-symbol-outline-modeline-format
           '((:propertize "%b" face mode-line-buffer-id) " ")
           "Local modeline format for the LSP symbol outline mode."
           :group 'lsp-symbol-outline)

(defconst lsp-symbol-outline-symbol-kind-alist
          '((1  . "File")
            (2  . "Module")
            (3  . "Namespace")
            (4  . "Package")
            (5  . "Class")
            (6  . "Method")
            (7  . "Property")
            (8  . "Field")
            (9  . "Constructor")
            (10 . "Enum")
            (11 . "Interface")
            (12 . "Function")
            (13 . "Variable")
            (14 . "Constant")
            (15 . "String")
            (16 . "Number")
            (17 . "Boolean")
            (18 . "Array"))
          "Alist of symbol kind associations. Used to print correct symbol
kind names.")

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
         ;; '((t (:foreground "white")))
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
         ;; '((t :foreground "#0C1314" :background "#9ea8aa"))
         ;; '((t :foreground "deep sky blue"))
         '((t :inherit default))
         "Face for ."
         :group 'lsp-symbol-outline-faces)


;; Major mode

;;;###autoload
(define-derived-mode lsp-symbol-outline-mode
                     special-mode
                     "S-outline"
                     "Major mode for the LSP Symbol Outline."
                     (read-only-mode 1))


;; Defuns

(defun lsp-symbol-outline--lsp-get-document-symbols ()
       "Get hash table of symbols in current document, uses LSP.
Ensure that lsp-mode is on and enabled."
       (lsp--send-request
        (lsp--make-request "textDocument/documentSymbol"
                           `(:textDocument
                             ,(lsp--text-document-identifier)))))

(defun lsp-symbol-outline--put-plist-name (plist-item hasht-item)
       "Get name from hash table of symbol in HASHT-ITEM and set plist
:name property. Return plist."
       (plist-put plist-item
                  :name
                  (replace-regexp-in-string "\(.+\)"
                                            ""
                                            (gethash "name" hasht-item))))

(defun lsp-symbol-outline--get-symbol-start-line (hasht-range)
       "Get the symbol start line from hash table HASHT-RANGE.
 Return line number."
       (1+ (gethash "line"
                    (gethash "start"
                             hasht-range))))

(defun lsp-symbol-outline--get-symbol-end-point (hasht-range)
       "Get the symbol end point from hash table HASHT-RANGE.
 Return line number."
       (lsp--position-to-point (gethash "end"
                                        hasht-range)))

(defun lsp-symbol-outline--get-symbol-column (hasht-range)
       "Get the symbol start column from hash table HASHT-RANGE.
 Return column number."
       (gethash "character" (gethash "start" hasht-range)))

(defun lsp-symbol-outline--set-placeholder-depth (plist-item)
       "Set symbol depth to 0. Depth updated by `lsp-symbol-outline--tree-sort'.
 Return PLIST-ITEM."
       (plist-put plist-item :depth 0))

(defun lsp-symbol-outline--get-symbol-args-generic (plist-item hasht-range)
       "Find symbol start line, move to opening param delimiting \"(\" and
return buffer contents between parens. Saves excursion so that next operation -
:docs lookup - can continue from :symbol-start-point.

Returns arg string based on whether it is empty or not."
       (goto-char (plist-get plist-item :symbol-start-point))
       (save-excursion
         (search-forward "(" nil t)
         (pcase (buffer-substring-no-properties
                 (progn (forward-char -1) (point))
                 (progn (forward-sexp) (point)))
           ("()" nil)
           (SYMBOL (s-replace-all '(("( " . "(") (" )" . ")"))
                    (s-collapse-whitespace SYMBOL))))))

(defun lsp-symbol-outline--create-symbols-list (sym-end-handler
                                                depth-handler
                                                args-handler
                                                docs-handler)
       "Create a list of plists corresponding to symbols in document.
Properties are inferred by transforming the hash-table returned by
`lsp-symbol-outline--lsp-get-document-symbols' and specific methods for
specific language needs. When such a method is required the function
passed in to the corresponding parameter is applied. For example
getting the lang symbol end position is done by funcalling SYM-END-HANDLER
passed to this function from lang specific file.

Each plist is generated by adding to the the plist-item local variable.
List of plists is returned by the local var agg-items."
       (let ((agg-items)
             (index 1))
         (dolist (hasht-item (lsp-symbol-outline--lsp-get-document-symbols))
           (let ((plist-item)
                 (hasht-kind (gethash "kind" hasht-item))
                 (hasht-range (gethash "range" (gethash "location" hasht-item))))

             ;; 0 - INDEX
             (setq plist-item (plist-put plist-item :index index))

             ;; 1 - NAME
             (setq plist-item
                   (lsp-symbol-outline--put-plist-name plist-item hasht-item))

             ;; 2 - KIND
             (plist-put plist-item :kind hasht-kind)

             ;; 3 & 4 SYMBOL START AND END POSITION
             (if (memq hasht-kind '(5 6 12)) ;is symbol function or class
                 (progn
                   ;; 3 - func/class start range
                   (plist-put plist-item :symbol-start-point
                              (lsp--position-to-point
                               (gethash "start" hasht-range)))
                   ;; 4 - func/class end range
                   (plist-put plist-item :symbol-end-point
                              (funcall sym-end-handler hasht-range)))
               ;; 3 - var start range
               (plist-put plist-item :symbol-start-point
                          (lsp--position-to-point
                           (gethash "start" hasht-range)))
               ;; 4 - var end range
               (plist-put plist-item :symbol-end-point
                          (lsp--position-to-point
                           (gethash "end" hasht-range))))

             ;; SYMBOL END LINE
             (plist-put plist-item :symbol-end-line
                        (gethash "line"
                         (gethash "end" hasht-range)))

             ;; 5 - DEPTH
             (setq plist-item (funcall depth-handler plist-item))

             ;; 6 - COLUMN
             (plist-put plist-item :column
                        (lsp-symbol-outline--get-symbol-column hasht-range))

             ;; get arguments and docstring
             (save-excursion
               (if (or (memq hasht-kind '(6 12))
                       (and (equal major-mode 'python-mode)
                            ;; ??? so all classes get args/docs?
                            (equal 5 (plist-get plist-item :kind))))
                   (progn
                     ;; 7 - ARGS
                     (plist-put plist-item :args (funcall args-handler
                                                          plist-item
                                                          hasht-range))
                     ;; 8 - DOCS
                     (plist-put plist-item :docs (funcall docs-handler
                                                          plist-item)))
                 ;; 7 nil args for vars
                 (plist-put plist-item :args nil)
                 ;; 8 nil docs for vars
                 (plist-put plist-item :docs nil)))

        (setq index (1+ index))
        (push plist-item agg-items)))
    (reverse agg-items)))

(defun lsp-symbol-outline--sort-list-by-index (list)
       "Sort list of plists by their :symbol-start-point property and update
indexes. Return list of plists in order they appear in document."
       (let ((list_ list)
             (index 1))
         (setq list_
               (--sort (< (plist-get it    :symbol-start-point)
                          (plist-get other :symbol-start-point))
                       list_))
        (dolist (item list_)
          (plist-put item :index index)
          (setq index (1+ index)))
        list_))

(defun lsp-symbol-outline--tree-sort (list)
       "Sort list of symbol plists into a hierarchical tree. This is done in two
stages. First compare :symbol-end-point of current symbol - the `global-counter'
local var - and find the next symbol with a higher :symbol-end-point - the
`local-counter' local var. Second, all symbol's depth properties between
`global-counter' and `local-counter' are incremented by one. Repeat for every
symbol.

Return tree sorted list of plists."
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

(defun lsp-symbol-outline-unhighlight-symbol ()
       "Remove idle timer highlighting in symbol outline buffer."
       (with-current-buffer
           lsp-outline-buffer
         (save-excursion
           (remove-overlays (point-min) (point-max)
                            'face 'lsp-symbol-outline-inside-current-symbol))))

(defun lsp-symbol-outline-highlight-symbol ()
       "Go to symbol outline buffer and highlight the symbol inside which point
currently resides."
       (let* ((inhibit-message t)
              (line
               (ignore-errors (overlay-get
                               (-first (lambda (o)
                                         (overlay-get
                                          o 'lsp-symbol-outline-timer-goto-line))
                                       (overlays-at (point) t))
                               'lsp-symbol-outline-timer-goto-line))))
         (if (not line)
             (ignore-errors (lsp-symbol-outline-unhighlight-symbol))
           (ignore-errors
             (with-current-buffer
                 lsp-outline-buffer
               (save-excursion
                 (goto-char (point-min))
                 (forward-line (1- line))
                 (remove-overlays (point-min) (point-max)
                                  'face 'lsp-symbol-outline-inside-current-symbol)
                 (let ((o (make-overlay (line-beginning-position)
                                        (1+(line-beginning-position)) )))
                   (overlay-put o 'display ">")
                   (overlay-put o 'face 'lsp-symbol-outline-inside-current-symbol)
                   (overlay-put o 'priority 99))))))))

(defun lsp-symbol-outline-add-idle-timer-highlight-props (start end line)
       "Ceate an overlay corresponding to :symbol-start-point and
:symbol-end-point that stores the line on which the symbol is printed on in the
symbol outline buffer. Adds `lsp-symbol-outline-timer-goto-line'
properties between START and END."
       (let ((o (make-overlay start end)))
         (overlay-put o
                      'lsp-symbol-outline-timer-goto-line
                      line)))

(defun lsp-symbol-outline--delete-idle-timer-overlays ()
       "Remove all overlays in buffer that have
`lsp-symbol-outline-timer-goto-line' properties."
       (dolist (o (overlays-in (point-min) (point-max)))
         (when (overlay-get o 'lsp-symbol-outline-timer-goto-line)
           (delete-overlay o))))

(defun lsp-symbol-outline--create-buffer ()
       "Create and return a buffer for inserting the LSP symbol outline."
       (get-buffer-create
        (format "*%s-outline*"
                (file-name-sans-extension (buffer-name)))))

(defun lsp-symbol-outline--print-indentation (item)
       "Loop that prints space :depth number of times. Indentation implies
hierarchy."
       (let ((x 0))
         (while (< x
                   (* (plist-get item :depth) 2))
           (progn (insert " ") (setq x (1+ x))))))

(defun lsp-symbol-outline--insert-sym-kind-name (same-kind-list)
       "Insert string based on plist's :kind property. Uses
`lsp-symbol-outline-symbol-kind-alist' for name associations."
       (insert (if (equal (plist-get (car same-kind-list) :kind) 5)
                   (propertize
                    (format "%ses\n"
                            (alist-get (plist-get (car same-kind-list) :kind)
                                       lsp-symbol-outline-symbol-kind-alist))
                    'face 'default)
                 (propertize
                  (format "%ss\n"
                          (alist-get (plist-get (car same-kind-list) :kind)
                                     lsp-symbol-outline-symbol-kind-alist))
                  'face 'default))))

(defun lsp-symbol-outline--print-symbol-icon-gui (item)
       "Inserts the gui version of glyph icon for symbol. Glyphs use
atomicons.ttf font. May appear in source code as the wrong glyps or unicode
placeholders. Outputted correctly when face set."
       (cond
        ((equal (plist-get item :kind) 2)  ;Module
         (insert (propertize " "
                             'face 'lsp-symbol-outline-atom-icons-face
                             'font-lock-ignore 't)))
        ((equal (plist-get item :kind) 5)  ;Class
         (insert (propertize " "
                             'face 'lsp-symbol-outline-atom-icons-face
                             'font-lock-ignore 't)))
        ((equal (plist-get item :kind) 6)  ;Method
         (insert (propertize " "
                             'face 'lsp-symbol-outline-atom-icons-face
                             'font-lock-ignore 't)))
        ((equal (plist-get item :kind) 12) ;Function
         (insert (propertize " "
                             'face 'lsp-symbol-outline-atom-icons-face
                             'font-lock-ignore 't)))
        ((equal (plist-get item :kind) 13) ;Variable
         (insert (propertize " "
                             'face 'lsp-symbol-outline-atom-icons-face
                             'font-lock-ignore 't)))
        ((equal (plist-get item :kind) 14) ;Constant
         (insert (propertize " "
                             'face 'lsp-symbol-outline-atom-icons-face
                             'font-lock-ignore 't)))
        ((equal (plist-get item :kind) 18) ;Array
         (insert (propertize " "
                             'face 'lsp-symbol-outline-atom-icons-face
                             'font-lock-ignore 't)))
        (t
         (insert (propertize " "     ;all other symbol types
                             'face 'lsp-symbol-outline-atom-icons-face
                             'font-lock-ignore 't)))))

(defun lsp-symbol-outline--print-symbol-icon-term (item)
       "Inserts the terminal version of icon for symbol. Uses standard ascii
chars."
       (cond
        ((equal (plist-get item :kind) 2)  ;Module
         (insert (propertize "M "
                             'face 'lsp-symbol-outline-term-symbol-type-name-face
                             'font-lock-ignore 't)))
        ((equal (plist-get item :kind) 5)  ;Class
         (insert (propertize "C "
                             'face 'lsp-symbol-outline-term-symbol-type-name-face
                             'font-lock-ignore 't)))
        ((equal (plist-get item :kind) 6)  ;Method
         (insert (propertize "m "
                             'face 'lsp-symbol-outline-term-symbol-type-name-face
                             'font-lock-ignore 't)))
        ((equal (plist-get item :kind) 12) ;Function
         (insert (propertize "F "
                             'face 'lsp-symbol-outline-term-symbol-type-name-face
                             'font-lock-ignore 't)))
        ((equal (plist-get item :kind) 13) ;Variable
         (insert (propertize "V "
                             'face 'lsp-symbol-outline-term-symbol-type-name-face
                             'font-lock-ignore 't)))
        ((equal (plist-get item :kind) 14) ;Constant
         (insert (propertize "K "
                             'face 'lsp-symbol-outline-term-symbol-type-name-face
                             'font-lock-ignore 't)))
        ((equal (plist-get item :kind) 18) ;Array
         (insert (propertize "A "
                             'face 'lsp-symbol-outline-term-symbol-type-name-face
                             'font-lock-ignore 't)))
        (t
         (insert (propertize "* "       ;all other symbol types
                             'face 'lsp-symbol-outline-term-symbol-type-name-face
                             'font-lock-ignore 't)))))

(defun lsp-symbol-outline--move-point-to-symbol (item buf)
       "Returns a function that moves the point and focus to symbol location in
original document buffer."
       `(lambda (x)
          (switch-to-buffer-other-window ,buf)
          (goto-char ,(plist-get item :symbol-start-point))))

(defun lsp-symbol-outline--print-button (item buf)
       "Print the button that handles the `lsp-symbol-outline-show-docstring-tip'
and `lsp-symbol-outline--move-point-to-symbol' functionality. Docstring
function is handled by letter 'd' - 100 in ascii."
       (insert-button (plist-get item :name)
                      'action (lsp-symbol-outline--move-point-to-symbol item buf)
                      'keymap `(keymap (mouse-2 . push-button)
                                       (100 . (lambda () (interactive)
                                                (lsp-symbol-outline-show-docstring-tip
                                                 ,(if (plist-get item :docs)
                                                      (plist-get item :docs)
                                                    nil)))))
                      'face (cond
                             ((and (plist-get item :docs)
                                   (ignore-errors (not (string-empty-p
                                                        (plist-get item :docs))))
                                   (equal (plist-get item :kind) 12))
                              'lsp-symbol-outline-function-face-has-doc)
                             ((equal (plist-get item :kind) 12)
                              'lsp-symbol-outline-function-face)
                             ((and (plist-get item :docs)
                                   (ignore-errors (not (string-empty-p
                                                        (plist-get item :docs))))
                                   (equal (plist-get item :kind) 6))
                              'lsp-symbol-outline-function-face-has-doc)
                             ((equal (plist-get item :kind) 6)
                              'lsp-symbol-outline-function-face)
                             ((and (plist-get item :docs)
                                   (ignore-errors (not (string-empty-p
                                                        (plist-get item :docs))))
                                   (equal (plist-get item :kind) 5))
                              'lsp-symbol-outline-class-face-has-doc)
                             ((equal (plist-get item :kind) 5)
                              'lsp-symbol-outline-class-face)
                             (t
                              'lsp-symbol-outline-var-face))))

(defun lsp-symbol-outline--find-closest-cell (list current-line)
       "Find the closest line to line that main LSP sym outline function called
from. Return line number."
  (cond ((ignore-errors
           (+ 2 (progn
                  (-elem-index
                   (car
                    (last
                     (-filter
                      (lambda (x) (< (plist-get x :symbol-start-point)
                                     current-line))
                      list)))
                   list)))))
        (t 1)))

(defun lsp-symbol-outline--jump-paren ()
       "Jump to the matching paren."
       (cond ((eq 4 (car (syntax-after (point))))
              (forward-sexp)
              (forward-char -1))
             ((eq 5 (car (syntax-after (point))))
              (forward-char 1)
              (backward-sexp))))

(defun lsp-symbol-outline--set-arg-type-props (beg end)
       "Set text-properties to sym outline arg type face."
       (set-text-properties beg end
                            '(face 'lsp-symbol-outline-arg-type-face)))

(defun lsp-symbol-outline--set-arg-props-inv (beg end)
       "Set text-properties to invisible."
       (set-text-properties beg end
                            '(invisible t)))

(defun lsp-symbol-outline--set-arg-props-vis (beg end)
       "Set text properties of args visible."
       (set-text-properties beg end
                            '(face lsp-symbol-outline-arg-face)))

(defun lsp-symbol-outline--set-info-vis ()
       "Search buffer for parens and set all found positions to visible
text property."
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward "(.+)" nil 'noerror 1)
           (let ((ref (match-string-no-properties 1)))
             (lsp-symbol-outline--set-arg-props-vis
              (point)
              (save-excursion
                (forward-char -1)
                (lsp-symbol-outline--jump-paren)
                (point)))))))

(defun lsp-symbol-outline--set-info-inv ()
       "Search buffer for parens and set all found positions to invisible
text property."
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward "(.*)" nil 'noerror 1)
           (lsp-symbol-outline--set-arg-props-inv
            (point)
            (save-excursion
              (forward-char -1)
              (lsp-symbol-outline--jump-paren)
              (point))))))

(defun lsp-symbol-outline--sort-by-category (list)
       "Sort list of symbol plists by their :kind property.
Returns list of plists."
       (--sort (< (plist-get it :kind) (plist-get other :kind)) list))

(defun lsp-symbol-outline-print-sorted ()
       "Save current line data. Sort list of symbols by category. Call the
function contained by `lsp-symbol-outline-print-sorted-func' to print an
outline grouped by symbol kind. Call function that sets argument text properties.
Set buffer local variable `lsp-symbol-outline-is-sorted' to t. Find saved line
data."
       (let ((line (nth 1 (s-match " +. \\(\\w+\\)"
                                   (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position)))))
             (list-sorted (lsp-symbol-outline--sort-by-category lsp-outline-list))
             (inhibit-read-only t)
             (inhibit-message t))
         (erase-buffer)
         (funcall lsp-symbol-outline-print-sorted-func list-sorted)
         (goto-char (point-min))
         (forward-whitespace 2)
         ;; TODO save arg visibility level between calling sorted/sequential?
         (funcall lsp-symbol-outline-args-props-func)
         (setq-local lsp-symbol-outline-is-sorted t)
         (search-forward-regexp (format "%s\\((\\|$\\)" line) nil t)
         (beginning-of-line-text)
         (with-current-buffer lsp-symbol-outline-src-buffer
           (lsp-symbol-outline--delete-idle-timer-overlays)
           (dolist (i buffer-orig-lsp-outline-list)
             (lsp-symbol-outline-add-idle-timer-highlight-props
              (plist-get i :symbol-start-point)
              (plist-get i :symbol-end-point)
              (plist-get i :line)))
           (lsp-symbol-outline-highlight-symbol))))

(defun lsp-symbol-outline-print-sequential ()
       "Reverse the grouping of symbols by kind and print a symbol outline in
order of symbol appearance in source document."
       (let ((l (nth 1
                     (s-match " +. \\(\\w+\\)"
                              (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position)))))
             (inhibit-read-only t)
             (inhibit-message t))
         (erase-buffer)
         (funcall lsp-symbol-outline-print-func
                  lsp-outline-list
                  lsp-symbol-outline-src-buffer)
         ;; TODO save arg visibility level between calling sorted/sequential?
         (funcall lsp-symbol-outline-args-props-func)
         (setq-local lsp-symbol-outline-is-sorted nil)
         (lsp-symbol-outline-go-top)
         (search-forward-regexp (format "%s\\((\\|$\\)" l) nil t)
         (beginning-of-line-text)
         (forward-to-word 1)
         (with-current-buffer lsp-symbol-outline-src-buffer
           (lsp-symbol-outline--delete-idle-timer-overlays)
           (dolist (i buffer-orig-lsp-outline-list)
             (lsp-symbol-outline-add-idle-timer-highlight-props
              (plist-get i :symbol-start-point)
              (plist-get i :symbol-end-point)
              (plist-get i :line)))
           (lsp-symbol-outline-highlight-symbol))))

;;; The below function is stolen from misc-cmds.el as emacswiki packages
;;; are no longer on melpa TODO implement own

;;;###autoload
(defun goto-longest-line (beg end)
  "Go to the first of the longest lines in the region or buffer.
If the region is active, it is checked.
If not, the buffer (or its restriction) is checked.

Returns a list of three elements:

 (LINE LINE-LENGTH OTHER-LINES LINES-CHECKED)

LINE is the first of the longest lines measured.
LINE-LENGTH is the length of LINE.
OTHER-LINES is a list of other lines checked that are as long as LINE.
LINES-CHECKED is the number of lines measured.

Interactively, a message displays this information.

If there is only one line in the active region, then the region is
deactivated after this command, and the message mentions only LINE and
LINE-LENGTH.

If this command is repeated, it checks for the longest line after the
cursor.  That is *not* necessarily the longest line other than the
current line.  That longest line could be before or after the current
line.

To search only from the current line forward, not throughout the
buffer, you can use `C-SPC' to set the mark, then use this
\(repeatedly)."
  (interactive
   (if (or (not mark-active)  (not (< (region-beginning) (region-end))))
       (list (point-min) (point-max))
     (if (< (point) (mark))
         (list (point) (mark))
       (list (mark) (point)))))
  (when (and (not mark-active) (= beg end)) (error "The buffer is empty"))
  (when (and mark-active (> (point) (mark))) (exchange-point-and-mark))
  (when (< end beg) (setq end (prog1 beg (setq beg end))))
  (when (eq this-command last-command)
    (forward-line 1) (setq beg (point)))
  (goto-char beg)
  (when (eobp) (error "End of buffer"))
  (cond ((<= end (save-excursion (goto-char beg) (forward-line 1) (point)))
         (let ((inhibit-field-text-motion  t))  (beginning-of-line))
         (when (and (> emacs-major-version 21) (require 'hl-line nil t))
           (let ((hl-line-mode  t))  (hl-line-highlight))
           (add-hook 'pre-command-hook #'hl-line-unhighlight nil t))
         (let ((lineno  (line-number-at-pos))
               (chars   (let ((inhibit-field-text-motion t))
                          (save-excursion (end-of-line) (current-column)))))
           (message "Only line %d: %d chars" lineno chars)
           (let ((visible-bell  t))  (ding))
           (setq mark-active  nil)
           (list lineno chars nil 1)))
        (t
         (let* ((start-line                 (line-number-at-pos))
                (max-width                  0)
                (line                       start-line)
                (inhibit-field-text-motion  t)
                long-lines col)
           (when (eobp) (error "End of buffer"))
           (while (and (not (eobp)) (< (point) end))
             (end-of-line)
             (setq col  (current-column))
             (when (>= col max-width)
               (setq long-lines  (if (= col max-width)
                                     (cons line long-lines)
                                   (list line))
                     max-width   col))
             (forward-line 1)
             (setq line  (1+ line)))
           (setq long-lines  (nreverse long-lines))
           (let ((lines  long-lines))
             (while (and lines (> start-line (car lines))) (pop lines))
             (goto-char (point-min))
             (when (car lines) (forward-line (1- (car lines)))))
           (when (and (> emacs-major-version 21) (require 'hl-line nil t))
             (let ((hl-line-mode  t))  (hl-line-highlight))
             (add-hook 'pre-command-hook #'hl-line-unhighlight nil t))
           (when (interactive-p)
             (let ((others  (cdr long-lines)))
               (message "Line %d: %d chars%s (%d lines measured)"
                (car long-lines) max-width
                (concat
                 (and others
                      (format ", Others: {%s}" (mapconcat
                                                (lambda (line) (format "%d" line))
                                                (cdr long-lines) ", "))))
                (- line start-line))))
           (list (car long-lines) max-width (cdr long-lines) (- line start-line))))))

(defun lsp-symbol-outline--find-longest-line ()
       "Find the longest line in buffer."
       (save-excursion (goto-longest-line (point-min)
                                          (point-max))
                       (end-of-line)
                       (ceiling (* 1.15 (1+ (current-column))))))

; Interactive defuns

;;;###autoload
(defun lsp-symbol-outline-create-buffer-window (sym-end-handler
                                                depth-handler
                                                args-handler
                                                docs-handler
                                                tree-sort-handler
                                                print-handler
                                                print-sorted-handler
                                                arg-props-handler
                                                visibility-cycle-handler)
       "Create and set-up the main LSP sym outline buffer. This is the
entry-point for the functionality of lsp-sym-outline package. This function is a
template to be called from language specific files, where relevant handler
functions for each language will be passed in.

Buffer is first checked for any changes since last call of this function by
calculating md5. If no changes have occured bypass generating new symbol list
and use old one instead."
       (interactive)
       (if (not lsp-mode)
           (lsp-mode))

       (let ((inhibit-message t)
             (current-pos (point))
             (lsp-outline-list
              ;; Caching
              ;; (if (and (boundp 'buffer-hash-value)
              ;;          (equal buffer-hash-value
              ;;                 (md5 (buffer-substring-no-properties (point-min)
              ;;                                                      (point-max)))))
              ;;     buffer-orig-lsp-outline-list
              ;;   (funcall tree-sort-handler (lsp-symbol-outline--sort-list-by-index
              ;;                               (lsp-symbol-outline--create-symbols-list
              ;;                                sym-end-handler
              ;;                                depth-handler
              ;;                                args-handler
              ;;                                docs-handler))))
              (funcall tree-sort-handler (lsp-symbol-outline--sort-list-by-index
                                          (lsp-symbol-outline--create-symbols-list
                                           sym-end-handler
                                           depth-handler
                                           args-handler
                                           docs-handler)))

              )
             (mod major-mode)
             (buf (current-buffer))
             (window (ignore-errors (split-window
                                     (selected-window)
                                     (- 0 (/ (frame-width) 6))
                                     lsp-symbol-outline-window-position)))
             (outline-buffer (lsp-symbol-outline--create-buffer)))


         (setq-local buffer-orig-lsp-outline-list lsp-outline-list)
         (setq-local lsp-outline-buffer outline-buffer)
         (setq-local buffer-hash-value
                     (md5 (buffer-substring-no-properties (point-min)
                                                          (point-max))))

         (when window
           (window--display-buffer outline-buffer window 'window nil t)
           window)

         (pop-to-buffer outline-buffer)
         (erase-buffer)

         (setq-local lsp-outline-buf-mode (symbol-name mod))

         ;; print the outline
         (funcall print-handler lsp-outline-list buf)
         ;; color the arguments and types
         (funcall arg-props-handler)
         (lsp-symbol-outline-mode)

         ;; Remove mode-line
         (setq-local mode-line-format nil)
         ;; Set initial visibility level of args
         (setq-local lsp-symbol-outline-args-inv 0)
         (setq-local lsp-outline-buf-mode (symbol-name mod))
         (setq-local lsp-outline-list lsp-outline-list)
         (setq-local lsp-symbol-outline-src-buffer buf)
         ;; start buffer in hierarchical view, not sorted view
         (setq-local lsp-symbol-outline-is-sorted nil)
         ;; set the visibility cycling function
         (setq-local lsp-symbol-outline-visibility-cycling-func
                     visibility-cycle-handler)
         ;; set the print function
         (setq-local lsp-symbol-outline-print-func
                     print-handler)
         ;; set the print sorted function
         (setq-local lsp-symbol-outline-print-sorted-func
                     print-sorted-handler)
         ;; set the arg text props function
         (setq-local lsp-symbol-outline-args-props-func
                     arg-props-handler)

         (with-current-buffer lsp-symbol-outline-src-buffer
             ;; add run-with-idle-timer for detecing which symbol cursor in
           (dolist (i lsp-outline-list)
             (lsp-symbol-outline-add-idle-timer-highlight-props
              (plist-get i :symbol-start-point)
              (plist-get i :symbol-end-point)
              (plist-get i :line)))
           (setq-local lsp-s-o-idle-timer-highlight
                       (run-with-idle-timer 0.3 t
                                            #'lsp-symbol-outline-highlight-symbol)))

         ;; Outline mode for folding symbols
         (outline-minor-mode 1)
         (make-local-variable 'outline-regexp)
         (setq outline-regexp "^\\ +[^ ]")

         ;; Changing outline ellipsis from ... to +
         (set-display-table-slot
          standard-display-table
          'selective-display
          (let ((face-offset (* (face-id 'lsp-symbol-outline-button-face)
                                (lsh 1 22))))
            (vconcat (mapcar (lambda (c) (+ face-offset c)) " +"))))

         ;; go to closest line that function was called from
         (goto-char (point-min))
         (forward-line
          (1- (lsp-symbol-outline--find-closest-cell lsp-outline-list current-pos)))
         (if (not (looking-at-p " *[^ ] "))
             (forward-whitespace 1)
           (forward-whitespace 2))

         ;; HACK to stop window size jumping around
         (setq window-size-fixed 'width)
         (toggle-truncate-lines 1)))    ;TODO make quiet

(defun lsp-symbol-outline-cycle-arg-vis ()
       "Call the function returned by the buffer local variable
`lsp-symbol-outline-visibility-cycling-func' to cycle the visibility of
function arguments in outline buffer. Visibility can be in three states.
1. Args and type information visible.
2. Just args visible.
3. No args visible.

Funcall calls the function passed to and set by
`lsp-symbol-outline-create-buffer-window' when setting up the sym outline
buffer."
       (interactive)
       (funcall lsp-symbol-outline-visibility-cycling-func))

(defun lsp-symbol-outline-toggle-sorted ()
       "Toggle whether symbol outline is grouped by symbol kind."
       (interactive)
       (if lsp-symbol-outline-is-sorted
           (lsp-symbol-outline-print-sequential)
         (lsp-symbol-outline-print-sorted)))

(defun lsp-symbol-outline-show-docstring-tip (doc)
       "Show a summary of docstring in echo area if available."
       (interactive)
       (if doc
           (message doc)))

(defun lsp-symbol-outline-widen-to-widest-column ()
       "Widen the window to the width of longest line in buffer."
       (interactive)
       (setq window-size-fixed nil)
       (enlarge-window (- (lsp-symbol-outline--find-longest-line)
                          (window-width (selected-window)))
                       t)
       (setq window-size-fixed 'width))

(defun lsp-symbol-outline-up-scope ()
       "Move up the hiearchy to symbol's parent."
       (interactive)
       (outline-up-heading 1 nil)
       (forward-whitespace 2))

(defun lsp-symbol-outline-up-sibling ()
       "Move up to previous symbol sibling."
       (interactive)
       (let ((opoint (point))
             (indent (current-indentation)))
            (lsp-symbol-outline-previous-line)
            (while (and (not (equal (current-indentation)
                                    indent))
                        (not (eobp)))
              (if (< (current-indentation)
                     indent)
                  (progn
                    (message "No more siblings in direction")
                    (goto-char opoint))
                (lsp-symbol-outline-previous-line)))))

(defun lsp-symbol-outline-down-sibling ()
       "Move down to next symbol sibling."
       (interactive)
       (let ((opoint (point))
             (indent (current-indentation)))
            (lsp-symbol-outline-next-line)  ;DONE does not work in sorted view
            (while (and (not (= (current-indentation)
                                    indent))
                        (not (eobp)))
              (if (< (current-indentation)
                     indent)
                  (progn
                    (message "No more siblings in direction")
                    (goto-char opoint))
                (lsp-symbol-outline-next-line)))))

(defun lsp-symbol-outline-previous-line ()
       "Go to previous symbol. Moves point to the beginning of symbol name."
       (interactive)
       (vertical-motion -1)
       (while (looking-at "$")
              (vertical-motion -1))
       (if (not (looking-at-p " *[^ ] "))
           (forward-whitespace 1)
           (forward-whitespace 2)))

(defun lsp-symbol-outline-next-line ()
       "Go to next symbol. Moves point to the beginning of symbol name."
       (interactive)
       (vertical-motion 1)
       (while (looking-at "$")
         (vertical-motion 1))
       (if (not (looking-at-p " *[^ ] "))
           (forward-whitespace 1)
           (forward-whitespace 2)))

(defun lsp-symbol-outline-toggle-folding ()
       "Fold the local tree at point. Hides symbols in scope below current
symbol's."
       (interactive)
       (outline-cycle)
       (forward-whitespace 2))

(defun lsp-symbol-outline-go-top ()
       "Go to top of symbol outline tree."
       (interactive)
       (goto-char (point-min))
       (forward-whitespace 2))

(defun lsp-symbol-outline-go-to-bottom ()
       "Go to bottom of symbol outline tree."
       (interactive)
       (end-of-buffer)
       (vertical-motion -1)
       (forward-whitespace 2))

(defun lsp-symbol-outline-peek ()
       "Find location of symbol in source buffer but do not lose focus of symbol
outline buffer."
       (interactive)
       (let ((w (selected-window)))
            (push-button)
            (select-window w)))

(defun lsp-symbol-outline-mark-symbol ()
       "Mark symbol name at point."
       (interactive)
       (when (not (use-region-p))
         (let ((b (bounds-of-thing-at-point 'symbol)))
           (goto-char (car b))
           (set-mark (cdr b)))))

(defun lsp-symbol-outline-kill-window ()
       "Kill the lsp-symbol-outline window and remove cursor-sensor-functions."
       (interactive)
       (with-current-buffer
           lsp-symbol-outline-src-buffer
         (lsp-symbol-outline--delete-idle-timer-overlays)
         (cancel-timer lsp-s-o-idle-timer-highlight)
         (setq lsp-s-o-idle-timer-highlight nil))
       (remove-overlays (point-min) (point-max)
                        'face 'lsp-symbol-outline-inside-current-symbol)
       (kill-buffer-and-window))


;; Keybindings

(define-key lsp-symbol-outline-mode-map
            (kbd  "j")
            #'lsp-symbol-outline-next-line)
(define-key lsp-symbol-outline-mode-map
            (kbd  "k")
            #'lsp-symbol-outline-previous-line)
(define-key lsp-symbol-outline-mode-map
            (kbd  "TAB")
            #'outline-hide-sublevels)
(define-key lsp-symbol-outline-mode-map
            (kbd  "<backtab>")
            #'outline-show-all)
(define-key lsp-symbol-outline-mode-map
            (kbd  "f")
            #'lsp-symbol-outline-toggle-folding)
(define-key lsp-symbol-outline-mode-map
            (kbd  "q")
            #'lsp-symbol-outline-kill-window)
(define-key lsp-symbol-outline-mode-map
            (kbd  "gg")
            #'lsp-symbol-outline-go-top)
(define-key lsp-symbol-outline-mode-map
            (kbd  "G")
            #'lsp-symbol-outline-go-to-bottom)
(define-key lsp-symbol-outline-mode-map
            (kbd  "o")
            #'push-button)
(define-key lsp-symbol-outline-mode-map
            (kbd  "i")
            #'lsp-symbol-outline-cycle-arg-vis)
(define-key lsp-symbol-outline-mode-map
            (kbd  "gh")
            #'lsp-symbol-outline-up-scope)
(define-key lsp-symbol-outline-mode-map
            (kbd  "gk")
            #'lsp-symbol-outline-up-sibling)
(define-key lsp-symbol-outline-mode-map
            (kbd  "gj")
            #'lsp-symbol-outline-down-sibling)
(define-key lsp-symbol-outline-mode-map
            (kbd  "w")
            #'lsp-symbol-outline-widen-to-widest-column)
(define-key lsp-symbol-outline-mode-map
            (kbd  "s")
            #'lsp-symbol-outline-toggle-sorted)
(define-key lsp-symbol-outline-mode-map
            (kbd  "l")
            #'lsp-symbol-outline-peek)
(define-key lsp-symbol-outline-mode-map
            (kbd  "d")
            #'lsp-symbol-outline-show-docstring-tip)
(define-key lsp-symbol-outline-mode-map
            (kbd  "m")
            #'lsp-symbol-outline-mark-symbol)


(provide 'lsp-symbol-outline)

;;; lsp-symbol-outline.el ends here
