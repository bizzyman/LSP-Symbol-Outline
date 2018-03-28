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
;;  - Support More languages: C++, Rust, Go, PHP, Ruby, Elixir, HTML
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
(require 'lsp-symbol-outline-custom)
(require 'lsp-symbol-outline-faces)
(require 'outline)
(require 'outline-magic)
(require 's)
(require 'dash)


;; Vars

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
                  (pcase
                   (replace-regexp-in-string "\(.+\)"
                                             ""
                                             (gethash "name" hasht-item))
                   ("" "*anon*")
                   (SYMBOL SYMBOL))))

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

(defun lsp-symbol-outline--get-symbol-end-point-clike-generic (hasht-range)
       "Get the symbol end point by moving point to end position in
HASHT-RANGE and jumping to matching } brace. Return line number.
For use with languages that have C/Java like syntax."
       (save-excursion
         (if
             (progn
               (goto-char
                (lsp--position-to-point (gethash "start"
                                                 hasht-range)))
               (beginning-of-line)
               (move-to-column
                (gethash "character"
                         (gethash "end"
                                  hasht-range)))
               ;; TODO bound can fail if args on multiple lines
               (search-forward "{" (line-end-position) t))
             (progn
               (backward-char)
               (lsp-symbol-outline--jump-paren)
               (1+ (point)))
           (lsp-symbol-outline--get-symbol-end-point hasht-range))))

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

(defun lsp-symbol-outline--sort-list-by-start (list)
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

(defun lsp-symbol-outline--remove-arg-indices (list)
       "Remove symbols that are already covered by the :args plist prop."
       (let ((indices))
         (dolist (item list)
           (if (plist-get item :args)
               (let ((m (replace-regexp-in-string "\n" "" (plist-get item :args)))
                     (c))
                 (cond
                  ((string-match "( *void *)" m) nil)
                  (m
                   (progn (setq c (s-count-matches "," m))
                          (push (number-sequence (plist-get item :index)
                                                 (+ (plist-get item :index)
                                                    (if (equal c 0) 0 c)) 1)
                                indices)))))))
         (setq indices (-flatten indices))
         (setf list (-remove-at-indices indices list))))

(defun lsp-symbol-outline--tree-sort-rem-args (list)
       "Sort list of symbol plists into a hierarchical tree. This is done in two
stages. First remove function args from symbols list. Then compare
:symbol-end-point of current symbol - the `global-counter' local var - and find
the next symbol with a higher :symbol-end-point - the `local-counter' local var.
Third, all symbol's depth properties between `global-counter' and
`local-counter' are incremented by one. Repeat for every symbol.

Return tree sorted list of plists."
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
                   (overlay-put o 'display
                                lsp-symbol-outline-position-indicator-char)
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

(defun lsp-symbol-outline--print-outline-clike-generic (list buf)
       "Insert indentation, icon, button and any args into symbol outline buffer.
Iterates over symbol list. For use with langs that resemeble C/Java in syntax."
       (dolist (item list)
         (plist-put item :line (string-to-number (format-mode-line "%l")))
         (insert "  ")
         ;; indentation
         (lsp-symbol-outline--print-indentation item)
         ;; icon
         (if (and window-system
                  lsp-symbol-outline-print-fancy-glyphs)
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

(defun lsp-symbol-outline--print-outline-sorted-clike-generic (list-sorted)
       "Print a symbol outline grouped by symbol kind. Takes list of symbol
plists LIST-SORTED and prints the symbol icon, the kind name, symbol button and
any argument information.

LIST-SORTED is filtered to yield only distinct :kind values. LIST-SORTED is then
iterated over and filtered by each distinct :kind value. Symbols of that kind
are printed with no regard to indentation or hierarchy.
For use with langs that resemeble C/Java in syntax."
       (let ((contains-types (-distinct (-map
                                         (lambda (x) (plist-get x :kind))
                                         list-sorted))))
         (dolist (sym-kind contains-types)
           (let ((same-kind-list
                  (-filter (lambda (i) (equal (plist-get i :kind) sym-kind))
                           list-sorted)))
             ;; icon
             (insert "  ")
             (if (and window-system
                      lsp-symbol-outline-print-fancy-glyphs)
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

(defun lsp-symbol-outline--finalize-arg-props-clike-generic ()
       "Parse buffer for comma char and find argument types. Position of types
is passed to `lsp-symbol-outline--set-arg-type-props' which sets different text
properties on argument type information.

Regex parsing is used to set invisible properties to toggle hiding type
information. For use with langs that have C/Java like syntax."
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
           (vertical-motion 1)))
       ;; highlight "void" symbols
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward "( *?void *?)" nil t)
         (lsp-symbol-outline--set-arg-type-props
          (1- (point))
          (save-excursion
            (progn
             (search-backward "(" (line-beginning-position) t)
             (+ (point) 1)))))))

(defun lsp-symbol-outline--set-arg-types-inv-clike-generic ()
       "Parse buffer for comma char and find argument types. Call
`lsp-symbol-outline--set-arg-props-inv' on found positions to set argument
information invisible by setting text properties.
For use with langs that have C/Java like syntax."
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

(defun lsp-symbol-outline--cycle-arg-visibility-clike-generic ()
       "If `lsp-symbol-outline-args-inv' is 0, set only argument types invisible.
If `lsp-symbol-outline-args-inv' is 1, set arguments invisible.
If `lsp-symbol-outline-args-inv' is 2, set all to visible.
For use with langs that have C/Java like syntax."
       (cond
        ;; arg types invisible
        ((equal lsp-symbol-outline-args-inv 0)
         (read-only-mode 0)
         (lsp-symbol-outline--set-arg-types-inv-clike-generic)
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
           (lsp-symbol-outline--finalize-arg-props-clike-generic))
         (setq-local lsp-symbol-outline-args-inv 0)
         (read-only-mode 1))))

(defun lsp-symbol-outline--find-end-of-arg-type-colon-generic ()
       "Parse current line to find the end range of type information of current
arg. For use with langs that delimit arg types with colons."
       (cond
        ((looking-at " fn")
         (search-forward "(")
         (backward-char 1)
         (goto-char (plist-get (sp-get-sexp) :end)))
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

(defun lsp-symbol-outline--finalize-arg-props-colon-generic ()
       "Parse buffer for colon char and find argument types. Position of types
is passed to `lsp-symbol-outline--set-arg-type-props' which sets different text
properties on argument type information.

Regex parsing is used to set invisible properties to toggle hiding type
information. For use with langs that delimit arg types with colons."
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward ":" nil 'noerror 1)
           (let ((ref (match-string-no-properties 1)))
             (lsp-symbol-outline--set-arg-type-props
              (1- (point))
              (lsp-symbol-outline--find-end-of-arg-type-colon-generic))))))

(defun lsp-symbol-outline--set-arg-types-inv-colon-generic ()
       "Parse buffer for colon char and find argument types. Call
`lsp-symbol-outline--set-arg-props-inv' on found positions to set argument
information invisible by setting text properties. For use with langs that delimit arg types with colons."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":" nil 'noerror 1)
      (let ((ref (match-string-no-properties 1)))
        (lsp-symbol-outline--set-arg-props-inv
         (1- (point))
         (lsp-symbol-outline--find-end-of-arg-type-colon-generic))))))

(defun lsp-symbol-outline--cycle-arg-visibility-colon-generic ()
       "If `lsp-symbol-outline-args-inv' is 0, set only argument types invisible.
If `lsp-symbol-outline-args-inv' is 1, set arguments invisible.
If `lsp-symbol-outline-args-inv' is 2, set all to visible.
For use with langs that delimit arg types with colons."
       (cond
        ;; arg types invisible
        ((equal lsp-symbol-outline-args-inv 0)
         (read-only-mode 0)
         (lsp-symbol-outline--set-arg-types-inv-colon-generic)
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
           (lsp-symbol-outline--finalize-arg-props-colon-generic))
         (setq-local lsp-symbol-outline-args-inv 0)
         (read-only-mode 1))))

(defun lsp-symbol-outline--sort-by-category (list)
       "Sort list of symbol plists by their :kind property.
Returns list of plists."
       (--sort (< (plist-get it :kind) (plist-get other :kind)) list))

(defun lsp-symbol-outline--save-arg-level (arg-level)
       "Set level of visibility for arguments and their types."
       (cond
        ((eq arg-level 0) 2)
        ((eq arg-level 1) 0)
        ((eq arg-level 2) 1)))

(defun lsp-symbol-outline-print-sorted ()
       "Save current line data. Sort list of symbols by category. Call the
function contained by `lsp-symbol-outline-print-sorted-func' to print an
outline grouped by symbol kind. Call function that sets argument text properties.
Set buffer local variable `lsp-symbol-outline-is-sorted' to t. Find saved line
data."
       (let ((line (nth 1 (s-match " +. \\([a-zA-Z0-9_-]+\\)"
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

         (funcall lsp-symbol-outline-args-props-func)
         ;;save arg visibility level between calling sorted/sequential
         (setq-local lsp-symbol-outline-args-inv
                     (lsp-symbol-outline--save-arg-level
                      lsp-symbol-outline-args-inv))
         (if (not (eq 2 lsp-symbol-outline-args-inv))
             (funcall lsp-symbol-outline-visibility-cycling-func)
           (setq-local lsp-symbol-outline-args-inv 0))
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
                     (s-match " +. \\([a-zA-Z0-9_-]+\\)"
                              (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position)))))
             (inhibit-read-only t)
             ;; (inhibit-message t)
             )
         (erase-buffer)
         (funcall lsp-symbol-outline-print-func
                  lsp-outline-list
                  lsp-symbol-outline-src-buffer)

         (funcall lsp-symbol-outline-args-props-func)
         ;;save arg visibility level between calling sorted/sequential
         (setq-local lsp-symbol-outline-args-inv
                     (lsp-symbol-outline--save-arg-level
                      lsp-symbol-outline-args-inv))
         (if (not (eq 2 lsp-symbol-outline-args-inv))
             (funcall lsp-symbol-outline-visibility-cycling-func)
           (setq-local lsp-symbol-outline-args-inv 0))
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
                       (ceiling (* 1.01 (1+ (current-column))))))

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
              ;;   (funcall tree-sort-handler (lsp-symbol-outline--sort-list-by-start
              ;;                               (lsp-symbol-outline--create-symbols-list
              ;;                                sym-end-handler
              ;;                                depth-handler
              ;;                                args-handler
              ;;                                docs-handler))))
              (funcall tree-sort-handler (lsp-symbol-outline--sort-list-by-start
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
                                     (eval lsp-symbol-outline-window-size)
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
         (lsp-symbol-outline-mode)

         (setq-local lsp-outline-buf-mode (symbol-name mod))
         ;; set the print function
         (setq-local lsp-symbol-outline-print-func
                     print-handler)
         ;; set the print sorted function
         (setq-local lsp-symbol-outline-print-sorted-func
                     print-sorted-handler)
         ;; Remove mode-line
         (setq-local mode-line-format lsp-symbol-outline-modeline-format)
         ;; Set initial visibility level of args
         (setq-local lsp-symbol-outline-args-inv
                     lsp-symbol-outline-default-arg-visibility)
         (setq-local lsp-outline-buf-mode (symbol-name mod))
         (setq-local lsp-outline-list lsp-outline-list)
         (setq-local lsp-symbol-outline-src-buffer buf)
         ;; set the visibility cycling function
         (setq-local lsp-symbol-outline-visibility-cycling-func
                     visibility-cycle-handler)
         ;; set the arg text props function
         (setq-local lsp-symbol-outline-args-props-func
                     arg-props-handler)
         ;; print the outline
         (if lsp-symbol-outline-start-sorted
             (lsp-symbol-outline-print-sorted)
           (lsp-symbol-outline-print-sequential))
         ;; create overlays that highlight which symbol cursor is in
         (if lsp-symbol-outline-enable-position-indicator
          (with-current-buffer lsp-symbol-outline-src-buffer
            ;; add run-with-idle-timer for detecing which symbol cursor in
            (dolist (i lsp-outline-list)
              (lsp-symbol-outline-add-idle-timer-highlight-props
               (plist-get i :symbol-start-point)
               (plist-get i :symbol-end-point)
               (plist-get i :line)))
            (setq-local lsp-s-o-idle-timer-highlight
                        (run-with-idle-timer 0.3 t
                                             #'lsp-symbol-outline-highlight-symbol))
            (lsp-symbol-outline-highlight-symbol)))

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

         ;; fold?
         (if lsp-symbol-outline-startup-folded
             (outline-hide-sublevels 3))

         ;; HACK to stop window size jumping around
         (setq window-size-fixed lsp-symbol-outline-lock-window)
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
       (enlarge-window (if lsp-symbol-outline-max-window-width
                           (min lsp-symbol-outline-max-window-width
                            (- (lsp-symbol-outline--find-longest-line)
                               (window-width (selected-window))))
                        (- (lsp-symbol-outline--find-longest-line)
                           (window-width (selected-window))))
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
       (while (and (not (eobp))
               (looking-at "$"))
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
            (kbd  "C-n")
            #'lsp-symbol-outline-next-line)
(define-key lsp-symbol-outline-mode-map
            (kbd  "C-p")
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
            (kbd  "M-<")
            #'lsp-symbol-outline-go-top)
(define-key lsp-symbol-outline-mode-map
            (kbd  "M->")
            #'lsp-symbol-outline-go-to-bottom)
(define-key lsp-symbol-outline-mode-map
            (kbd  "o")
            #'push-button)
(define-key lsp-symbol-outline-mode-map
            (kbd  "i")
            #'lsp-symbol-outline-cycle-arg-vis)
(define-key lsp-symbol-outline-mode-map
            (kbd  "C-M-u")
            #'lsp-symbol-outline-up-scope)
(define-key lsp-symbol-outline-mode-map
            (kbd  "M-p")
            #'lsp-symbol-outline-up-sibling)
(define-key lsp-symbol-outline-mode-map
            (kbd  "M-n")
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
