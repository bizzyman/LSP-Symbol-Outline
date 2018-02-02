;;; lsp-symbol-outline.el --- a symbol tree for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 bizzyman

;; Version: 0.0.1
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
;; Call (lsp-symbol-outline-create-buffer-window) to enable.
;;
;;  TODO:
;;
;;  - Refactor
;;  - Introduce configuration
;;  - Support More languages: C++, Rust, Go, Elixir, HTML
;;  - Split file into language specific parts
;;  - Debug tree sort function
;;  - Make request based functions async and parallel
;;  - Remove Ternjs and Anaconda as dependency and rely only on lsp for func
;;    arguments and hierarchy/depth parsing
;;

;;; Code:

;; Dependencies

(require 'lsp-mode)
(require 'ov)
(require 'request)
(require 'outline-magic)
(require 's)
(require 'dash)
(require 'tern)
(require 'misc-cmds)
(require 'misc)
(require 'outline)
(require 'sgml-mode)


;; Vars

(defcustom lsp-symbol-outline-window-position
           'right
           "LSP symbol outline window position."
           :group 'lsp-symbol-outline)

(defcustom lsp-symbol-outline-modeline-format
           '((:propertize "%b" face mode-line-buffer-id) " ")
           "Local modeline format for the LSP symbol outline mode."
           :group 'lsp-symbol-outline)


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
         ;; '((t :inherit font-lock-type-face))
         '((t :inherit font-lock-preprocessor-face))
         "Face for outline node arguments."
         :group 'lsp-symbol-outline-faces)

(defface lsp-symbol-outline-html-tag-props-face
         '((t :foreground "#75B5AA"))
         "Face for outline html props."
         :group 'lsp-symbol-outline-faces)


;; Major mode

;;;###autoload
(define-derived-mode lsp-symbol-outline-mode
                     special-mode
                     "S-outline"
                     "Major mode for the LSP Symbol Outline."
                     (read-only-mode 1))


;; Defuns

(defun lsp-symbol-outline-widen-to-widest-column ()
       (interactive)
       (setq window-size-fixed nil)
       (enlarge-window (- (lsp-symbol-outline-find-longest-line)
                          (window-width (selected-window)))
                       t)
       (setq window-size-fixed 'width))

(defun lsp-symbol-outline-find-longest-line ()
       (save-excursion (goto-longest-line (point-min)
                                          (point-max))
                       (end-of-line)
                       (ceiling (* 1.15 (1+ (current-column))))))

(defun lsp-symbol-outline-create-buffer ()
       (get-buffer-create
        (format "*%s-outline*"
                (file-name-sans-extension (buffer-name)))))

(defun lsp-symbol-outline-up-scope ()
       (interactive)
       (outline-up-heading 1 nil)
       (forward-whitespace 2))

(defun lsp-symbol-outline-up-sibling ()
       (interactive)
       (let ((indent (current-indentation)))
            (lsp-symbol-outline-previous-line)
            (while (and (not (equal (current-indentation)
                                    indent))
                        (not (eobp)))
                   (lsp-symbol-outline-previous-line))))

(defun lsp-symbol-outline-down-sibling ()
       (interactive)
       (let ((indent (current-indentation)))
            (outline-next-line)
            (while (and (not (equal (current-indentation)
                                    indent))
                        (not (eobp)))
                   (outline-next-line))
            (forward-whitespace 2)))

(defun lsp-symbol-outline-forward-sexp ()
       (interactive)
       (let ((indent (current-indentation)))
            (forward-line 1)
            (while (and (> (current-indentation) indent)
                        (not (eobp)))
                   (forward-line 1)
                   (end-of-line))
            (forward-line -1)
            (end-of-line)))

(defun lsp-symbol-outline-previous-line ()
       (interactive)
       ;; (ignore-errors (previous-line))
       (vertical-motion -1)
       ;; (forward-line -1)
       ;; (beginning-of-line)
       (while (looking-at "$")
         ;; (ignore-errors (previous-line))
              (vertical-motion -1))
       ;; (beginning-of-line)
       ;; (forward-whitespace 2)
       (if (not (looking-at-p " *[^ ] "))
           (forward-whitespace 1)
           (forward-whitespace 2)))

(defun lsp-symbol-outline-next-line-my ()
  (interactive)
  ;; (forward-line 1)
  ;; (evil-next-visual-line)
  ;; (next-line)
  (vertical-motion 1)
  ;; (beginning-of-line)
  (if (not (looking-at-p " *[^ ] "))
      (forward-whitespace 1)
    ;; (forward-to-word 2)
      (forward-whitespace 2)))

(defun lsp-symbol-outline-overlay-at-point-p ()
       (ov-in 'invisible
              t
              (point)
              (save-excursion (forward-line 1)
                              (point))))

(defun lsp-symbol-outline-toggle-folding ()
       (interactive)
       (outline-cycle)
       (forward-whitespace 2))

(defun lsp-symbol-outline-go-top ()
       (interactive)
       (beginning-of-buffer)
       (forward-whitespace 2))

(defun lsp-symbol-outline-go-to-bottom ()
       (interactive)
       (end-of-buffer )
       (vertical-motion -1)
       (forward-whitespace 2))

(defun lsp-symbol-outline-peek ()
       (interactive)
       (let (;; (w (window-numbering-get-number))
             (w (selected-window)))
            (push-button)
            (select-window w)
         ;; (select-window-by-number w)
            ))

;;;###autoload
(defun lsp-symbol-outline-create-buffer-window ()
  (interactive)
  (if (not lsp-mode)
      (lsp-mode)
    )
  ;; (setq timing-var2 0.0)
  (let ((current-line (string-to-number (format-mode-line "%l")))

        (outline-list

         ;; Caching

         ;; (if (and (boundp 'buffer-hash-value) (equal buffer-hash-value
         ;;                                             (md5 (buffer-substring-no-properties (point-min) (point-max)))))
         ;;     buffer-orig-outline-list
         ;;   (lsp-symbol-outline-tree-sort (lsp-symbol-outline-sort-list (lsp-symbol-outline-get-symbols-list)) 0))

         (progn
          ;; (profiler-start 'cpu)
           (my-new-func (lsp-symbol-outline-sort-list
                         (lsp-symbol-outline-get-symbols-list))
                        )
          )
         )


        (mod major-mode)
        (buf (current-buffer))
        (window (ignore-errors (split-window
                                ;; (frame-root-window)
                                (selected-window)
                                (- 0
                                   (/ (frame-width) 6)) lsp-symbol-outline-window-position)))
        (outline-buffer (lsp-symbol-outline-create-buffer))
        )

    ;; (profiler-report)
    ;; (profiler-stop)

    (setq-local buffer-orig-outline-list outline-list)
    (setq-local buffer-hash-value (md5 (buffer-substring-no-properties (point-min) (point-max))))

    (when window
      (window--display-buffer outline-buffer window 'window nil t)
      window)

    (pop-to-buffer outline-buffer)
    (erase-buffer)

    (setq-local outline-buf-mode (symbol-name mod))
    (lsp-symbol-outline-print-outline outline-list buf)

    (if (equal mod 'java-mode)
        (lsp-symbol-outline-source-to-final-java)
     (lsp-symbol-outline-source-to-final))

    (lsp-symbol-outline-mode)

    (setq-local mode-line-format nil)

    (setq-local inv 0)
    (setq-local outline-buf-mode (symbol-name mod))
    (setq-local outline-list outline-list)
    (setq-local orig-buffer buf)
    (setq-local sorted nil)

    (outline-minor-mode 1)

    (make-local-variable 'outline-regexp)
    (setq outline-regexp "^\\ +[^ ]")

    (set-display-table-slot
     standard-display-table
     'selective-display
     (let ((face-offset (* (face-id 'lsp-symbol-outline-button-face) (lsh 1 22))))
       (vconcat (mapcar (lambda (c) (+ face-offset c)) " +"))))

    ;; (evil-goto-first-line)
    ;; (forward-whitespace)
    (goto-line (lsp-symbol-outline-find-closest-cell outline-list current-line))
    (if (not (looking-at-p " *[^ ] "))
        (forward-whitespace 1)
      (forward-whitespace 2)
      )
    (setq window-size-fixed 'width)
    (toggle-truncate-lines 1)
    )
  )


(defun lsp-symbol-outline-find-closest-cell (list current-line)
  (cond ((ignore-errors (+ 2 (progn
                 (-elem-index (car (last (-filter (lambda (x) (< (plist-get x :symbol-start-line) current-line)) list))) list)))))
        (t 1))
  )

(defun lsp-symbol-outline-lsp-get-document-symbols ()
  (lsp--send-request
   (lsp--make-request "textDocument/documentSymbol"
                      `(:textDocument ,(lsp--text-document-identifier)))))


;; (lsp--send-request
;;  (lsp--make-request "textDocument/signatureHelp"
;;                     '(:textDocument (:uri "file:///usr/local/lib/python3.5/dist-packages/pyls/python_ls.py") :position (:line  50 :character 19))
;;                     ))

;; (:textDocument (:uri "file:///usr/local/lib/python3.5/dist-packages/pyls/python_ls.py")) (:position (:line  46) (:character 9))

;; (lsp--send-request (lsp--make-request
;;                     "textDocument/hover"
;;                     '(:textDocument (:uri "file:///usr/local/lib/python3.5/dist-packages/pyls/python_ls.py") :position (:line  51 :character 8))))





(defun lsp-symbol-outline-tern ()
  (let ((opening-paren 21797))
    (tern-run-query (lambda (data)
                      (let ((type (tern-parse-function-type data)))
                        (when type
                          (setf tern-last-argument-hints (cons opening-paren type))
                          (tern-show-argument-hints))))
                    `((type . "type")
                      (preferFunction . t))
                    opening-paren
                    :silent)))






(defun lsp-symbol-outline-tern-update-argument-hints (pos)
  (let ( ll)
    (tern-run-query (lambda (data)
                      (let ((type (tern-parse-function-type data)))
                        (when type
                          (setf tern-last-argument-hints (cons pos type))
                          (if data
                              (progn
                                (setq ll (lsp-symbol-outline-tern-show-argument-hints))
                                (if (not ll)
                                    (cl-destructuring-bind (name args ret) data
                                      (setq ll args)
                                      )
                                  )
                                )
                            )
                          ))
                      )
                    `((type . "type")
                      (preferFunction . t))
                    pos
                    :silent)

    (sleep-for 0 10 )

    ll

    )


  )

(defun lsp-symbol-outline-tern-show-argument-hints ()
  (cl-destructuring-bind (paren . type) tern-last-argument-hints
    (let ((parts ()) aaa
          (current-arg (tern-find-current-arg paren)))
      (cl-destructuring-bind (name args ret) type
        (push (propertize name 'face 'font-lock-function-name-face) parts)
        (push "(" parts)
        (setq aaa args)
        (cl-loop for arg in args for i from 0 do
                 (unless (zerop i) (push ", " parts))
                 (let ((name (or (car arg) "?")))
                   (push (if (eq i current-arg) (propertize name 'face 'highlight) name) parts))
                 (unless (equal (cdr arg) "?")
                   (push ": " parts)
                   (push (propertize (cdr arg) 'face 'font-lock-type-face) parts)))
        (push ")" parts)
        (when ret
          (push " -> " parts)
          (push (propertize ret 'face 'font-lock-type-face) parts)))
      (let (message-log-max)
        (tern-message (apply #'concat (nreverse parts))))
      aaa
      )))


(require 'deferred)

(defun lsp-symbol-outline-jump-paren ()
    "Go to the matching paren."
    (cond ((eq 4 (car (syntax-after (point))))
           (forward-sexp)
           (forward-char -1))
          ((eq 5 (car (syntax-after (point))))
           (forward-char 1)
           (backward-sexp))
          )
  )

(defun my-deferred-request ()
  (let*
      ((concatted-var)
       (url-mime-charset-string nil)
       (url-request-method "POST")
       (deactivate-mark nil)
       (url-request-data (format
                          "{\"query\":{\"end\":%s,\"file\":\"%s\",\"type\":\"type\",\"preferFunction\":true}}"
                          196 (buffer-file-name)))
       (url-show-status nil)
       (url (url-parse-make-urlobj "http" nil nil tern-server tern-known-port "/" nil nil nil))
       (url-current-object url))
  (deferred:$
    (deferred:parallel
      (deferred:url-retrieve url)
      (deferred:url-retrieve url)
      (deferred:url-retrieve url)
      )
    (deferred:nextc it (lambda (buffers)
                         (cl-loop for i in buffers
                                  do
                                  (with-current-buffer i
                                    (push (alist-get 'type
                                                (json-read-from-string (car (s-match "{.*}" (buffer-substring-no-properties
                                                                                             (point-min) (point-max))))))
                                          concatted-varrr
                                          )))
                         )))
  ;; concatted-var
  ))

(defun my-lsp-make-request-deferred-new ()
  (let ((nd (deferred:new #'identity)))
    (lsp--send-request-async (lsp--make-request
                              "textDocument/hover"
                              `(:textDocument (:uri "file:///usr/local/lib/python3.6/dist-packages/pyls/python_ls.py")
                                              :position
                                              (:line
                                               16
                                               :character 13)))
                             (lambda (x) (deferred:callback-post nd x) )
                             )
    nd))

(defun my-lsp-make-request-deferred-new ()
  (let ((nd (deferred:new #'identity)))
    (deferred:callback-post nd (lsp--send-request (lsp--make-request
                                                   "textDocument/hover"
                                                   `(:textDocument (:uri "file:///usr/lib/erlang/lib/jinterface-1.8/java_src/com/ericsson/otp/erlang/AbstractConnection.java")
                                         :position
                                         (:line
                                          147
                                          :character 30)))
                        ))
    nd))

;; (setq-local alist-var nil)
;; (push '(another . lol) alist-var)
;; (alist-get )

(defun lsp-callback-func (x &rest args)
  ;; (message (gethash "value" (car (gethash "contents" x))))
  ;; x
  )



(defun simple-lsp-hover-req-async ()
  (lsp--send-request-async (lsp--make-request
                           "textDocument/hover"
                           `(:textDocument (:uri "file:///usr/lib/erlang/lib/jinterface-1.8/java_src/com/ericsson/otp/erlang/AbstractConnection.java")
                                           :position
                                           (:line
                                            147
                                            :character 30)))
                           ;; #'intern
                           (lambda (x) (message "HELLO"))
                          ))

;; (message (gethash "value" (car (gethash "contents" (lsp--send-request (lsp--make-request
;;                                                                "textDocument/hover"
;;                                                                `(:textDocument (:uri "file:///home/a/Dropbox/java_test_files/New_Text_Document.java")
;;                                                                                :position
;;                                                                                (:line
;;                                                                                 557
;;                                                                                 :character 39)))
;;                                                               )))))

;; (dotimes (i 603)
;;   (simple-lsp-hover-req-async)
;;   )

;; (dotimes (i 603) (message
;;                   (gethash "contents" (lsp--send-request (lsp--make-request
;;                                        "textDocument/hover"
;;                                        `(:textDocument (:uri "file:///usr/local/lib/python3.6/dist-packages/pyls/python_ls.py")
;;                                                        :position
;;                                                        (:line
;;                                                         16
;;                                                         :character 13)))))
;;                   )
;;          )


;; (dotimes (i 603) (message (gethash "value" (car (gethash "contents"  (lsp--send-request (lsp--make-request
;;                                       "textDocument/hover"
;;                                       `(:textDocument (:uri "file:///usr/lib/erlang/lib/jinterface-1.8/java_src/com/ericsson/otp/erlang/AbstractConnection.java")
;;                                                       :position
;;                                                       (:line
;;                                                        147
;;                                                        :character 30)))
;;                                      ;; (lambda (x) (message (gethash "value" (car (gethash "contents" x)))))
;;                                      )))))
;;          )



(defun my-deferred-request--lsp ()
  (let*
      ((concatted-var ))
  (deferred:$
    (deferred:parallel

      (my-lsp-make-request-deferred-new)
      (my-lsp-make-request-deferred-new)
      (my-lsp-make-request-deferred-new)

      )
    (deferred:nextc it (lambda (buffers)
                         (cl-loop for i in buffers
                                  do
                                  (push (ignore-errors (gethash "value" (car (gethash "contents" i))))  concatted-varrr)
                                  )
                         ))
    )
  ;; concatted-var
  )
  )




(defun lsp-symbol-outline-tern-request-sync (linenum)
  (let* ((concatted-var)
         (url-mime-charset-string nil)
         (url-request-method "POST")
         (deactivate-mark nil)
         (url-request-data (format
                            "{\"query\":{\"end\":%s,\"file\":\"%s\",\"type\":\"type\",\"preferFunction\":true}}"
                            linenum
                            (buffer-file-name)
                            ))
         (url-show-status nil)
         (url (url-parse-make-urlobj "http" nil nil tern-server tern-known-port "/" nil nil nil))
         (url-current-object url))
    (alist-get 'type (json-read-from-string
                      (with-current-buffer (url-retrieve-synchronously url)
                        (car (s-match "{.*}" (buffer-substring-no-properties (point-min) (point-max)))))))
    )
  )




(defun lsp-symbol-outline-get-symbols-list ()
  ;; get name, kind, line and character info
  ;; (setq timing-var 0.0)
  (let ((agg-items ) (index 1))
    (dolist (item (lsp-symbol-outline-lsp-get-document-symbols))
      ;; (message "%s" (princ item))
      (let ((ind-item ) )


        ;; 0 INDEX
        (setq ind-item (plist-put ind-item :index index))

        ;; 1 - NAME
        (setq ind-item (plist-put ind-item :name (replace-regexp-in-string "\(.+\)" "" (gethash "name" item))))

        ;; (if (equal (plist-get (reverse ind-item) :name) "fulfilled")
        ;;     (if
        ;;         't
        ;;         nil
        ;;       nil)
        ;;   nil
        ;;   )

        ;; 2 - KIND
        (plist-put ind-item :kind (gethash "kind" item))

        (if (and (or (equal (gethash "kind" item) 5) (equal (gethash "kind" item) 6) (equal (gethash "kind" item) 12)) (equal major-mode 'java-mode))
            (progn
              (save-excursion
                (goto-line
                 (1+ (gethash "line" (gethash "end" (gethash "range" (gethash "location" item)))))
                )
                (move-to-column
                 (gethash "character" (gethash "end" (gethash "range" (gethash "location" item))))
                 )

                ;; (while (progn
                ;;          (search-forward "{")
                ;;          (backward-char)
                ;;          (in-string-p)
                ;;          )
                ;;   )
                (search-forward "{")
                (backward-char)

                ;; 2 - java func start range
                (plist-put ind-item :symbol-start-line (1+ (gethash "line" (gethash "start" (gethash "range" (gethash "location" item))))))
                (lsp-symbol-outline-jump-paren)
                ;; 3 - java func end range
                (plist-put ind-item :symbol-end-line (line-number-at-pos))

                )
              )

            (progn
              ;; 2 - var start range
              (plist-put ind-item :symbol-start-line (1+ (gethash "line" (gethash "start" (gethash "range" (gethash "location" item))))))
              ;; 3 - var end range
              (plist-put ind-item :symbol-end-line (1+ (gethash "line" (gethash "end" (gethash "range" (gethash "location" item))))))
             )
          )

        ;; 4 - DEPTH ?
        ;; (plist-put ind-item :depth (pcase (gethash "depth" item) (`nil 0)))
        (if (equal major-mode 'python-mode)
            (progn
             (plist-put ind-item :indent-depth
                        (save-excursion
                          (goto-line (plist-get ind-item :symbol-start-line))
                          (/ (current-indentation) 4)
                          )
                        )
             (if (ignore-errors (= (plist-get ind-item :indent-depth) (plist-get (car agg-items) :indent-depth)))
                 (plist-put ind-item :depth (plist-get (car agg-items) :indent-depth))
               (plist-put ind-item :depth
                          (if (ignore-errors (> (plist-get ind-item :indent-depth) (plist-get (car agg-items) :indent-depth)))
                          (1+ (plist-get (car agg-items) :indent-depth))
                          (plist-get ind-item :indent-depth)
                          )
                          )
                 )
             )

          (plist-put ind-item :depth 0))
        ;; 5 - COLUMN
        (plist-put ind-item :column (gethash "character" (gethash "start" (gethash "range" (gethash "location" item)))))


        ;;debug



        ;; get arguments and docstring

        (if (or (equal 6 (plist-get ind-item :kind)) ;; (equal 5 (plist-get ind-item :depth))
                (equal 12 (plist-get ind-item :kind))
                (and (equal major-mode 'python-mode)
                     (equal 5 (plist-get ind-item :kind)))
                )

            (plist-put ind-item :args (cond

                   (
                    (memq major-mode '(js-mode js2-mode))
                    (let ((lk
                           (lsp-symbol-outline-tern-request-sync
                            (save-excursion
                              (goto-line (plist-get ind-item :symbol-start-line))
                                 (move-to-column (plist-get ind-item :column))

                                 (search-forward "(")
                                 (backward-char)

                                 ;; 6 - js2 docstring
                                 (save-excursion
                                   (goto-line (1- (plist-get ind-item :symbol-start-line)))

                                   (if (search-forward "*/" (line-end-position) t)
                                       (plist-put ind-item :docs (progn
                                         (search-backward "/**")
                                         (forward-char 3)
                                         (s-collapse-whitespace
                                          (s-chop-prefix "\n"
                                           (s-replace-regexp "/\\*\\*" ""
                                            (s-replace-regexp " +\\* " ""
                                             (buffer-substring-no-properties
                                              (point)
                                              (progn (forward-sentence) (point)))))))
                                         ))
                                     nil
                                     ))


                                 (1- (point))
                                 )
                       )
                      ))
                      ;; (replace-regexp-in-string ": [^ )\\|]+" "" lk )
                      lk
                      )
                    ;; 7 - js2 args
                    )



                 (
                    (equal major-mode 'java-mode)
                    (let ((docs
                           (save-excursion
                             (goto-line (1- (plist-get ind-item :symbol-start-line)))

                             (if (search-forward "*/" (line-end-position) t)
                                 (progn
                                   (search-backward "/**")
                                   (forward-char 3)
                                   ;; (forward-line 1)
                                   ;; (s-replace-regexp " +\\* " "" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                                   (s-collapse-whitespace
                                    (s-chop-prefix "\n"
                                      (s-replace-regexp "<.+?>" ""
                                       (s-replace-regexp "/\\*\\*" ""
                                        (s-replace-regexp " +\\* " "" (thing-at-point 'sentence t))))))
                                   )
                               ))))
                      (if docs
                          ;; 6 java docstring
                          (plist-put ind-item
                                     :docs docs
                                     )
                        (plist-put ind-item :docs nil)
                        )

                      ;; 7 java args

                      (let ((arg (save-excursion
                               (goto-line (plist-get ind-item :symbol-start-line))
                               (search-forward "(" (line-end-position) t)
                               (buffer-substring-no-properties
                                (progn (forward-char -1) (point))
                                (progn (forward-sexp) (point)))
                               )))
                        (if (equal arg "()") nil (s-collapse-whitespace arg))
                        )

                    )
                    )

                 (
                  (equal major-mode 'python-mode)

                  (progn
                    (let ((contts
                           (save-excursion
                             (goto-line (plist-get ind-item :symbol-start-line))
                             (forward-line 1)
                             (if
                                 (search-forward "\"\"\"" (line-end-position) t)
                                 (s-collapse-whitespace
                                  (s-chop-prefix "\n" (buffer-substring-no-properties
                                                       (point)
                                                       (progn (forward-sentence) (point)))))
                             )
                             )
                           ))

                      ;; 6 python docstring
                      (if contts
                          (progn
                            (plist-put ind-item
                                       :docs contts
                                       )
                            )
                        (plist-put ind-item :docs nil)
                        )

                      ;; 7 python args
                      (let ((arg (save-excursion
                                   (goto-line (plist-get ind-item :symbol-start-line))
                                   (search-forward "(" (line-end-position) t)
                                   (buffer-substring-no-properties
                                    (progn (forward-char -1) (point))
                                    (progn (forward-sexp) (point)))
                                   )))
                        (if (equal arg "()") nil (s-collapse-whitespace arg))
                        )
                      )
                    )

                    )

                   )
                  )

          (progn

            ;; 6 nil docs for vars
            (plist-put ind-item :docs nil)

            ;; 7 nil args for vars
            (plist-put ind-item :args nil)
            )

          )

        ;; 8 DEPTH?
        ;; (if (gethash "depth" item) (push `(python-depth ,(gethash "depth" item)) ind-item)  (push '(python-depth nil) ind-item))

        (setq index (1+ index))
        (push ind-item agg-items)
        )
      )
    (reverse agg-items)
    )
  )

;; list of kind associations
;; '((1 . "File")
;;  (2 . "Module")
;;  (3 . "Namespace")
;;  (4 . "Package")
;;  (5 . "Class")
;;  (6 . "Method")
;;  (7 . "Property")
;;  (8 . "Field")
;;  (9 . "Constructor"),
;;  (10 . "Enum")
;;  (11 . "Interface")
;;  (12 . "Function")
;;  (13 . "Variable")
;;  (14 . "Constant")
;;  (15 . "String")
;;  (16 . "Number")
;;  (17 . "Boolean")
;;  (18 . "Array"))


(defun lsp-symbol-outline-show-docstring-tip (item)
  (interactive)
  (if item
      ;; (if (window-system)
      ;;     (pos-tip-show item nil nil nil 2)
      ;;   (message item)
      ;;   )
      (message item)
    )
  )

(defun lsp-symbol-outline-print-outline (list buf)
  (dolist (item list)
    ;;   (and
    ;;    (save-excursion
    ;;      (vertical-motion -1)
    ;;      (looking-at (format " %s" (make-string (* 4 (truncate (plist-get item :depth))) 32)) )
    ;;      )
    ;;    (save-excursion

    ;;      (search-forward "(" (line-end-position) t 1)
    ;;      (search-forward (format "%s" (plist-get item :name)) (line-end-position) t 1)
    ;;      )
    ;;    (or (equal (plist-get item :kind) 13) (equal (plist-get item :kind) 14))
    ;;    )
    ;; (delete item list)
    (insert " ")
    (let ((x 0)) (while (< x (* (plist-get item :depth) 2)) (progn (insert " ") (setq x (1+ x)))) )
    (cond
     ((equal (plist-get item :kind) 2)
      (if window-system
          (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
        (insert (propertize "M " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
        )
      )
     ((equal (plist-get item :kind) 5)  (if window-system
                                  (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize "C " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (plist-get item :kind) 6)  (if window-system
                                  (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize "m " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (plist-get item :kind) 12) (if window-system
                                  (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize "F " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (plist-get item :kind) 13) (if window-system
                                  (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize "V " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (plist-get item :kind) 14) (if window-system
                                  (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize "K " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (plist-get item :kind) 18) (if window-system
                                  (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize "A " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     (t (if window-system
            (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
          (insert (propertize "* " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
          ))
     )
    (insert-button (plist-get item :name) 'action `(lambda (x)
                                         (switch-to-buffer-other-window ,buf)
                                         (goto-line ,(plist-get item :symbol-start-line))
                                         (move-to-column ,(plist-get item :column))
                                         )

                   'keymap `(keymap (mouse-2 . push-button)  (100 . (lambda () (interactive) (lsp-symbol-outline-show-docstring-tip
                                                                                              ,(if (plist-get item :docs) (plist-get item :docs) nil))
                                                                      )))


                   'face (cond
                          ((and (plist-get item :docs) (ignore-errors (not (string-empty-p (plist-get item :docs))))
                                (equal (plist-get item :kind) 12)) 'lsp-symbol-outline-function-face-has-doc)
                          ((equal (plist-get item :kind) 12) 'lsp-symbol-outline-function-face)
                          ((and (plist-get item :docs) (ignore-errors (not (string-empty-p (plist-get item :docs))))
                                (equal (plist-get item :kind) 6)) 'lsp-symbol-outline-function-face-has-doc)
                          ((equal (plist-get item :kind) 6) 'lsp-symbol-outline-function-face)
                          ((and (plist-get item :docs) (ignore-errors (not (string-empty-p (plist-get item :docs))))
                                (equal (plist-get item :kind) 5)) 'lsp-symbol-outline-class-face-has-doc)
                          ((equal (plist-get item :kind) 5) 'lsp-symbol-outline-class-face)
                          (t 'lsp-symbol-outline-var-face)
                          )


                   )

    ;; "int num - Test.main(...).Foo.bar(int, int)"

    (if (plist-get item :args) (let ((arg-string
                            (cond ((equal outline-buf-mode "java-mode")
                                (ignore-errors (replace-regexp-in-string "\n" ""
                                         (replace-regexp-in-string " -> .+" ""
                                              (plist-get item :args)))))

                                  ((equal outline-buf-mode "python-mode")
                                   (car (s-match  "\(.+\)"
                                    (s-collapse-whitespace
                                     (replace-regexp-in-string "\n" ""
                                      (replace-regexp-in-string " -> .+" ""
                                                                (plist-get item :args)))))))
                                  (t
                                   (car (s-match  "\(.+?\)\)?"
                                    (s-collapse-whitespace
                                     (replace-regexp-in-string "\n" ""
                                        (replace-regexp-in-string " -> .+" ""
                                          (plist-get item :args)))))))
                              )

                                       ))

                       (if arg-string
                           (progn
                             ;; (insert "\n")
                             ;; (let ((x 0)) (while (< x (* (plist-get item :depth) 4)) (progn (insert " ") (setq x (1+ x)))) )
                             (insert
                              (propertize
                               arg-string
                               'face 'lsp-symbol-outline-arg-face 'font-lock-ignore 't))))))
    (insert "\n")
    )
  )







;;;; TOGGLE VISIBILITY

;; non-html

(defun lsp-symbol-outline-source-to-final-java ()
  "Cut refs from the txt, but letting them appear as text properties."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ")" nil 'noerror 1)
      ;; (kill-word -1)

      (search-backward " " (line-beginning-position) t)
      (lsp-symbol-outline-set-some-overlay-or-textproperty-here
       (point)
       (progn
         (or (search-backward "," (line-beginning-position) t)
             (search-backward "(" (line-beginning-position) t))
         (+ (point) 1)
         )
       )
      (while (save-excursion (or
                              (search-backward "," (line-beginning-position) t)
                              (search-backward "(" (line-beginning-position) t)))
        (search-backward " " (line-beginning-position) t)
        (lsp-symbol-outline-set-some-overlay-or-textproperty-here
         (point)
         (progn
           (or (search-backward "," (line-beginning-position) t)
               (search-backward "(" (line-beginning-position) t))
           (+ (point) 1)
           )
         )
        )
      (vertical-motion 1)

      )))

(defun lsp-symbol-outline-set-arg-types-inv-java ()
  "Cut refs from the txt, but letting them appear as text properties."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ")" nil 'noerror 1)
      ;; (kill-word -1)

      (search-backward " " (line-beginning-position) t)

      (let ((p (point)) (e))
        (cond ((search-backward "," (line-beginning-position) t)
               (setq e (+ (point) 1)))
              ((search-backward "(" (line-beginning-position) t)
               (progn (setq e (+ (point) 1)) (setq p (1+ p))))
              )

        (when e (lsp-symbol-outline-set-arg-textproperty-inv
          p e
          )))

      (while (save-excursion (or
                              (search-backward "," (line-beginning-position) t)
                              (search-backward "(" (line-beginning-position) t)))

        (search-backward " " (line-beginning-position) t)

        (let ((p (point)) (e))
          (cond ((search-backward "," (line-beginning-position) t) (setq e (+ (point) 1)))
                ((search-backward "(" (line-beginning-position) t) (progn (setq e (+ (point) 1)) (setq p (1+ p))))
                )

          (lsp-symbol-outline-set-arg-textproperty-inv
           p e
           ))
        )
      (vertical-motion 1)

      )))


(defun lsp-symbol-outline-source-to-final ()
  "Cut refs from the txt, but letting them appear as text properties."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":" nil 'noerror 1)
      ;; (kill-word -1)
      (let ((ref (match-string-no-properties 1)))
        (lsp-symbol-outline-set-some-overlay-or-textproperty-here (1- (point))

                                               (cond
                                                ((looking-at " fn")
                                                 (search-forward "(")
                                                 (backward-char 1)
                                                 (goto-char (plist-get (sp-get-sexp) :end))
                                                 )
                                                (
                                                 )
                                                ((looking-at " {")
                                                 (search-forward "{")
                                                 (backward-char 1)
                                                 (lsp-symbol-outline-jump-paren)

                                                 (if
                                                     (looking-at ".|")
                                                     (progn
                                                       (forward-char 2)
                                                       (cond ((lsp-symbol-outline-jump-paren)
                                                              (point)
                                                              )
                                                             ((search-forward "," (line-end-position) t)
                                                              (1- (point))
                                                              )
                                                             ((search-forward ")" (line-end-position) t)
                                                              (1- (point))
                                                              )
                                                           )
                                                      )
                                                   (1+ (point))
                                                     )

                                                 )
                                                (t (progn
                                                     (backward-char 1)
                                                     (if (re-search-forward ": .+?," (line-end-position) t 1)
                                                         (1- (point))
                                                       (re-search-forward ": .+?$" (line-end-position) t 1)
                                                       (- (point) 1)
                                                       )
                                                     )))

                                               ;; (cond
                                               ;;                                        ((looking-back "{}" 2) (point))
                                               ;;                                        ((looking-back "}" 1) (1- (point)))
                                               ;;                                        ((looking-at ")") (point))
                                               ;;                                        ((looking-back ":" 1) (forward-word 1) (point))
                                               ;;                                        (t (1- (point)))
                                               ;;                                        )
                                               ;;                                       (save-excursion (search-backward " " nil t 1) (point))
                                               )

        ))))


;; (replace-regexp-in-string ": [^ )\\|]+" "" lk )

(defun lsp-symbol-outline-set-some-overlay-or-textproperty-here (beg end)
  (set-text-properties beg end
                       '(face 'lsp-symbol-outline-arg-type-face)
                       ;; '(invisible t)
                       ;; (propertize ,(buffer-substring-no-properties beg end) 'face 'font-lock-constant-face))
                       ))



(defun lsp-symbol-outline-set-arg-types-inv ()
  "Cut refs from the txt, but letting them appear as text properties."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":" nil 'noerror 1)
      ;; (kill-word -1)
      (let ((ref (match-string-no-properties 1)))
        (lsp-symbol-outline-set-arg-textproperty-inv (1- (point))

                                  (cond
                                   ((looking-at " fn")
                                    (search-forward "(")
                                    (backward-char 1)
                                    (goto-char (plist-get (sp-get-sexp) :end))
                                    )
                                   ((looking-at " {")
                                    (search-forward "{")
                                    (backward-char 1)
                                    (lsp-symbol-outline-jump-paren)

                                    (if
                                                     (looking-at ".|")
                                                     (progn
                                                       (forward-char 2)
                                                       (cond ((lsp-symbol-outline-jump-paren)
                                                              (point)
                                                              )
                                                             ((search-forward "," (line-end-position) t)
                                                              (1- (point))
                                                              )
                                                             ((search-forward ")" (line-end-position) t)
                                                              (1- (point))
                                                              )
                                                           )
                                                      )
                                                   (1+ (point))
                                                     )

                                    )
                                   (t (progn
                                        (backward-char 1)
                                        (if (re-search-forward ": .+?," (line-end-position) t 1)
                                            (1- (point))
                                          (re-search-forward ": .+?$" (line-end-position) t 1)
                                          (- (point) 1)
                                          )
                                        )))

                                  ;; (cond
                                  ;;                                        ((looking-back "{}" 2) (point))
                                  ;;                                        ((looking-back "}" 1) (1- (point)))
                                  ;;                                        ((looking-at ")") (point))
                                  ;;                                        ((looking-back ":" 1) (forward-word 1) (point))
                                  ;;                                        (t (1- (point)))
                                  ;;                                        )
                                  ;;                                       (save-excursion (search-backward " " nil t 1) (point))
                                  )

        ))))







;; (defun lsp-symbol-outline-set-arg-types-inv ()
;;   "Cut refs from the txt, but letting them appear as text properties."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward ": \\([^ )]+\\)" nil 'noerror 1)
;;       ;; (kill-word -1)
;;       (let ((ref (match-string-no-properties 1)))
;;         (lsp-symbol-outline-set-arg-textproperty-inv (cond
;;                                    ((looking-back "{}" 2) (point))
;;                                    ((looking-back "}" 1) (1- (point)))
;;                                    ((looking-at ")") (point))
;;                                    ((looking-back ":" 1) (forward-word 1) (point))
;;                                    (t (1- (point)))
;;                                    )
;;                                                (save-excursion (search-backward ":" nil t 1) (point))
;;                                                )

;;         ))))



(defun lsp-symbol-outline-set-arg-textproperty-inv (beg end)
  ;; (let ((ovv (make-overlay beg end )))

  ;; (overlay-put ovv 'invisible t)
  ;; )
  (set-text-properties beg end
                       '(invisible t)
                       ;; (propertize ,(buffer-substring-no-properties beg end) 'face 'font-lock-constant-face))
                       )
  )




(defun lsp-symbol-outline-set-info-vis ()
  "Cut refs from the txt, but letting them appear as text properties."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "(.+)" nil 'noerror 1)
      ;; (kill-word -1)
      (let ((ref (match-string-no-properties 1)))
        (lsp-symbol-outline-set-properties-vis (point)
                            (save-excursion
                              ;; (search-backward "(" nil t 1)
                              (forward-char -1)
                              (lsp-symbol-outline-jump-paren)
                              (point))
                            )

        ))))


(defun lsp-symbol-outline-set-properties-vis (beg end)
  (set-text-properties beg end
                       '(face lsp-symbol-outline-arg-face)
                       ;; (propertize ,(buffer-substring-no-properties beg end) 'face 'font-lock-constant-face))
                       )
  )



(defun lsp-symbol-outline-set-info-inv ()
  "Cut refs from the txt, but letting them appear as text properties."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "(.*)" nil 'noerror 1)
      ;; (kill-word -1)
      (lsp-symbol-outline-set-arg-textproperty-inv (point)
                                (save-excursion
                                  ;; (re-search-backward "(" nil t )
                                  (forward-char -1)
                                  (lsp-symbol-outline-jump-paren)
                                  (point))
                                ))))



(defun lsp-symbol-outline-cycle-vis ()
  (interactive)
  (cond
   ((member outline-buf-mode '("js-mode" "js2-mode" "java-mode"))
    (cond
     ((equal inv 0)
      (read-only-mode 0)
      (if (equal outline-buf-mode "java-mode")
          (lsp-symbol-outline-set-arg-types-inv-java)
       (lsp-symbol-outline-set-arg-types-inv))
      (setq-local inv 1)
      (read-only-mode 1)
      )

     ((equal inv 1)
      (read-only-mode 0)
      (lsp-symbol-outline-set-info-inv)
      (setq-local inv 2)
      (read-only-mode 1)
      )

     ((equal inv 2)
      (read-only-mode 0)
      (progn
        (remove-list-of-text-properties (point-min) (point-max) '(invisible ))
        (lsp-symbol-outline-set-info-vis)
        (if (equal outline-buf-mode "java-mode")
            (lsp-symbol-outline-source-to-final-java)
         (lsp-symbol-outline-source-to-final))
        )
      (setq-local inv 0)
      (read-only-mode 1)
      )
     ))

   ((equal outline-buf-mode "python-mode")
    (cond
     ((equal inv 0)
      (read-only-mode 0)
      (lsp-symbol-outline-set-info-inv)
      (setq-local inv 1)
      (read-only-mode 1)
      )

     ((equal inv 1)
      (read-only-mode 0)
      (progn
        (remove-list-of-text-properties (point-min) (point-max) '(invisible ))
        (lsp-symbol-outline-set-info-vis)
        (lsp-symbol-outline-source-to-final)
        )
      (setq-local inv 0)
      (read-only-mode 1)
      )
     ))

   ((equal outline-buf-mode "web-mode")
    (cond
     ((equal inv 0)
      (read-only-mode 0)
      (lsp-symbol-outline-set-classes-inv)
      (setq-local inv 1)
      (read-only-mode 1)
      )

     ((equal inv 1)
      (read-only-mode 0)
      (lsp-symbol-outline-set-html-info-inv)
      (setq-local inv 2)
      (read-only-mode 1)
      )

     ((equal inv 2)
      (read-only-mode 0)
      (progn
        (remove-list-of-text-properties (point-min) (point-max) '(invisible ))
        (lsp-symbol-outline-set-html-info-vis)
        (lsp-symbol-outline-set-classes-normal)
        )
      (setq-local inv 0)
      (read-only-mode 1)
      )
     ))

   )
  )


(defun lsp-symbol-outline-set-classes-normal ()
  "Cut refs from the txt, but letting them appear as text properties."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\..+$" nil 'noerror 1)
      ;; (kill-word -1)
      (let ((ref (match-string-no-properties 1)))
        (lsp-symbol-outline-set-classes-textprop-normal (point)
                                     (save-excursion
                                       (search-backward "#" nil t)
                                       (re-search-forward "\\." nil t)
                                       (1- (point)))
                                     )

        ))))


(defun lsp-symbol-outline-set-classes-textprop-normal  (beg end)
  (set-text-properties beg end
                       '(face font-lock-constant-face)
                       )
  )

(defun lsp-symbol-outline-set-classes-inv ()
  "Cut refs from the txt, but letting them appear as text properties."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\..+$" nil 'noerror 1)
      ;; (kill-word -1)
      (let ((ref (match-string-no-properties 1)))
        (lsp-symbol-outline-set-classes-textprop-inv (point)
                                  (save-excursion
                                    (search-backward "" nil t)
                                    (re-search-forward "\\." nil t)
                                    (1- (point)))
                                  )

        ))))


(defun lsp-symbol-outline-set-classes-textprop-inv  (beg end)
  (set-text-properties beg end
                       '(invisible t)
                       )
  )




(defun lsp-symbol-outline-set-html-info-vis ()
  "Cut refs from the txt, but letting them appear as text properties."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "#.+$" nil 'noerror 1)
      ;; (kill-word -1)
      (let ((ref (match-string-no-properties 1)))
        (lsp-symbol-outline-set-properties-vis (point)
                            (save-excursion (search-backward "#" nil t 1) (point))
                            )

        ))))


(defun lsp-symbol-outline-set-properties-vis (beg end)
  (set-text-properties beg end
                       '(face lsp-symbol-outline-arg-face)
                       ;; (propertize ,(buffer-substring-no-properties beg end) 'face 'font-lock-constant-face))
                       )
  )



(defun lsp-symbol-outline-set-html-info-inv ()
  "Cut refs from the txt, but letting them appear as text properties."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "#.+$" nil 'noerror 1)
      ;; (kill-word -1)
      (let ((ref (match-string-no-properties 1)))
        (lsp-symbol-outline-set-arg-textproperty-inv (point)
                                  (save-excursion (search-backward "#" nil t 1) (point))
                                  )

        ))))



(defun lsp-symbol-outline-sort-by-category (list )

  (--sort (< (plist-get it :kind) (plist-get other :kind)) list)

  )


(defun lsp-symbol-outline-print-fn-sorted (list)
  (let ((headingt )
        (types

         '((1 . "File")
           (2 . "Module")
           (3 . "Namespace")
           (4 . "Package")
           (5 . "Class")
           (6 . "Method")
           (7 . "Property")
           (8 . "Field")
           (9 . "Constructor"),
           (10 . "Enum")
           (11 . "Interface")
           (12 . "Function")
           (13 . "Variable")
           (14 . "Constant")
           (15 . "String")
           (16 . "Number")
           (17 . "Boolean")
           (18 . "Array"))

         )

        (contains-types (-distinct (-map (lambda (x) (plist-get x :kind)) outline-list-sorted)))

        )

    (dolist (l contains-types)
      (let ((k (-filter (lambda (i) (equal (plist-get i :kind) l)) list)))
        (cond
     ((equal (plist-get (car k) :kind) 2)
      (if window-system
          (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
        (insert (propertize " M " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
        )
      )
     ((equal (plist-get (car k) :kind) 5)  (if window-system
                                  (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize " C " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (plist-get (car k) :kind) 6)  (if window-system
                                  (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize " m " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (plist-get (car k) :kind) 12) (if window-system
                                  (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize " F " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (plist-get (car k) :kind) 13) (if window-system
                                  (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize " V " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (plist-get (car k) :kind) 14) (if window-system
                                  (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize " K " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (plist-get (car k) :kind) 18) (if window-system
                                  (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize " A " 'face 'LSP-SYMBOL-OUTLINE-TERM-SYMBOL-TYPE-NAME-FACe 'font-lock-ignore 't))
                                ))
     (t (if window-system
            (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
          (insert (propertize " * " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
          ))
         )

        (insert (if (equal (plist-get (car k) :kind) 5)
                    (propertize (format "%ses\n" (alist-get (plist-get (car k) :kind) types)) 'face 'default)
                  (propertize (format "%ss\n" (alist-get (plist-get (car k) :kind) types)) 'face 'default)
                  )
                )

        (dolist (item k)
          (insert (make-string 5 32))

          (insert-button (plist-get item :name) 'action `(lambda (x)
                                               (switch-to-buffer-other-window ,orig-buffer)
                                         (goto-line ,(plist-get item :symbol-start-line))
                                         (move-to-column ,(plist-get item :column))
                                         )

                   'keymap `(keymap (mouse-2 . push-button)  (100 . (lambda () (interactive) (lsp-symbol-outline-show-docstring-tip
                                                                                              ,(if (plist-get item :docs) (plist-get item :docs) nil))
                                                                      )))


                   'face (cond
                          ((and (plist-get item :docs) (ignore-errors (not (string-empty-p (plist-get item :docs))))
                                (equal (plist-get item :kind) 12)) 'lsp-symbol-outline-function-face-has-doc)
                          ((equal (plist-get item :kind) 12) 'lsp-symbol-outline-function-face)
                          ((and (plist-get item :docs) (ignore-errors (not (string-empty-p (plist-get item :docs))))
                                (equal (plist-get item :kind) 6)) 'lsp-symbol-outline-function-face-has-doc)
                          ((equal (plist-get item :kind) 6) 'lsp-symbol-outline-function-face)
                          ((and (plist-get item :docs) (ignore-errors (not (string-empty-p (plist-get item :docs))))
                                (equal (plist-get item :kind) 5)) 'lsp-symbol-outline-class-face-has-doc)
                          ((equal (plist-get item :kind) 5) 'lsp-symbol-outline-class-face)
                          (t 'lsp-symbol-outline-var-face)
                          )


                   )

          (if (plist-get item :args) (let ((arg-string
                            (if (equal outline-buf-mode "java-mode")

                                (ignore-errors (replace-regexp-in-string "\n" ""
                                  (replace-regexp-in-string " -> .+" ""
                                    (plist-get item :args))))

                              (car (s-match  "\(.+?\)"
                                                                   (replace-regexp-in-string "\n" ""
                                                                     (replace-regexp-in-string " -> .+" ""
                                                                              (plist-get item :args)))))
                                           )

                                       ))

                       (if arg-string
                           (progn
                             ;; (insert "\n")
                             ;; (let ((x 0)) (while (< x (* (plist-get :depth item) 4)) (progn (insert " ") (setq x (1+ x)))) )
                             (insert
                              (propertize
                               arg-string
                               'face 'lsp-symbol-outline-arg-face 'font-lock-ignore 't))))))
          (insert "\n")
          )

        (insert " \t \n")

        ))

    )
  (save-excursion
    (end-of-buffer)
    (set-mark (point))
    (backward-char 4)
    (delete-region (point) (mark))
    )

  (delete-trailing-whitespace)

  )


(defun lsp-symbol-outline-print-sorted ()
  (let ((l (nth 1 (s-match " +. \\(\\w+\\)" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))
  (setq-local outline-list-sorted (lsp-symbol-outline-sort-by-category outline-list))

  (read-only-mode 0)
  (erase-buffer)
  (lsp-symbol-outline-print-fn-sorted outline-list-sorted)

  (beginning-of-buffer)
  (forward-whitespace 2)

  (if (equal outline-buf-mode "java-mode")
      (lsp-symbol-outline-source-to-final-java)
    (lsp-symbol-outline-source-to-final)
    )
  (setq-local sorted t)
  (read-only-mode 1)
  (search-forward-regexp (format "%s\\((\\|$\\)" l) nil t)
  (beginning-of-line-text)

  )
  )


(defun lsp-symbol-outline-print-sequential ()

  (let ((lk)
        (l (nth 1
                (s-match " +. \\(\\w+\\)"
                         (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))))))
    (read-only-mode 0)
    (erase-buffer)

    (lsp-symbol-outline-print-outline outline-list orig-buffer)

    (with-current-buffer orig-buffer
      (if (equal major-mode 'js2-mode)
          (setq lk t)
        )
      )

    (if lk
        (lsp-symbol-outline-source-to-final)
      )

    (setq-local sorted nil)
    (lsp-symbol-outline-go-top)

    (read-only-mode 1)
    (search-forward-regexp (format "%s\\((\\|$\\)" l) nil t)
    (beginning-of-line-text)
    (forward-to-word 1)
    )
  )



(defun lsp-symbol-outline-toggle-sorted ()
  (interactive)
  (if sorted
      (lsp-symbol-outline-print-sequential)
    (lsp-symbol-outline-print-sorted)
    )
  )


;; (defun lsp-symbol-outline-mode-remove-python-func-args (list)
;;   (let ((indices ))
;;     (dolist (item list)
;;       (if (plist-get item :args)
;;           (let ((m (s-match "\(.+?\)"
;;                             (replace-regexp-in-string "\n" "" (plist-get item :args))
;;                             ;; (plist-get item :args)
;;                             ))
;;                 c
;;                 )
;;             (if m
;;                 (progn
;;                   (setq c (s-count-matches "," (car m)))
;;                   (push (number-sequence (plist-get item :index)
;;                                          (+ (plist-get item :index)
;;                                             (if (equal c 0) 0 c)
;;                                             )
;;                                          1
;;                                          ) indices)
;;                   )
;;               )
;;             )
;;         )
;;       )
;;     (setq indices (-flatten indices))
;;     (setf list (-remove-at-indices
;;                 indices
;;                 list
;;                 )))
;;   )


(defun my-new-func (list)
  (let ((global-counter 0) (local-counter 0) (local-end 0) (list-length (length list)))
    (if (equal major-mode 'python-mode)
        ;; (setq list (lsp-symbol-outline-mode-remove-python-func-args list))
        list
      (while (< global-counter list-length)
      ;; check if end-point (plist-get 'symbol-end-line) of current symbol greater than next symbol in list
      (if (ignore-errors (> (plist-get (nth global-counter list) :symbol-end-line) (plist-get (nth (1+ global-counter) list) :symbol-end-line)))
          ;; if it is > find the next symbol with end-point (plist-get :symbol-end-line) > than symbol at index global-counter
          (let ((local-counter (1+ global-counter)))
            (while (ignore-errors (> (plist-get (nth global-counter list) :symbol-end-line) (plist-get (nth local-counter list) :symbol-end-line)))
              (setq local-counter (1+ local-counter))
              )
            (setq local-end local-counter)
            (setq local-counter (1+ global-counter))
            (while (< local-counter local-end)
              (plist-put (nth local-counter list) :depth (1+ (plist-get (nth local-counter list) :depth)))
              (setq local-counter (1+ local-counter))
              )
            )
          )
      (setq global-counter (1+ global-counter))
      ))
    )
  list
  )


(defun lsp-symbol-outline-tree-sort-map (list)
  (let ((split 0) (start 0))
    (while (< start (length list))
     (-map-indexed (lambda (it-index it)
                     (if (if (eq (plist-get :symbol-end-line (nth (1+ it-index) (-drop start list))) nil)
                             nil
                           ;; compare end point of cell and 1+ cell
                           (< (plist-get :symbol-end-line (nth (1+ it-index) (-drop start list))) (plist-get :symbol-end-line it)))
                         (progn
                           ;; first cell that has end line > than current cell becomes split
                           (setq split
                                 (let ((index (1+ it-index)) )
                                   (while (equal split 0)
                                     (if (< (if (eq nil (plist-get :symbol-end-line (nth index list)))
                                                (1+ (plist-get :symbol-end-line (nth it-index list)))
                                              (plist-get :symbol-end-line (nth index list))
                                              )
                                            (plist-get :symbol-end-line (nth it-index list))
                                            )
                                         (setq index (1+ index))
                                       (setq split index)
                                       )
                                     )
                                   split
                                   )
                                 )
                           ;; increment depth for cells between it-index+1 and split
                           (setq list
                                 (let ((index (1+ it-index)))
                                   (while (< index split)
                                     ;; (-update-at 4 (lambda (x) (1+ x)) (nth index list))
                                     (setf (plist-get :depth (nth index list)) (1+ (plist-get :depth (nth index list))))
                                     (setq index (1+ index))
                                     )
                                   list
                                   )
                                 )
                           )
                       ))
                   (-drop start list))
     (setq start (1+ start))
     )
    )
  list
  )




(defun lsp-symbol-outline-tree-sort (list start)
  (let ((split 0) (indices ))
    ;; is next cell's end line less than current cells? yes cont; no increment start, break

    (if (equal major-mode 'python-mode)
        (progn (dolist (item list)
                 (if (plist-get item :args)
                     (let ((m (s-match "\(.+?\)"
                                       (replace-regexp-in-string "\n" "" (plist-get item :args))
                                       ;; (plist-get item :args)
                                       ))
                           c
                           )
                       (if m
                           (progn
                             (setq c (s-count-matches "," (car m)))
                             (push (number-sequence (plist-get item :index)
                                                    (+ (plist-get item :index)
                                                       (if (equal c 0) 0 c)
                                                       )
                                                    1
                                                    ) indices)
                             )
                         )
                       )
                   )
                 )
               (setq indices (-flatten indices))
               (setf list (-remove-at-indices
                           indices
                           list
                           ))

               )
      )

    ;; (if (equal (plist-get (nth start list) :name) "Board")
    ;;     (if
    ;;         't
    ;;         nil
    ;;       nil)
    ;;   nil
    ;;   )

    (if (plist-get (nth start list) :python-depth)
        (dolist (item list)
          (setf (plist-get item :depth) (plist-get item :python-depth))
          )

      (if (if (eq (plist-get (nth (1+ start) list) :symbol-end-line) nil)
              nil
            (< (plist-get (nth (1+ start) list) :symbol-end-line) (plist-get (nth start list) :symbol-end-line))
            )
          (progn
            ;; first cell that has end line > than current cell becomes split
            (setq split
                  (let ((index (1+ start)))
                    (while (equal split 0)
                      (if (< (if (eq nil (plist-get (nth index list) :symbol-end-line))
                                 (1+ (plist-get (nth start list) :symbol-end-line))
                               (plist-get (nth index list) :symbol-end-line)
                               )
                             (plist-get (nth start list) :symbol-end-line)
                             )
                          (setq index (1+ index))
                        (setq split index)
                        )
                      )
                    split
                    )
                  )
            ;; increment depth for cells between start+1 and split
            (setq list
                  (let ((index (1+ start)))
                    (while (< index split)
                      ;; (-update-at 4 (lambda (x) (1+ x)) (nth index list))
                      (setf (plist-get (nth index list) :depth) (1+ (plist-get (nth index list) :depth)))
                      (setq index (1+ index))
                      )
                    list
                    )
                  )

            ;; call lsp-symbol-outline-tree-sort with start set to start+1
            (lsp-symbol-outline-tree-sort list (1+ start))

            )
        ;; call lsp-symbol-outline-tree-sort with new start
        (if (eq (plist-get (nth (1+ start) list) :symbol-end-line) nil)
            list
          (lsp-symbol-outline-tree-sort list (1+ start))
          )
        )
      )
    list
    )
  )



(defun lsp-symbol-outline-sort-list (list)
  (--sort (< (plist-get it :symbol-start-line) (plist-get other :symbol-start-line))  list))





;; documentation and popup

;; (popup-tip "test" )



;; (defun outline-check-mode-imenu ()
;;   (interactive)
;;   ;; (profiler-start 'cpu)
;;   (cond ((or (equal major-mode 'python-mode) (equal major-mode 'js2-mode)
;;              (equal major-mode 'typescript-mode) (equal major-mode 'java-mode))
;;          (lsp-symbol-outline-create-buffer-window))
;;         ((equal major-mode 'web-mode) (outline-create-html-window))
;;         (t (imenu-list-minor-mode))
;;         )
;;   ;; (profiler-report)
;;   ;; (profiler-stop)
;;   )

;; (setq lsp-symbol-outline-mode-hook nil)

;; Keybindings

(define-key lsp-symbol-outline-mode-map (kbd "j") #'lsp-symbol-outline-next-line-my)
(define-key lsp-symbol-outline-mode-map (kbd "k") #'lsp-symbol-outline-previous-line)
(define-key lsp-symbol-outline-mode-map (kbd "<tab>") #'outline-hide-sublevels)
(define-key lsp-symbol-outline-mode-map (kbd "<backtab>") #'outline-show-all)
(define-key lsp-symbol-outline-mode-map (kbd "f") #'lsp-symbol-outline-toggle-folding)
(define-key lsp-symbol-outline-mode-map (kbd "q") #'kill-buffer-and-window)
(define-key lsp-symbol-outline-mode-map (kbd "gg") #'lsp-symbol-outline-go-top)
(define-key lsp-symbol-outline-mode-map (kbd "G") #'lsp-symbol-outline-go-to-bottom)
(define-key lsp-symbol-outline-mode-map (kbd "o") #'push-button)
(define-key lsp-symbol-outline-mode-map (kbd "i") #'lsp-symbol-outline-cycle-vis)
(define-key lsp-symbol-outline-mode-map (kbd "gh") #'lsp-symbol-outline-up-scope)
(define-key lsp-symbol-outline-mode-map (kbd "gk") #'lsp-symbol-outline-up-sibling)
(define-key lsp-symbol-outline-mode-map (kbd "gj") #'lsp-symbol-outline-down-sibling)
(define-key lsp-symbol-outline-mode-map (kbd "w") #'lsp-symbol-outline-widen-to-widest-column)
(define-key lsp-symbol-outline-mode-map (kbd "s") #'lsp-symbol-outline-toggle-sorted)
(define-key lsp-symbol-outline-mode-map (kbd "l") #'lsp-symbol-outline-peek)
(define-key lsp-symbol-outline-mode-map (kbd "d") #'lsp-symbol-outline-show-docstring-tip)

(set-face-attribute 'lsp-symbol-outline-button-face nil :foreground "#93a0b2")

(provide 'lsp-symbol-outline)

;;; lsp-symbol-outline.el ends here
