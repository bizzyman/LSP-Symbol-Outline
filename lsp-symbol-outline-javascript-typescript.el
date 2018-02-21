;;; lsp-symbol-outline-javascript-typescript.el --- a symbol tree for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 bizzyman

;; Version: 0.0.3
;; Homepage: https://github.com/bizzyman/LSP-Symbol-Outline
;; Keywords: languages, lsp, outline, javascript

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
;;; Javascript lang specific code.

;;; Code:

;; Dependencies

(require 'lsp-mode)
(require 'lsp-javascript-typescript)
(require 'lsp-symbol-outline)
(require 'url)
(require 'tern)

;; Defuns

(defun lsp-symbol-outline-tern-start-server (c)
       "Start LSP symbol outline managed tern process for querying argument info."
       (let* ((default-directory (tern-project-dir))
              (cmd (append tern-command
                           `(,(format "%s" (tern-project-dir))
                             "--strip-crs" "--persistent")))
              (proc (apply #'start-process "LSP-S-O-tern" nil cmd))
              (all-output ""))
         (set-process-query-on-exit-flag proc nil)
         (set-process-sentinel proc
                               (lambda (_proc _event)
                                 (delete-process proc)
                                 (setf lsp-s-o-tern-known-port
                                       (cons :failed
                                             (concat "Could not start Tern server\n"
                                                     all-output)))
                                 (run-at-time "30 sec" nil
                                              (lambda (buf)
                                                (with-current-buffer buf
                                                  (when (consp
                                                         (lsp-s-o-tern-known-port))
                                                    (setf lsp-s-o-tern-known-port
                                                          nil))))
                                              (current-buffer))))
         (set-process-filter proc
                             (lambda (proc output)
                               (if (not (string-match
                                         "Listening on port \\([0-9][0-9]*\\)"
                                         output))
                                   (setf all-output (concat all-output output))
                                 (setf lsp-s-o-tern-known-port
                                       (string-to-number (match-string 1 output)))
                                 (set-process-sentinel proc
                                                       (lambda (proc _event)
                                                         (delete-process proc)
                                                         (setf
                                                          lsp-s-o-tern-known-port
                                                          nil)))
                                 (set-process-filter proc nil))))))

(defun lsp-symbol-outlline--kill-tern-fn ()
       "Kill the LSP symbol outline managed tern process."
       (kill-process (get-process "LSP-S-O-tern")))


(defun lsp-symbol-outline--tern-request-sync (point-pos)
       ;; FIXME Peformance, maybe make parallel through deferred
       ;; ??? multithreading not possible?
       ;; FIXME search fails "(" intermittently
       ;; FIXME Fails after tern idle for some time
       "Send a request to the ternjs server using `url-retrieve-synchronously'
returning the function args and their types for func at POINT-POS."
       (let* ((url-mime-charset-string nil)
              (url-request-method "POST")
              (deactivate-mark nil)
              (url-request-data
               (format
                "{\"query\":{\"end\":%s,\"file\":\"%s\",\"type\":\"type\",
                  \"preferFunction\":true}}"
                point-pos
                (s-chop-prefix (tern-project-dir) (buffer-file-name))))
              (url-show-status nil)
              (url (url-parse-make-urlobj "http" nil nil
                                          "127.0.0.1"
                                          lsp-s-o-tern-known-port
                                          "/" nil nil nil))
              (url-current-object url))
         (condition-case err
             (with-current-buffer (url-retrieve-synchronously url)
               (goto-char (point-min))
               (buffer-substring-no-properties
                (progn (search-forward "(")
                       (forward-char -1)
                       (point))
                (re-search-forward "[^\\])" nil t)))
           ('error
            (progn
              (message "restarting tern server ... please wait ...")
              ;; (ignore-errors (kill-process (get-process "LSP-S-O-tern")))
              ;; (sit-for 0.1)
              ;; (lsp-symbol-outline-tern-start-server (lambda (on e) nil))
              (sit-for 1.5)
              (lsp-symbol-outline--tern-request-sync point-pos))))))

(defun lsp-symbol-outline--tern-update-args ()
  (let ((opening-paren (cadr (syntax-ppss))))
    (tern-run-query (lambda (data)
                      (let ((type (tern-parse-function-type data)))
                        (when type
                          (setf tern-last-argument-hints (cons opening-paren type))
                          )))
                    `((type . "type")
                      (preferFunction . t))
                    opening-paren
                    :silent)))

(defun lsp-symbol-outline--get-symbol-args-js (plist-item hasht-range)
       "Get the arguments for symbol by moving to symbol definition in buffer and
making a tern request there."
       (goto-char (plist-get plist-item :symbol-start-point))
       (if
        (search-forward "("
                        (plist-get plist-item :symbol-end-point)
                        t)
           (lsp-symbol-outline--tern-request-sync
            (- (point) 2))
         nil))

(defun lsp-symbol-outline--get-symbol-docs-js (plist-item)
       "Get the docstring for symbol by parsing the JSDoc block above symbol
seeing as this function is invoked inside a `save-excursion', so no need to move
to symbol definition twice."
       (goto-char (plist-get plist-item :symbol-start-point))
       (vertical-motion -1)
       (if (and (search-forward "*/" (line-end-position) t)
                (forward-comment -1)
                (looking-at "\\/\\*\\*"))
           (progn
             (forward-char 3)
             (s-collapse-whitespace
              (s-chop-prefix "\n"
                             (s-replace-regexp
                              "/\\*\\*" ""
                              (s-replace-regexp
                               " +\\* " ""
                               (buffer-substring-no-properties
                                (point)
                                (progn
                                  (re-search-forward "[.?!/]")
                                  (point))))))))
         nil))

(defun lsp-symbol-outline--print-outline-js (list buf)
      "Insert indentation, icon, button and any args into symbol outline buffer.
Iterates over symbol list. Javascript specific argument printing."
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
               (plist-get item :args)))
          (if arg-string
              (progn
                (insert (propertize arg-string
                                    'face 'lsp-symbol-outline-arg-face
                                    'font-lock-ignore 't))))))
    (insert "\n")))

(defun lsp-symbol-outline--print-outline-sorted-js (list-sorted)
       "Print a symbol outline grouped by symbol kind. Takes list of symbol
plists LIST-SORTED and prints the symbol icon, the kind name, symbol button and
any argument information.

LIST-SORTED is filtered to yield only distinct :kind values. LIST-SORTED is then
iterated over and filtered by each distinct :kind value. Symbols of that kind
are printed with no regard to indentation or hierarchy.
JS specific."
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
                          (car (s-match
                                "\(.+\)\)?"
                                (s-collapse-whitespace
                                 (replace-regexp-in-string
                                  "\n" ""
                                  (replace-regexp-in-string
                                   " -> .+" ""
                                   (plist-get item :args))))))))
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

;;;###autoload
(defun lsp-symbol-outline-make-outline-js ()
       "Call `lsp-symbol-outline-create-buffer-window' with js specific
functions. Ensure tern server is running in correct project dir. Creates LSP sym
ouline buffer."
       (interactive)
       (if (not (equal (tern-project-dir)
                       (ignore-errors
                         (nth 1 (process-command
                                 (get-process "LSP-S-O-tern"))))))
           (progn
             (ignore-errors (kill-process (get-process "LSP-S-O-tern")))
             (sit-for 0.1)
             (lsp-symbol-outline-tern-start-server (lambda (on e) nil))
             (sit-for 1)
             (add-hook 'kill-buffer-hook
                       #'lsp-symbol-outlline--kill-tern-fn
                       nil t)))
       (lsp-symbol-outline-create-buffer-window
        #'lsp-symbol-outline--get-symbol-end-point
        #'lsp-symbol-outline--set-placeholder-depth
        #'lsp-symbol-outline--get-symbol-args-js
        #'lsp-symbol-outline--get-symbol-docs-js
        #'lsp-symbol-outline--tree-sort
        #'lsp-symbol-outline--print-outline-js
        #'lsp-symbol-outline--print-outline-sorted-js
        #'lsp-symbol-outline--finalize-arg-props-colon-generic
        #'lsp-symbol-outline--cycle-arg-visibility-colon-generic))

(provide 'lsp-symbol-outline-javascript-typescript)

;;; lsp-symbol-outline-javascript-typescript.el ends here
