;;; LSP SYMBOL OUTLINE

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


;; vars

(defcustom lsp-symbol-outline-window-position
  'right
  "LSP symbol outline window position."
  :group 'lsp-symbol-outline)

(defcustom lsp-symbol-outline-modeline-format
  '((:propertize "%b" face mode-line-buffer-id) " ")
  "Local modeline format for the LSP symbol outline mode."
  :group 'lsp-symbol-outline)

;; faces

(face-spec-set
 'lsp-symbol-outline-button-face
 '((t :foreground "#93a0b2"
      ))
 'face-defface-spec
 )

(defface lsp-symbol-outline-atom-icons-face
  '((t
     :foreground "#a9afba"
     :family "atomicons"
     :height 1.0
     ))
  "face for atom-outline icons"
  )

(defface lsp-symbol-outline-term-symbol-type-name-face
  '((t (:foreground "white")
       ))
  "face for outline symbol types node"
  )

(defface lsp-symbol-outline-class-face-has-doc
  '((t (:inherit (font-lock-type-face)
                 :underline t
                 )
       ))
  "face for outline class nodes"
  )


(defface lsp-symbol-outline-class-face
  '((t (:inherit (font-lock-type-face))
       ))
  "face for outline class nodes"
  )

(defface lsp-symbol-outline-function-face-has-doc
  '((t (:inherit (font-lock-function-name-face)
                 :underline t
                 )
       ))
  "face for outline function nodes"
  )


(defface lsp-symbol-outline-function-face
  '((t (:inherit (font-lock-function-name-face))
       ))
  "face for outline function nodes"
  )

(defface lsp-symbol-outline-var-face
  '((t (:inherit (font-lock-variable-name-face))
       ))
  "face for outline variable nodes"
  )

(defface lsp-symbol-outline-arg-face
  '((t (:inherit (font-lock-doc-face))
       ))
  "face for outline node arguments"
  )

(defface lsp-symbol-outline-html-tag-props-face
  '((t (:foreground "#75B5AA")
       ))
  "face for outline html props"
  )

;; (count-lines 1 (point))


;; Defuns


(defun lsp-symbol-outline-toggle-off-fl ()

  ;; (setq my-mode-font-lock-keywords
  ;;       (list
  ;;        '("#...." 0
  ;;          (progn (remove-text-properties (match-beginning 0)
  ;;                                      (match-end 0)
  ;;                                      'invisible)
  ;;                 'bold))))
  (read-only-mode 0)

  (remove-text-properties (point-min)
                          (point-max)
                          '(invisible t))

  ;; (font-lock-fontify-buffer)

  (read-only-mode 1)

  )
(remove-text-properties (point-min)
                        (point-max)
                        '(invisible t))

(defun lsp-symbol-outline-toggle-on-fl ()

  (setq my-mode-font-lock-keywords
        (list
         '("#\\w+" 0
           (progn (add-text-properties (match-beginning 0)
                                       (match-end 0)
                                       '(invisible t))
                  'bold))))

  (font-lock-fontify-buffer)

  )


(defun lsp-symbol-outline-widen-to-widest-column ()
  (interactive)
  (setq window-size-fixed nil)
  (enlarge-window
   (- (lsp-symbol-outline-find-longest-line)
 (window-width (selected-window)))
   t)

  (setq window-size-fixed 'width)
  )


(defun lsp-symbol-outline-find-longest-line ()
  (save-excursion
    (goto-longest-line (point-min) (point-max))
    (end-of-line)
    (ceiling (* 1.15 (1+ (current-column))))
    )
  )


(defun lsp-symbol-outline-create-buffer ()
  (get-buffer-create (format "*%s-outline*" (file-name-sans-extension (buffer-name))))
  )


(defun lsp-symbol-outline-up-scope ()
  (interactive)
  (outline-up-heading 1 nil)
  (forward-whitespace 2)
  )


(defun lsp-symbol-outline-up-sibling ()
  (interactive)
  (let ((indent (current-indentation)))
    (lsp-symbol-outline-previous-line)
    (while (and (not (equal (current-indentation) indent) ) (not (eobp)))
      (lsp-symbol-outline-previous-line)
      )
    )
  )


(defun lsp-symbol-outline-down-sibling ()
  (interactive)
  (let ((indent (current-indentation)))
    (outline-next-line)
    (while (and (not (equal (current-indentation) indent) ) (not (eobp)))
      (outline-next-line)
      )
    (forward-whitespace 2)
    )
  )


(defun lsp-symbol-outline-forward-sexp ()
  (interactive)
  (let ((indent (current-indentation)))
    (forward-line 1)
    (while (and (> (current-indentation) indent) (not (eobp)))
      (forward-line 1)
      (end-of-line)
      )
    (forward-line -1)
    (end-of-line)

    )
  )


(defun lsp-symbol-outline-previous-line ()
  (interactive)
  ;; (ignore-errors (previous-line))
  (vertical-motion -1)
  ;; (forward-line -1)
  ;; (beginning-of-line)
  (while (looking-at "$")
    ;; (ignore-errors (previous-line))
    (vertical-motion -1)
    )
  ;; (beginning-of-line)
  ;; (forward-whitespace 2)

  (if (not (looking-at-p " *[^ ] "))
      (forward-whitespace 1)
    (forward-whitespace 2)
    )
  )


(defun lsp-symbol-outline-next-line-my ()
  (interactive)
  ;; (forward-line 1)
  ;; (evil-next-visual-line)
  ;; (next-line)
  (vertical-motion 1)

  ;; (beginning-of-line)

  (if (not (looking-at-p " *[^ ] "))
      (forward-whitespace 1)
    (forward-to-word 2)
    )
  )


;; (looking-at-p " *[^ ] ")

(defun lsp-symbol-outline-overlay-at-point-p ()
  (ov-in 'invisible t (point) (save-excursion (forward-line 1) (point)))
  )

(define-derived-mode lsp-symbol-outline-mode special-mode "outline"
  "my outline mode"
  (read-only-mode 1)
  (face-remap-add-relative 'default 'lsp-symbol-outline-button-face)
  (if (featurep 'evil)
   (evil-normalize-keymaps))
  ;; (set-face-attribute 'default nil :foreground "white")
  ;; (setq-local truncate-lines 1)
  )

;; (add-hook 'lsp-symbol-outline-mode-hook '(lambda () (toggle-truncate-lines 1) (setq truncate-lines 1) (spacemacs/toggle-truncate-lines-on) (spacemacs/toggle-visual-line-navigation-off)))

(defun lsp-symbol-outline-toggle-folding ()
  (interactive)
  (outline-cycle)
  (forward-whitespace 2)
  )

(defun lsp-symbol-outline-go-top ()
  (interactive)
  (beginning-of-buffer)
  (forward-whitespace 2)
  )

(defun lsp-symbol-outline-go-to-bottom ()
  (interactive)
  (end-of-buffer )
  (vertical-motion -1)
  (forward-whitespace 2)
  )


(defun lsp-symbol-outline-peek ()
  (interactive)
  (let (;; (w (window-numbering-get-number))
        (w (selected-window))
        )
    (push-button)
    (select-window w)
    ;; (select-window-by-number w)
    )
  )


(defun lsp-symbol-outline-create-buffer-window ()
  (interactive)
  (if (not lsp-mode)
      (lsp-mode)
    )
  (let ((current-line (string-to-number (format-mode-line "%l")))

        (outline-list

         ;; Caching

         ;; (if (and (boundp 'buffer-hash-value) (equal buffer-hash-value
         ;;                                             (md5 (buffer-substring-no-properties (point-min) (point-max)))))
         ;;     buffer-orig-outline-list
         ;;   (lsp-symbol-outline-tree-sort (lsp-symbol-outline-sort-list (lsp-symbol-outline-get-symbols-list)) 0))

         (lsp-symbol-outline-tree-sort (lsp-symbol-outline-sort-list (lsp-symbol-outline-get-symbols-list)) 0)
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

    (setq-local buffer-orig-outline-list outline-list)
    (setq-local buffer-hash-value (md5 (buffer-substring-no-properties (point-min) (point-max))))

    (when window
      (window--display-buffer outline-buffer window 'window nil t)
      window)

    (pop-to-buffer outline-buffer)
    (erase-buffer)

    (setq-local outline-buf-mode (symbol-name mod))
    (lsp-symbol-outline-print-outline outline-list buf)

    (lsp-symbol-outline-source-to-final)

    ;; (imenu-list--set-mode-line)
    ;; (insert " \t \n")

    (lsp-symbol-outline-mode)

    (if (featurep 'evil-snipe)
     (evil-snipe-mode 0))

    (setq-local mode-line-format nil)

    (setq-local inv 0)
    (setq-local outline-buf-mode (symbol-name mod))
    (setq-local outline-list outline-list)
    (setq-local orig-buffer buf)
    (setq-local sorted nil)

    (outline-minor-mode 1)

    ;; (set-fringe-style '(0 . 0))

    (if (boundp 'evil-evilified-state-local-map)
        (define-key evil-evilified-state-local-map (kbd "d") #'lsp-symbol-outline-show-docstring-tip)
      )

    ;; (evil-local-mode 0)
    ;; (evil-mode 1)

    (if (featurep 'evil)
        (evil-make-overriding-map lsp-symbol-outline-mode-map 'normal))

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
  (1+ (cond ((progn (-elem-index (car (last (-filter (lambda (x) (< (nth 2 x) current-line)) list))) list))) (t 0)))
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



(defun lsp-symbol-outline-jump-paren ()
    "Go to the matching paren"
    (cond ((eq 4 (car (syntax-after (point))))
           (forward-sexp)
           (forward-char -1))
          ((eq 5 (car (syntax-after (point))))
           (forward-char 1)
           (backward-sexp))
          )
  )




(defun lsp-symbol-outline-get-symbols-list ()
  ;; get name, kind, line and character info
  (let ((agg-items ) (index 1))
    (dolist (item (lsp-symbol-outline-lsp-get-document-symbols))
      ;; (message "%s" (princ item))
      (let ((ind-item ) )

        ;; 0 - NAME
        (push (replace-regexp-in-string "\(.+\)" "" (gethash "name" item)) ind-item )
        ;; 1 - KIND
        (push (gethash "kind" item) ind-item)

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

                ;; (if (equal (nth 0 (reverse ind-item)) "Board")
                ;;     (if
                ;;         't
                ;;         nil
                ;;       nil)
                ;;   nil
                ;;   )

                ;; 2 - java func start range
                (push (line-number-at-pos) ind-item)
                (lsp-symbol-outline-jump-paren)
                ;; 3 - java func end range
                (push (line-number-at-pos) ind-item)

                )
              )

            (progn
              ;; 2 - var start range
              (push (1+ (gethash "line" (gethash "start" (gethash "range" (gethash "location" item))))) ind-item)
              ;; 3 - var end range
              (push (1+ (gethash "line" (gethash "end" (gethash "range" (gethash "location" item))))) ind-item)
             )
          )

        ;; 4 - depth placeholder?
        (push 0 ind-item)
        ;; 5 - COLUMN
        (push (gethash "character" (gethash "start" (gethash "range" (gethash "location" item)))) ind-item)


        ;;debug



        ;; get arguments and docstring

        (if (or (equal 6 (nth 4 ind-item)) ;; (equal 5 (nth 4 ind-item))
                (equal 12 (nth 4 ind-item))
                (and (equal major-mode 'python-mode)
                     (equal 5 (nth 4 ind-item)))
                )

            (push (cond

                   (
                    (equal major-mode 'js2-mode)
                    (let ((lk ))
                      (request
                       (format "http://localhost:%s" tern-known-port)
                       :type "POST"
                       :parser 'json-read
                       :success (function* (lambda (&key data &allow-other-keys) (setq lk (cdr (car data)))))
                       :data
                       (format "{\"query\":{\"end\":%s,\"file\":\"%s\",\"type\":\"type\",\"preferFunction\":true}}"
                               (save-excursion
                                 (goto-line (nth 3 ind-item))
                                 (move-to-column (nth 0 ind-item))

                                 (search-forward "(")
                                 (backward-char)

                                 ;; 6 - js2 docstring
                                 (let ((docs
                                        (cddr (gethash "contents"
                                                      (lsp--send-request
                                                       (lsp--make-request "textDocument/hover"
                                                                          (lsp--text-document-position-params)))
                                                      ))))
                                                         (if docs
                                                          (push
                                                           (car docs)
                                                           ind-item
                                                           )
                                                          (push nil ind-item)
                                                          )
                                                         )
                                 (1- (point))
                                 )

                               (buffer-file-name)
                               )
                       :sync t
                       )

                      ;; (replace-regexp-in-string ": [^ )\\|]+" "" lk )
                      lk
                      )
                    ;; 7 - js2 args
                    )


                 (
                    (equal major-mode 'java-mode)
                    (ignore-errors (gethash "value"
                              (car (gethash "contents"
                                 (lsp--send-request (lsp--make-request
                                  "textDocument/hover"
                                   `(:textDocument (:uri ,(gethash "uri" (gethash "location" item) item))
                                     :position (:line
                                      ,(1- (nth 3 ind-item))
                                       :character ,(save-excursion
                                         (goto-line (nth 3 ind-item))
                                           (move-to-column
                                             (gethash "character"
                                              (gethash "end" (gethash "range" (gethash "location" item)))))
                                                (let ((docs
                                                  (cdr (gethash "contents"
                                                   (lsp--send-request (lsp--make-request
                                                     "textDocument/hover"
                                                      `(:textDocument (:uri ,(gethash "uri" (gethash "location" item) item))
                                                         :position
                                                          (:line
                                                            ,(1- (nth 3 ind-item))
                                                              :character ,(nth 0 ind-item)))
                                                               ))))
                                                                ))
                                                                 (if docs
                                                                  ;; 6 java docstring
                                                                    (push
                                                                      (car docs)
                                                                       ind-item
                                                                        )
                                                                          (push nil ind-item)
                                                                            )
                                                                             )

                                                                             ;; (while
                                                                             ;;     (progn
                                                                             ;;       (search-forward ")")
                                                                             ;;       (in-string-p)))
                                                                               (search-forward ")")
                                                                                (backward-char 2)
                                                                                 (string-to-number
                                                                                  (format-mode-line "%c")))
                                                                                           )))))
                                   )))
                    ;; 7 java args
                    )

                 (
                  (equal major-mode 'python-mode)

                  (progn
                    (let ((contts
                           (ignore-errors (gethash "contents"
                                     (lsp--send-request (lsp--make-request
                                                         "textDocument/hover"
                                                         `(:textDocument (:uri ,(gethash "uri" (gethash "location" item) item))
                                                                         :position
                                                                         (:line
                                                                          ,(1- (nth 3 ind-item))
                                                                          :character ,(nth 0 ind-item)))
                                                         ))))))

                      ;; 6 python docstring
                      (if contts
                          (if (equal 5 (nth 4 ind-item))
                              (push contts ind-item)
                            (progn
                              (push (ignore-errors (string-join
                                      (-remove (lambda (i) (or (not (stringp i)) (string-empty-p i)))
                                               (-drop 2
                                                      (s-split "\n"
                                                               contts
                                                               )))
                                      "\n"))
                                    ind-item
                                    )
                              )
                            )
                        (push nil ind-item)
                        )

                      ;; 7 python args
                      (if (not (equal 5 (nth 5 ind-item)))

                          contts
                        nil

                          )
                      )
                    )

                    )

                   )
                  ind-item)

          (progn

            ;; 6 nil docs for vars
            (push nil ind-item)

            ;; 7 nil args for vars
            (push nil ind-item)
            )

          )

        ;; 8 DEPTH?
        (if (gethash "depth" item) (push (gethash "depth" item) ind-item)  (push nil ind-item))
        ;; 9 INDEX
        (push index ind-item)
        (setq index (1+ index))
        (push (reverse ind-item) agg-items)
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
    ;;      (looking-at (format " %s" (make-string (* 4 (truncate (nth 4 item))) 32)) )
    ;;      )
    ;;    (save-excursion

    ;;      (search-forward "(" (line-end-position) t 1)
    ;;      (search-forward (format "%s" (nth 0 item)) (line-end-position) t 1)
    ;;      )
    ;;    (or (equal (nth 1 item) 13) (equal (nth 1 item) 14))
    ;;    )
    ;; (delete item list)
    (insert " ")
    (let ((x 0)) (while (< x (* (nth 4 item) 2)) (progn (insert " ") (setq x (1+ x)))) )
    (cond
     ((equal (nth 1 item) 2)
      (if window-system
          (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
        (insert (propertize "M " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
        )
      )
     ((equal (nth 1 item) 5)  (if window-system
                                  (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize "C " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (nth 1 item) 6)  (if window-system
                                  (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize "m " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (nth 1 item) 12) (if window-system
                                  (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize "F " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (nth 1 item) 13) (if window-system
                                  (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize "V " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (nth 1 item) 14) (if window-system
                                  (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize "K " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (nth 1 item) 18) (if window-system
                                  (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize "A " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     (t (if window-system
            (insert (propertize " " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
          (insert (propertize "* " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
          ))
     )
    (insert-button (car item) 'action `(lambda (x)
                                         (switch-to-buffer-other-window ,buf)
                                         (goto-line ,(nth 2 item))
                                         (move-to-column ,(nth 5 item))
                                         )

                   'keymap `(keymap (mouse-2 . push-button)  (100 . (lambda () (interactive) (lsp-symbol-outline-show-docstring-tip
                                                                                              ,(if (nth 6 item) (nth 6 item) nil))
                                                                      )))


                   'face (cond
                          ((and (nth 6 item) (ignore-errors (not (string-empty-p (nth 6 item))))
                                (equal (nth 1 item) 12)) 'lsp-symbol-outline-function-face-has-doc)
                          ((equal (nth 1 item) 12) 'lsp-symbol-outline-function-face)
                          ((and (nth 6 item) (ignore-errors (not (string-empty-p (nth 6 item))))
                                (equal (nth 1 item) 6)) 'lsp-symbol-outline-function-face-has-doc)
                          ((equal (nth 1 item) 6) 'lsp-symbol-outline-function-face)
                          ((and (nth 6 item) (ignore-errors (not (string-empty-p (nth 6 item))))
                                (equal (nth 1 item) 5)) 'lsp-symbol-outline-class-face-has-doc)
                          ((equal (nth 1 item) 5) 'lsp-symbol-outline-class-face)
                          (t 'lsp-symbol-outline-var-face)
                          )


                   )

    ;; "int num - Test.main(...).Foo.bar(int, int)"

    (if (nth 7 item) (let ((arg-string
                            (if (equal outline-buf-mode "java-mode")

                                (ignore-errors (car (s-match "\(.+\)" (car (s-match (concat (car item)  "\(.+?\)")
                                                                   (replace-regexp-in-string "\n" ""
                                                                     (replace-regexp-in-string " -> .+" ""
                                                                           (nth 7 item))))))))

                              (car (s-match  "\(.+?\)\)?"
                                             (s-collapse-whitespace
                                              (replace-regexp-in-string "\n" ""
                                                                        (replace-regexp-in-string " -> .+" ""
                                                                                                  (nth 7 item))))))
                                           )

                                       ))

                       (if arg-string
                           (progn
                             ;; (insert "\n")
                             ;; (let ((x 0)) (while (< x (* (nth 4 item) 4)) (progn (insert " ") (setq x (1+ x)))) )
                             (insert
                              (propertize
                               arg-string
                               'face 'lsp-symbol-outline-arg-face 'font-lock-ignore 't))))))
    (insert "\n")
    )
  )







;;;; TOGGLE VISIBILITY

;; non-html

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
                                                 (goto-char (plist-get (sp-get-sexp) ':end))
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
                       '(face 'lsp-symbol-outline-var-face)
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
                                    (goto-char (plist-get (sp-get-sexp) ':end))
                                    )
                                   ((looking-at " {")
                                    (search-forward "{")
                                    (backward-char 1)
                                    (lsp-symbol-outline-jump-paren)

                                    (if
                                                     (looking-at ".|")
                                                     (progn
                                                       (forward-char)
                                                       (cond ((search-forward "}" (line-end-position) t )
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
   ((equal outline-buf-mode "js2-mode")
    (cond
     ((equal inv 0)
      (read-only-mode 0)
      (lsp-symbol-outline-set-arg-types-inv)
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
        (lsp-symbol-outline-source-to-final)
        )
      (setq-local inv 0)
      (read-only-mode 1)
      )
     ))

   ((or (equal outline-buf-mode "python-mode") (equal outline-buf-mode "java-mode"))
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

  (--sort (< (nth 1 it) (nth 1 other)) list)

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

        (contains-types (-distinct (-map (lambda (x) (nth 1 x)) outline-list-sorted)))

        )

    (dolist (l contains-types)
      (let ((k (-filter (lambda (i) (equal (nth 1 i) l)) list)))
        (cond
     ((equal (nth 1 (car k)) 2)
      (if window-system
          (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
        (insert (propertize " M " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
        )
      )
     ((equal (nth 1 (car k)) 5)  (if window-system
                                  (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize " C " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (nth 1 (car k)) 6)  (if window-system
                                  (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize " m " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (nth 1 (car k)) 12) (if window-system
                                  (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize " F " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (nth 1 (car k)) 13) (if window-system
                                  (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize " V " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (nth 1 (car k)) 14) (if window-system
                                  (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize " K " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     ((equal (nth 1 (car k)) 18) (if window-system
                                  (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
                                (insert (propertize " A " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
                                ))
     (t (if window-system
            (insert (propertize "  " 'face 'lsp-symbol-outline-atom-icons-face 'font-lock-ignore 't))
          (insert (propertize " * " 'face 'lsp-symbol-outline-term-symbol-type-name-face 'font-lock-ignore 't))
          ))
         )

        (insert (if (equal (alist-get (nth 1 (nth 0 k)) types) "Class")
                    (propertize (format "%ses\n" (alist-get (nth 1 (nth 0 k)) types)) 'face 'default)
                  (propertize (format "%ss\n" (alist-get (nth 1 (nth 0 k)) types)) 'face 'default)
                  )
                )

        (dolist (item k)
          (insert (make-string 5 32))

          (insert-button (car item) 'action `(lambda (x)
                                               (switch-to-buffer-other-window ,orig-buffer)
                                         (goto-line ,(nth 2 item))
                                         (move-to-column ,(nth 5 item))
                                         )

                   'keymap `(keymap (mouse-2 . push-button)  (100 . (lambda () (interactive) (lsp-symbol-outline-show-docstring-tip
                                                                                              ,(if (nth 6 item) (nth 6 item) nil))
                                                                      )))


                   'face (cond
                          ((and (nth 6 item) (ignore-errors (not (string-empty-p (nth 6 item))))
                                (equal (nth 1 item) 12)) 'lsp-symbol-outline-function-face-has-doc)
                          ((equal (nth 1 item) 12) 'lsp-symbol-outline-function-face)
                          ((and (nth 6 item) (ignore-errors (not (string-empty-p (nth 6 item))))
                                (equal (nth 1 item) 6)) 'lsp-symbol-outline-function-face-has-doc)
                          ((equal (nth 1 item) 6) 'lsp-symbol-outline-function-face)
                          ((and (nth 6 item) (ignore-errors (not (string-empty-p (nth 6 item))))
                                (equal (nth 1 item) 5)) 'lsp-symbol-outline-class-face-has-doc)
                          ((equal (nth 1 item) 5) 'lsp-symbol-outline-class-face)
                          (t 'lsp-symbol-outline-var-face)
                          )


                   )

          (if (nth 7 item) (let ((arg-string
                            (if (equal outline-buf-mode "java-mode")

                                (ignore-errors (car (s-match "\(.+\)" (car (s-match (concat (car item)  "\(.+?\)")
                                                                   (replace-regexp-in-string "\n" ""
                                                                     (replace-regexp-in-string " -> .+" ""
                                                                           (nth 7 item))))))))

                              (car (s-match  "\(.+?\)"
                                                                   (replace-regexp-in-string "\n" ""
                                                                     (replace-regexp-in-string " -> .+" ""
                                                                              (nth 7 item)))))
                                           )

                                       ))

                       (if arg-string
                           (progn
                             ;; (insert "\n")
                             ;; (let ((x 0)) (while (< x (* (nth 4 item) 4)) (progn (insert " ") (setq x (1+ x)))) )
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

  (setq-local outline-list-sorted (lsp-symbol-outline-sort-by-category outline-list))
  (read-only-mode 0)
  (erase-buffer)
  (lsp-symbol-outline-print-fn-sorted outline-list-sorted)

  (beginning-of-buffer)
  (forward-whitespace 2)

  (lsp-symbol-outline-source-to-final)
  (setq-local sorted t)
  (read-only-mode 1)

  )


(defun lsp-symbol-outline-print-sequential ()

  (let ((lk))
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
    )
  )



(defun lsp-symbol-outline-toggle-sorted ()
  (interactive)
  (if sorted
      (lsp-symbol-outline-print-sequential)
    (lsp-symbol-outline-print-sorted)
    )
  )






(defun lsp-symbol-outline-tree-sort (list start)
  (let ((split 0) (indices ))
    ;; is next cell's end line less than current cells? yes cont; no increment start, break

    (if (equal major-mode 'python-mode)
        (progn (dolist (item list)
                 (if (nth 7 item)
                     (let ((m (s-match "\(.+?\)"
                                       (replace-regexp-in-string "\n" "" (nth 7 item))
                                       ;; (nth 7 item)
                                       ))
                           c
                           )
                       (if m
                           (progn
                             (setq c (s-count-matches "," (car m)))
                             (push (number-sequence (nth 9 item)
                                                    (+ (nth 9 item)
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


    ;; (if (equal (nth 0 (nth start list)) "Board")
    ;;     (if
    ;;         't
    ;;         nil
    ;;       nil)
    ;;   nil
    ;;   )

    (if (nth 8 (nth start list))
        (dolist (item list)
          (setf (nth 4 item) (nth 8 item))
          )

      (if (if (eq (nth 3 (nth (1+ start) list)) nil)
              nil
            (< (nth 3 (nth (1+ start) list)) (nth 3 (nth start list)))
            )
          (progn
            ;; first cell that has end line > than current cell becomes split
            (setq split
                  (let ((index (1+ start)))
                    (while (equal split 0)
                      (if (< (if (eq nil (nth 3 (nth index list)))
                                 (1+ (nth 3 (nth start list)))
                               (nth 3 (nth index list))
                               )
                             (nth 3 (nth start list))
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
                      (setf (nth 4 (nth index list)) (1+ (nth 4 (nth index list))))
                      (setq index (1+ index))
                      )
                    list
                    )
                  )

            ;; call lsp-symbol-outline-tree-sort with start set to start+1
            (lsp-symbol-outline-tree-sort list (1+ start))

            )
        ;; call lsp-symbol-outline-tree-sort with new start
        (if (eq (nth 3 (nth (1+ start) list)) nil)
            list
          (lsp-symbol-outline-tree-sort list (1+ start))
          )
        )
      )
    list
    )
  )



(defun lsp-symbol-outline-sort-list (list)
  (--sort (< (nth 2 it) (nth 2 other))  list))





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
