;; Doc on SMIE: "SMIE: Weakness is Power! Auto-indentation with incomplete information" <http://www.iro.umontreal.ca/~monnier/smie.pdf>
;;
;; Also see the "Simple Minded Indentation Engine" page in the Emacs Lisp manual.

(require 'smie)

;;;###autoload (put 'smithy-indent-basic 'safe-local-variable 'integerp)
(defcustom smithy-indent-basic 2
  "The standard indentation change for Smithy document nested forms."
  :group 'smithy
  :type 'integer
  :safe 'integerp)

(defconst smithy-mode-font-lock-keywords
  (let* ((identifier "\\(?:[[:alpha:]_][[:alnum:]_]*\\)")
         (namespace (format "\\(?:%s\\(?:\\.%s\\)*\\)" identifier identifier))
         (relative-shape-id (format "\\(?:%s\\(?:\\$%s\\)?\\)" identifier identifier))
         (absolute-shape-id (format "\\(?:%s#%s\\)" namespace relative-shape-id))
         (shape-id (format "\\(?:%s\\|%s\\)" absolute-shape-id relative-shape-id)))
    (list (concat "^" (regexp-opt '("metadata" "namespace" "apply" "trait"
                                    ;; shape statements
                                    "service" "resource" "operation" "structure" "union" "list" "set" "map"
                                    ;; simple shapes
                                    "blob" "boolean" "string" "byte" "short" "integer" "long"
                                    "float" "double" "bigInteger" "bigDecimal" "timestamp")
                                  'symbols))
          (cons "\\$version" font-lock-preprocessor-face)
          (cons (concat "@" shape-id) font-lock-preprocessor-face)))
  "Font lock keywords for Smithy mode.")

(defvar smithy-mode-abbrev-table nil "Abbrev table for Smithy mode.")
(define-abbrev-table 'smithy-mode-abbrev-table ())

(defconst smithy-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments are double-slashes, reaching to EOL.
    (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?\n ">" table)

    ;; Paired single quotes delimit strings.
    (modify-syntax-entry ?' "\"" table)

    ;; Periods, hashes, and dollar signs are part of shape IDs, and
    ;; need symbol syntax.
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?# "_" table)
    (modify-syntax-entry ?$ "_" table)

    (modify-syntax-entry ?@ "_" table)

    table)
  "Syntax table for Smithy mode.")

;; Smithy doesn't really have operator precedence.
(defconst smithy-mode-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '()
    )
   ))

(defun smithy-backward-token ()
  (forward-comment (- (point)))
  (buffer-substring-no-properties
   (point)
   (progn
     (if (zerop (skip-syntax-backward "."))
         (skip-syntax-backward "w_'"))
     (point))))

(defun smithy-forward-token ()
  (forward-comment (point-max))
  (buffer-substring-no-properties
   (point)
   (progn
     (if (zerop (skip-syntax-forward "."))
         (skip-syntax-forward "w_'"))
     (point))))

(defun smithy-smie-rules (kind token)
  (pcase (cons kind token)
    ;; Hijack the rules that do basic element indentation.  Don't
    ;; change the offset for successive strings.  This is intended to make
    ;;
    ;;   @documentation("foo"
    ;;                  "bar")
    ;;
    ;; look good.  I don't know whether it will have any other impact.
    (`(:elem . basic)
     (if (and (looking-at "\\s\"\\|\\s|")
              (smithy-forward-token)
              (looking-at "\\s\"\\|\\s|"))
         0
       smithy-indent-basic))
    ;; When we're in a list, always have lines align with their
    ;; predecesor.  This covers lists, map key/value pairs, and lists
    ;; of traits preceeding a declaration.
    (`(:list-intro . ,_)
     0)
    ;; Inside curly brackets.  We want C-style indentation of
    ;; `smithy-indent-basic' columns relative to the enclosing line.
    ;; Point is just before the token that we have matched.
    (`(:after . "{")
     (let ((enclosing-indent (save-excursion
                               (back-to-indentation)
                               (current-column))))
       (cons 'column (+ smithy-indent-basic enclosing-indent))))))

;;;###autoload
(define-derived-mode smithy-mode prog-mode "Smithy"
  "Major mode for Smithy API definition files."
  :syntax-table smithy-mode-syntax-table
  :abbrev-table smithy-mode-abbrev-table
  :group 'smithy

  (setq font-lock-defaults '(smithy-mode-font-lock-keywords nil nil nil))

  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-use-syntax) t)

  (set (make-local-variable 'smie-backward-token-function) 'smithy-backward-token)
  (set (make-local-variable 'smie-forward-token-function) 'smithy-forward-token)
  (smie-setup smithy-mode-smie-grammar #'smithy-smie-rules))

(add-to-list 'auto-mode-alist '("\\.smithy\\'" . smithy-mode))

(provide 'smithy-mode)
