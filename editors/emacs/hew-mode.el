;;; hew-mode.el --- Major mode for the Hew programming language -*- lexical-binding: t; -*-

;; Author: Hew Contributors
;; URL: https://github.com/hew-lang/hew
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Provides syntax highlighting, indentation, and comment support for
;; the Hew actor-oriented programming language.

;;; Code:

(defgroup hew nil
  "Support for the Hew programming language."
  :group 'languages
  :prefix "hew-")

(defcustom hew-indent-offset 4
  "Number of spaces for each indentation level in Hew."
  :type 'integer
  :group 'hew
  :safe #'integerp)

;; Syntax table
(defvar hew-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; // line comments
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    ;; Characters
    (modify-syntax-entry ?\' "\"" st)
    ;; Underscores in identifiers
    (modify-syntax-entry ?_ "w" st)
    ;; Punctuation
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?% "." st)
    ;; Braces/parens
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    st)
  "Syntax table for `hew-mode'.")

;; Keywords
;; Keywords — sourced from hew-lexer ALL_KEYWORDS (single source of truth).
;; v0.5 additions: is, emit, entry, exit (machine sub-keywords); fork promoted
;;                 from actor group.
;; v0.6: struct keyword removed (#[wire] type/enum only); wire keyword removed
;;       (attribute-only via #[wire]); await_restart added; mut added;
;;       ActorRef -> LocalPid/RemotePid/LambdaPid.
(defconst hew-keywords
  '("if" "else" "is" "match" "loop" "for" "in" "while"
    "break" "continue" "return"
    "let" "var" "const" "mut" "fn" "gen" "type" "record" "indirect" "enum"
    "trait" "impl" "import" "pub" "super" "where"
    "actor" "fork" "receive" "init" "spawn" "async" "move" "await" "await_restart" "this"
    "supervisor" "child" "restart" "budget" "strategy"
    "reserved" "optional" "deprecated" "default"
    "machine" "state" "event" "on" "when" "entry" "exit" "emit"
    "try" "catch" "select" "join" "yield" "cooperate" "after" "from"
    "scope" "race" "defer" "foreign"
    "dyn" "unsafe" "extern" "package"
    "as")
  "Hew language keywords.")

(defconst hew-builtin-types
  '("i8" "i16" "i32" "i64" "u8" "u16" "u32" "u64"
    "f32" "f64" "isize" "usize"
    "bool" "char" "string" "bytes" "void" "never" "duration" "instant"
    "Result" "Option" "Ok" "Err" "Some" "Vec" "HashMap" "HashSet" "Range"
    "Box" "Arc" "Rc" "Weak"
    "LocalPid" "RemotePid" "LambdaPid" "Task" "Scope"
    "Generator" "AsyncGenerator" "Stream" "Sink"
    "Self")
  "Hew built-in types.")

(defconst hew-builtin-traits
  '("Send" "Frozen" "Copy" "Drop" "Clone"
    "Eq" "Ord" "Hash" "Display" "Debug"
    "Default" "Iterator" "AsyncIterator"
    "IntoIterator" "Into" "From" "Try" "Allocator")
  "Hew built-in traits.")

(defconst hew-constants
  '("true" "false" "None"
    "one_for_one" "one_for_all" "rest_for_one" "simple_one_for_one" "pool"
    "permanent" "transient" "temporary"
    "brutal_kill"
    "block" "drop_new" "drop_old" "fail" "coalesce" "fallback")
  "Hew built-in constants.")

;; Font-lock (syntax highlighting)
(defvar hew-font-lock-keywords
  (let ((kw-re (regexp-opt hew-keywords 'symbols))
        (type-re (regexp-opt hew-builtin-types 'symbols))
        (trait-re (regexp-opt hew-builtin-traits 'symbols))
        (const-re (regexp-opt hew-constants 'symbols)))
    `(
      ;; Attributes: #[...]
      ("\\(#\\[\\)[^]]*\\(\\]\\)" . font-lock-preprocessor-face)

      ;; Keywords
      (,kw-re . font-lock-keyword-face)

      ;; Built-in types
      (,type-re . font-lock-type-face)

      ;; Traits
      (,trait-re . font-lock-type-face)

      ;; Constants
      (,const-re . font-lock-constant-face)

      ;; Function definitions: fn name(
      ("\\<fn\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-function-name-face)

      ;; Actor/enum/trait/impl definitions
      ("\\<\\(actor\\|enum\\|trait\\|impl\\|supervisor\\)\\s-+\\([A-Z][a-zA-Z0-9_]*\\)"
       2 font-lock-type-face)

      ;; PascalCase type names
      ("\\<\\([A-Z][a-zA-Z0-9_]*\\)\\>" 1 font-lock-type-face)

      ;; this — the current actor/record receiver
      ("\\<this\\>" . font-lock-variable-name-face)

      ;; Numeric literals
      ("\\<0[xX][0-9a-fA-F_]+\\>" . font-lock-constant-face)
      ("\\<0[bB][01_]+\\>" . font-lock-constant-face)
      ("\\<0[oO][0-7_]+\\>" . font-lock-constant-face)
      ("\\<[0-9][0-9_]*\\(?:\\.[0-9][0-9_]*\\)?\\(?:[eE][+-]?[0-9_]+\\)?\\>" . font-lock-constant-face)

      ;; Regex literals: re"..."
      ("re\"" . font-lock-string-face)

      ;; Format strings: f"..."
      ("\\<f\"" . font-lock-string-face)

      ;; @reenter decorator (machine state-transition re-entry)
      ("@reenter\\>" . font-lock-builtin-face)

      ;; Labels: @label
      ("@\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-constant-face)))
  "Font-lock keywords for `hew-mode'.")

;; Indentation
(defun hew-indent-line ()
  "Indent the current line in Hew mode."
  (interactive)
  (let ((indent (hew--calculate-indent)))
    (when indent
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion (indent-line-to indent))))))

(defun hew--calculate-indent ()
  "Calculate the indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; At the beginning of the buffer
     ((bobp) 0)
     ;; Closing brace — match the opening line's indent
     ((looking-at "^\\s-*[})]")
      (hew--matching-brace-indent))
     ;; Otherwise, base on previous line
     (t
      (hew--previous-line-indent)))))

(defun hew--matching-brace-indent ()
  "Find the indentation of the line with the matching open brace."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (let ((close-char (char-after)))
      (condition-case nil
          (progn
            (forward-char 1)
            (backward-sexp 1)
            (current-indentation))
        (error 0)))))

(defun hew--previous-line-indent ()
  "Calculate indent based on the previous non-blank line."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp)) (looking-at "^\\s-*$"))
      (forward-line -1))
    (let ((prev-indent (current-indentation))
          (opens-block (hew--line-opens-block-p)))
      (if opens-block
          (+ prev-indent hew-indent-offset)
        prev-indent))))

(defun hew--line-opens-block-p ()
  "Return non-nil if the current line opens a block (ends with `{')."
  (save-excursion
    (end-of-line)
    ;; Skip trailing comments
    (let ((eol (point)))
      (beginning-of-line)
      (let ((comment-start-pos (re-search-forward "//" eol t)))
        (if comment-start-pos
            (goto-char (- comment-start-pos 2))
          (goto-char eol))))
    (skip-chars-backward " \t")
    (and (> (point) (line-beginning-position))
         (eq (char-before) ?{))))

;; Comment support
(defun hew-comment-dwim (arg)
  "Comment or uncomment the current line or region.
With prefix ARG, use that many comment characters."
  (interactive "*P")
  (let ((comment-start "// ")
        (comment-end ""))
    (comment-dwim arg)))

;; Mode map
(defvar hew-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap comment-dwim] #'hew-comment-dwim)
    map)
  "Keymap for `hew-mode'.")

;;;###autoload
(define-derived-mode hew-mode prog-mode "Hew"
  "Major mode for editing Hew source code.

\\{hew-mode-map}"
  :syntax-table hew-mode-syntax-table
  :group 'hew

  ;; Font-lock
  (setq font-lock-defaults '(hew-font-lock-keywords))

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s-*")
  (setq-local comment-multi-line t)

  ;; Indentation
  (setq-local indent-line-function #'hew-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width hew-indent-offset)

  ;; Electric pairs
  (setq-local electric-pair-pairs '((?\{ . ?\}) (?\( . ?\)) (?\[ . ?\])))

  ;; Paragraphs
  (setq-local paragraph-start (concat "^\\s-*$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hew\\'" . hew-mode))

(provide 'hew-mode)

;;; hew-mode.el ends here
