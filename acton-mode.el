;;; acton-mode.el -*- lexical-binding: t; -*-
;;; Major mode for editing Acton source code

;; Copyright (C) Kristian Larsson <k@centor.se>
;; Author: Kristian Larsson
;; Keywords: languages programming
;; Homepage: https://github.com/actonlang/acton-mode
;; Version: 0.3.0
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This is a major mode for the Acton programming language

;;; License:
;;
;; Redistribution and use in source and binary forms, with or without modification,
;; are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;; list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;; this list of conditions and the following disclaimer in the documentation and/or
;; other materials provided with the distribution.
;;
;; 3. Neither the name of the copyright holder nor the names of its contributors
;; may be used to endorse or promote products derived from this software without
;; specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(require 'cl-lib)
(require 'rx)

(defgroup acton nil
  "Major mode for editing Acton source code."
  :prefix "acton-"
  :group 'languages)

;; Define several categories of keywords
(defvar acton-keywords
  '("if" "elif" "else" "while" "for" "in" "try" "except" "finally"
    "with" "return" "break" "continue" "pass" "raise" "yield" "from"
    "import" "as" "global" "nonlocal" "assert" "await" "async" "del"
    "lambda"))

(defvar acton-declarations
  '("def" "class" "actor" "protocol" "extension" "var"))

(defvar acton-constants
  '("True" "False" "None" "NotImplemented" "..."))

(defvar acton-effects
  '("proc" "mut" "pure" "action"))

(defvar acton-builtin-functions
  '("isinstance"))

(defvar acton-decorators
  '("@property" "@staticmethod" "@static"))

;; Create the regex string for each category of keywords
(defconst acton-font-lock-keywords
  (let ((kw-re (concat "\\_<" (regexp-opt acton-keywords t) "\\_>"))
        (decl-re (concat "\\_<" (regexp-opt acton-declarations t) "\\_>"))
        (const-re (concat "\\_<" (regexp-opt acton-constants t) "\\_>"))
        (effects-re (concat "\\_<" (regexp-opt acton-effects t) "\\_>"))
        (builtin-re (concat "\\_<" (regexp-opt acton-builtin-functions t) "\\_>"))
        (decorator-re (regexp-opt acton-decorators)))
    `(
      ;; Triple-quoted strings
      ("\\(?:\"\"\"[\\s\\S]*?\"\"\"\\|'''[\\s\\S]*?'''\\)"
       0 font-lock-string-face)

      ;; Raw strings
      ("\\<r\\(?:\"[^\"\\]*\\(?:\\\\.[^\"\\]*\\)*\"\\|'[^'\\]*\\(?:\\\\.[^'\\]*\\)*'\\)"
       0 font-lock-string-face)

      ;; Byte strings
      ("\\<b\\(?:\"[^\"\\]*\\(?:\\\\.[^\"\\]*\\)*\"\\|'[^'\\]*\\(?:\\\\.[^'\\]*\\)*'\\)"
       0 font-lock-string-face)

      ;; Regular strings with escapes
      ("\\(?:\"[^\"\\]*\\(?:\\\\.[^\"\\]*\\)*\"\\|'[^'\\]*\\(?:\\\\.[^'\\]*\\)*'\\)"
       0 font-lock-string-face)

      ;; String escapes
      ("\\\\[abfnrtv'\"\\\\]\\|\\\\[0-7]\\{1,3\\}\\|\\\\x[[:xdigit:]]\\{2\\}\\|\\\\u[[:xdigit:]]\\{4\\}\\|\\\\U[[:xdigit:]]\\{8\\}"
       0 font-lock-escape-face t)

      ;; Numbers - complex
      ("\\<\\(?:[0-9]+\\.[0-9]*\\|[0-9]*\\.[0-9]+\\)\\(?:[eE][+-]?[0-9]+\\)?[jJ]\\>"
       . font-lock-constant-face)
      ("\\<[0-9]+[jJ]\\>" . font-lock-constant-face)

      ;; Numbers - hex
      ("\\<0x[0-9a-fA-F]+\\>" . font-lock-constant-face)
      ;; Numbers - octal
      ("\\<0o[0-7]+\\>" . font-lock-constant-face)
      ;; Numbers - float
      ("\\<\\(?:[0-9]+\\.[0-9]*\\|[0-9]*\\.[0-9]+\\)\\(?:[eE][+-]?[0-9]+\\)?\\>"
       . font-lock-constant-face)
      ;; Numbers - decimal
      ("\\<[0-9]+\\>" . font-lock-constant-face)

      ;; Operators
      ;; Arithmetic operators
      ("[-+*/%@]\\|//\\|\\*\\*" . font-lock-builtin-face)
      ;; Bitwise operators
      ("[&|^~]\\|<<\\|>>" . font-lock-builtin-face)
      ;; Comparison operators
      ("==\\|!=\\|<=\\|>=\\|<\\|>" . font-lock-builtin-face)
      ;; Assignment operators
      ("\\(?:[-+*/%&|^]\\|//\\|\\*\\*\\|<<\\|>>\\)=" . font-lock-builtin-face)
      ;; Special operators
      ("\\_<\\(?:is\\s-+not\\|not\\s-+in\\)\\_>" . font-lock-builtin-face)
      ("\\(?:->\\|=>\\)" . font-lock-builtin-face)

      ;; Type variables and annotations
      ;; Generic type variables (T, T0, T1, etc.)
      ("\\_<[A-Z]\\d*\\_>" . font-lock-type-face)
      ;; Type annotations with optional whitespace
      (":\\s-*\\([A-Z][a-zA-Z0-9_]*\\(?:\\[[^]]*\\]\\)?\\)" 1 font-lock-type-face)

      ;; Decorators
      (,(concat "^[ \t]*" decorator-re "[ \t]*\\(?:$\\|[^[:alnum:]_]\\)")
       . font-lock-preprocessor-face)

      ;; Keywords
      (,kw-re . font-lock-keyword-face)
      (,decl-re . font-lock-keyword-face)
      (,const-re . font-lock-constant-face)
      (,effects-re . font-lock-builtin-face)
      (,builtin-re . font-lock-builtin-face)

      ;; Function definitions
      ("\\<def\\>[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
       (1 font-lock-function-name-face))

      ;; Type annotations
      (":[ \t]*\\([A-Z][a-zA-Z0-9_]*\\)"
       (1 font-lock-type-face))

      ;; Class/Protocol/Extension definitions
      ("\\<\\(?:class\\|protocol\\|extension\\)\\>[ \t]+\\([A-Z][a-zA-Z0-9_]*\\)"
       (1 font-lock-type-face))

      ;; Actor definitions
      ("\\<actor\\>[ \t]+\\([A-Z][a-zA-Z0-9_]*\\)"
       (1 font-lock-type-face))

      ;; Generic type parameters
      ("\\<[A-Z]\\d*\\>" . font-lock-type-face)

      ;; Comments
      ("#.*$" . font-lock-comment-face))))

;; Indentation settings
(defcustom acton-indent-offset 4
  "Number of spaces for each indentation step in `acton-mode'."
  :type 'integer
  :safe 'integerp
  :group 'acton)

;; Create and set up syntax table
(defvar acton-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Pairs
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Operators
    (dolist (char '(?+ ?- ?* ?/ ?% ?& ?| ?^ ?! ?< ?> ?= ?~))
      (modify-syntax-entry char "." table))

    ;; Symbol constituents
    (modify-syntax-entry ?_ "_" table)
    table))

(defun acton-indent-line ()
  "Indent current line as Acton code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil
                    (max (acton-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun acton-calculate-indentation ()
  "Calculate the indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (let ((syntax-bol (syntax-ppss)))
      (cond
       ;; Inside a string
       ((nth 3 syntax-bol)
        (save-excursion
          (goto-char (nth 8 syntax-bol))  ; Go to start of string
          (current-indentation)))

       ;; Inside a paren/bracket/brace
       ((nth 1 syntax-bol)
        (goto-char (nth 1 syntax-bol))
        (+ (current-indentation)
           (if (looking-at "[[({][ \t]*$")
               acton-indent-offset
             1)))

       ;; Normal indentation
       (t
        (let ((indent 0)
              (saw-colon nil))
          ;; Find previous non-blank line
          (while (and (zerop indent) (not (bobp)))
            (forward-line -1)
            (when (not (looking-at "^[ \t]*$"))
              (setq indent (current-indentation))
              ;; Check for colon at end of line
              (when (looking-at ".*:[ \t]*$")
                (setq saw-colon t))))

          ;; Adjust indentation based on context
          (cond
           ;; After colon, indent one level
           (saw-colon
            (+ indent acton-indent-offset))
           ;; At top level
           ((zerop indent)
            0)
           ;; Default: keep previous indentation
           (t indent))))))))

;;;###autoload
(define-derived-mode acton-mode prog-mode "Acton"
  "Major mode for editing Acton source code."
  :syntax-table acton-mode-syntax-table

  ;; Comment setup
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)

  ;; Indentation
  (setq-local indent-line-function 'acton-indent-line)
  (setq-local tab-width acton-indent-offset)
  (setq-local indent-tabs-mode nil)  ; Use spaces for indentation

  ;; Font lock
  (setq-local font-lock-defaults
              '(acton-font-lock-keywords  ; keywords
                nil                       ; keywords-only
                nil                       ; case-fold
                nil                       ; syntax-alist
                beginning-of-defun))      ; syntax-begin

  ;; Paragraph separation
  (setq-local paragraph-start (concat "^[ \t]*$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)

  ;; Add imenu support
  (setq-local imenu-generic-expression
              '(("Class" "^[ \t]*class[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
                ("Actor" "^[ \t]*actor[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
                ("Protocol" "^[ \t]*protocol[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
                ("Extension" "^[ \t]*extension[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1)
                ("Function" "^[ \t]*def[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)" 1))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.act\\'" . acton-mode))

(provide 'acton-mode)

;;; acton-mode.el ends here
