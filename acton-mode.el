;;; acton-mode.el --- Major mode for editing Acton source code
;;; ;; -*- lexical-binding: t; -*-

;; Copyright (C) Kristian Larsson <k@centor.se>
;; Author: Kristian Larsson
;; Keywords: languages programming
;; Homepage: https://github.com/actonlang/acton-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This is a major mode for the Acton programming language
;;
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
  '("actor" "after" "and" "as" "assert" "async" "await"
    "break" "class" "continue" "def" "del" "elif"
    "else" "extension" "finally" "for" "from" "if" "import"
    "in" "is" "lambda" "not" "or" "pass" "protocol"
    "raise" "return" "try" "var" "while" "with" "yield"))

(defvar acton-constants
  '("NotImplemented" "..."))

(defvar acton-effects
  '("_" "proc" "mut" "pure" "action"))

(defvar acton-decorators
  '("@property" "@staticmethod"))

;; Create the regex for different syntax elements
(defconst acton-font-lock-keywords
  (let ((kw-re (regexp-opt acton-keywords 'words))
        (const-re (regexp-opt acton-constants 'words))
        (effects-re (regexp-opt acton-effects 'words))
        (decorator-re (regexp-opt acton-decorators)))
    `(
      ;; Decorators
      (,(concat "^[ \t]*" decorator-re "[ \t]*$") . font-lock-preprocessor-face)
      ;; Keywords
      (,kw-re . font-lock-keyword-face)
      ;; Constants
      (,const-re . font-lock-constant-face)
      ;; Effects
      (,effects-re . font-lock-builtin-face)
      ;; Function definitions
      ("\\<def\\>[ \t]+\\([[:alnum:]_]+\\)"
       (1 font-lock-function-name-face))
      ;; Class/Protocol/Extension definitions
      ("\\<\\(class\\|protocol\\|extension\\)\\>[ \t]+\\([[:alnum:]_]+\\)"
       (2 font-lock-type-face))
      ;; Actor definitions
      ("\\<actor\\>[ \t]+\\([[:alnum:]_]+\\)"
       (1 font-lock-type-face))
      ;; String literals
      ("\\(\"\"\"[^\"]*\"\"\"\\|'''[^']*'''\\|\"[^\"\\]*\\(\\\\.[^\"\\]*\\)*\"\\|'[^'\\]*\\(\\\\.[^'\\]*\\)*'\\)"
       . font-lock-string-face)
      ;; Numbers
      ("\\<\\([0-9]+\\([.][0-9]*\\)?\\|[0-9]*[.][0-9]+\\)\\([eE][+-]?[0-9]+\\)?[jJ]?"
       . font-lock-constant-face)
      ;; Operators
      ("\\([-+*/%&|^<>=!]=?\\|//\\|<<\\|>>\\|\\*\\*\\|->\\|=>\\)"
       . font-lock-builtin-face)
      ;; Comments
      ("#.*$" . font-lock-comment-face))))

;;;###autoload
(define-derived-mode acton-mode prog-mode "Acton"
  "Major mode for editing Acton source code."
  :syntax-table acton-mode-syntax-table

  ;; Comment setup
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+\\s-*")

  ;; Indentation setup
  (setq-local indent-line-function 'acton-indent-line)
  (setq-local tab-width 4)  ; Default to 4 spaces per indent level

  ;; Font lock setup
  (setq-local font-lock-defaults
              '(acton-font-lock-keywords  ; keywords
                nil                       ; keywords-only
                nil                       ; case-fold
                nil                       ; syntax-alist
                nil)))                    ; syntax-begin

;; Create and set up syntax table
(defvar acton-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?' "\"" table)

    ;; Pairs
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Operators
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?^ "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?= "." table)

    ;; Symbol constituents
    (modify-syntax-entry ?_ "_" table)
    table))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.act\\'" . acton-mode))

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
      (if (bobp)  ; Beginning of buffer?
          0
        (let ((indent 0))
          ;; Previous non-empty line indentation
          (while (and (zerop indent) (not (bobp)))
            (forward-line -1)
            (unless (looking-at "^[ \t]*$") ; Skip empty lines
              (setq indent (current-indentation))))

          ;; Increase indent after lines ending with :
          (save-excursion
            (when (and (skip-chars-backward " \t\n") (not (bobp)))
              (when (looking-back ":[ \t]*" (line-beginning-position))
                (setq indent (+ indent tab-width)))))

          ;; Return the indentation level
          indent)))))

(provide 'acton-mode)

;;; acton-mode.el ends here
