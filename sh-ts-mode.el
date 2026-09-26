;;; sh-ts-mode.el --- Tree-sitter mode for POSIX sh  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 konomanoasa
;;
;; Author: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Maintainer: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: languages
;; URL: https://github.com/konomanoasa/sh-ts-mode
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; Tree-sitter major mode for POSIX sh.

;;; Code:

(require 'treesit)
(require 'elec-pair)

(defgroup sh-ts nil
  "Tree-sitter mode for POSIX sh."
  :group 'languages)

;;;; Grammar

(defconst sh-ts-mode--grammar-sources
  '((sh "https://github.com/konomanoasa/tree-sitter-sh"
        :revision "v0.18.0"))
  "Tree-sitter grammar sources for POSIX sh.")

(defun sh-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language sh-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (or (treesit-ensure-installed language)
        (user-error "Tree-sitter grammar `%s' is unavailable" language))))

;;;; Context

(defconst sh-ts-mode--pattern-context-types
  '("pattern_list" "parameter_pattern")
  "Node types that enter active shell patterns.")

(defconst sh-ts-mode--pattern-boundary-types
  '("command_substitution_body" "backquote_substitution_body")
  "Node types that leave active shell patterns.")

(defconst sh-ts-mode--pathname-owner-types
  '("cmd_name" "cmd_word" "cmd_suffix" "wordlist" "filename")
  "Node types that can own pathname patterns.")

(defconst sh-ts-mode--pathname-pattern-regexp
  (rx string-start
      (or "pattern_bracket_source" "pattern_question_source" "pattern_star_source")
      string-end)
  "Regexp matching pathname pattern candidates.")

(defconst sh-ts-mode--pattern-scope-types
  (append sh-ts-mode--pattern-context-types
          sh-ts-mode--pattern-boundary-types
          '("tilde_expansion"))
  "Node types whose nested patterns belong to another pattern scope.")

(defun sh-ts-mode--continued-lexical-token-p (node)
  "Return non-nil when NODE's parent has only continuation leaves."
  (let* ((parent (treesit-node-parent node))
         (index 0)
         (count (treesit-node-child-count parent)))
    (while (and (< index count)
                (equal (treesit-node-type (treesit-node-child parent index))
                       "line_continuation"))
      (setq index (1+ index)))
    (= index count)))

(defun sh-ts-mode--pathname-pattern-word-p (word)
  "Return non-nil when WORD contains a pathname pattern."
  (treesit-search-subtree
   word
   `(and ,sh-ts-mode--pathname-pattern-regexp
         ,(lambda (node)
            (let ((parent (treesit-node-parent node)))
              (while (and parent
                          (not (treesit-node-eq parent word))
                          (not (member (treesit-node-type parent)
                                       sh-ts-mode--pattern-scope-types)))
                (setq parent (treesit-node-parent parent)))
              (treesit-node-eq parent word))))))

(defun sh-ts-mode--pattern-interior-p (node)
  "Return non-nil when NODE is inside any shell pattern."
  (let (result)
    (while node
      (let ((parent (treesit-node-parent node))
            (type (treesit-node-type node)))
        (cond
         ((member type sh-ts-mode--pattern-context-types)
          (setq result t node nil))
         ((or (member type sh-ts-mode--pattern-boundary-types)
              (member type '("simple_command" "for_clause" "case_clause"
                             "io_file" "io_here")))
          (setq node nil))
         ((and parent
               (equal type "word")
               (member (treesit-node-type parent)
                       sh-ts-mode--pathname-owner-types))
          (setq result (sh-ts-mode--pathname-pattern-word-p node)
                node nil))
         (t (setq node parent)))))
    result))

(defun sh-ts-mode--outside-pattern-interior-p (node)
  "Return non-nil when NODE is outside every shell pattern."
  (not (sh-ts-mode--pattern-interior-p node)))

(defun sh-ts-mode--pattern-source-owner (node)
  "Return the owner outside the pattern source structure of NODE."
  (let ((parent (treesit-node-parent node)))
    (while (and parent
                (member (treesit-node-type parent)
                        '("pattern_bracket_source"
                          "pattern_bracket_negation_source"
                          "pattern_bracket_members_source"
                          "pattern_bracket_range_source"
                          "pattern_character_class_source"
                          "pattern_collating_symbol_source"
                          "pattern_equivalence_class_source")))
      (setq parent (treesit-node-parent parent)))
    parent))

(defun sh-ts-mode--tilde-pattern-source-p (node)
  "Return non-nil when NODE is pattern source owned by a tilde prefix."
  (let ((owner (sh-ts-mode--pattern-source-owner node)))
    (and owner (equal (treesit-node-type owner) "tilde_user"))))

(defun sh-ts-mode--plain-pattern-source-p (node)
  "Return non-nil when NODE is pattern source treated as plain text."
  (let* ((owner (sh-ts-mode--pattern-source-owner node))
         (type (and owner (treesit-node-type owner)))
         (parent (and owner (treesit-node-parent owner))))
    (and (not (equal type "tilde_user"))
         (not (member type sh-ts-mode--pattern-context-types))
         (not (and (equal type "word") parent
                   (member (treesit-node-type parent)
                           sh-ts-mode--pathname-owner-types)))
         (sh-ts-mode--outside-pattern-interior-p owner))))

(defun sh-ts-mode--shell-pattern-source-p (node)
  "Return non-nil when NODE is pattern source outside a tilde prefix."
  (and (sh-ts-mode--pattern-interior-p node)
       (not (sh-ts-mode--tilde-pattern-source-p node))))

(defun sh-ts-mode--string-literal-p (node)
  "Return non-nil when NODE has a string literal owner."
  (let* ((parent (treesit-node-parent node))
         (owner (and parent (treesit-node-parent parent))))
    (and (not (and parent
                   (equal (treesit-node-type parent) "tilde_user")))
         (not (and parent owner
                   (equal (treesit-node-type parent) "word")
                   (member (treesit-node-type owner)
                           '("cmd_name" "cmd_word")))))))

;;;; Syntax

(defvar sh-ts-mode-syntax--text-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (dolist (character '(?# ?$ ?' ?` ?\" ?\\ ?\( ?\) ?\[ ?\] ?{ ?}))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for text without a CST syntax classification.")

(defvar sh-ts-mode-syntax-table
  (let ((table (copy-syntax-table sh-ts-mode-syntax--text-table)))
    (dolist (entry '((?\( . "()") (?\) . ")(")
                     (?\[ . "(]") (?\] . ")[")
                     (?{ . "(}") (?} . "){")))
      (modify-syntax-entry (car entry) (cdr entry) table))
    table)
  "Syntax table for `sh-ts-mode'.")

;;;;; Syntax Queries

(defconst sh-ts-mode-syntax--query
  (treesit-query-compile
   'sh
   '((comment) @comment
     (parameter_expansion ["{" "}"] @delimiter)
     (command_substitution ["(" ")"] @delimiter)
     (arithmetic_expansion ["(" ")"] @delimiter)
     (function_definition ["(" ")"] @delimiter)
     (case_item
      patterns: (pattern_list "(" @delimiter)
      ")" @delimiter)
     (case_item_ns
      patterns: (pattern_list "(" @delimiter)
      ")" @delimiter)
     (brace_group ["{" "}"] @delimiter)
     (subshell ["(" ")"] @delimiter)
     (parenthesized_arithmetic ["(" ")"] @delimiter)
     (parenthesized_arithmetic_source ["(" ")"] @delimiter)
     (parenthesized_arithmetic_dynamic_source
      ["(" ")"] @delimiter)))
  "Compiled syntax query for POSIX sh.")

;;;;; Propertization

(defun sh-ts-mode-syntax--delimiter-syntax (position)
  "Return the syntax descriptor for the delimiter at POSITION."
  (pcase (char-after position)
    (?\( (string-to-syntax "()"))
    (?\) (string-to-syntax ")("))
    (?{ (string-to-syntax "(}"))
    (?} (string-to-syntax "){"))))

(defun sh-ts-mode-syntax--propertize (start end)
  "Apply syntax properties between START and END."
  (let ((accessible-start (point-min)))
    (save-restriction
      (widen)
      (when (and (= start accessible-start)
                 (> accessible-start (point-min)))
        (setq start (point-min))
        (syntax-ppss-flush-cache start))
      (put-text-property start end 'syntax-table sh-ts-mode-syntax--text-table)
      (dolist (capture (treesit-query-capture
                        (treesit-parser-root-node treesit-primary-parser)
                        sh-ts-mode-syntax--query start end))
        (let* ((name (car capture))
               (node (cdr capture))
               (position (if (eq name 'comment)
                             (treesit-node-start node)
                           (1- (treesit-node-end node)))))
          (if (eq name 'comment)
              (let ((end (treesit-node-end node)))
                (put-text-property position (1+ position) 'syntax-table
                                   (string-to-syntax "< b"))
                (when (< end (point-max))
                  (put-text-property end (1+ end) 'syntax-table
                                     (string-to-syntax "> b"))))
            (put-text-property
             position (1+ position) 'syntax-table
             (sh-ts-mode-syntax--delimiter-syntax position))))))))

;;;;; Setup

(defun sh-ts-mode-syntax--setup ()
  "Configure syntax handling for the current buffer."
  (setq-local syntax-propertize-function
              #'sh-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#[[:blank:]]*")
  (setq-local comment-use-syntax t))

;;;; Electric Pair

(defun sh-ts-mode-electric-pair--newline-context-p ()
  "Return non-nil for a multiline CST delimiter pair around the newline."
  (when (and (eq (char-before) ?\n)
             (>= (- (point) 2) (point-min))
             (< (point) (point-max)))
    (let* ((opening (treesit-node-at (- (point) 2) treesit-primary-parser))
           (closing (treesit-node-at (point) treesit-primary-parser))
           (owner (treesit-node-parent opening)))
      (and (= (treesit-node-start opening) (- (point) 2))
           (= (treesit-node-end opening) (1- (point)))
           (= (treesit-node-start closing) (point))
           (= (treesit-node-end closing) (1+ (point)))
           (treesit-node-eq owner (treesit-node-parent closing))
           (member (treesit-node-type owner)
                   '("brace_group" "subshell" "command_substitution"
                     "parenthesized_arithmetic"
                     "parenthesized_arithmetic_source"
                     "parenthesized_arithmetic_dynamic_source"))))))

(defun sh-ts-mode-electric-pair--setup ()
  "Configure electric pairing for the current buffer."
  (let ((pairs '((?\( . ?\)) (?\[ . ?\]) (?{ . ?})))
        (table (copy-syntax-table (syntax-table))))
    (setq-local electric-pair-pairs (append electric-pair-pairs pairs))
    (dolist (pair pairs)
      (unless (eq (cdr (assq (car pair) electric-pair-pairs)) (cdr pair))
        (modify-syntax-entry (car pair) "." table)))
    (set-syntax-table table))
  (let ((setting electric-pair-open-newline-between-pairs))
    (setq-local electric-pair-open-newline-between-pairs
                (lambda ()
                  (and (if (functionp setting) (funcall setting) setting)
                       (sh-ts-mode-electric-pair--newline-context-p))))))

;;;; Font Lock

;;;;; Features

(defconst sh-ts-mode-font-lock--feature-list
  '((comment)
    (keyword function command string)
    (number constant variable escape)
    (pattern operator punctuation bracket))
  "Font-lock features by decoration level.")

;;;;; Settings

(defun sh-ts-mode-font-lock--pattern-source-query (face predicate &optional scope)
  "Return leaf queries using FACE when PREDICATE and SCOPE match."
  (mapcar
   (lambda (pattern)
     (append (list pattern (list :pred predicate face))
             (when scope (list (list :pred scope face)))))
   `(([(pattern_star_source) (pattern_question_source)
       (pattern_bracket_character_source) (pattern_bracket_hyphen_source)
       (pattern_bracket_range_operator_source)
       (pattern_character_class_content_source)
       (pattern_collating_symbol_character_source)
       (pattern_equivalence_class_character_source)] ,face)
     (pattern_bracket_source ["[" "]"] ,face)
     ((pattern_bracket_negation_source) ,face)
     (pattern_character_class_source ["[" ":" "]"] ,face)
     (pattern_collating_symbol_source ["[" "." "]"] ,face)
     (pattern_equivalence_class_source ["[" "=" "]"] ,face))))

(defun sh-ts-mode-font-lock--settings ()
  "Return font-lock settings for the current buffer."
  (treesit-font-lock-rules
   :default-language 'sh

   :feature 'comment
   '((comment_text) @font-lock-comment-face)

   :feature 'keyword
   '([(case_keyword)
      (do_keyword)
      (done_keyword)
      (elif_keyword)
      (else_keyword)
      (esac_keyword)
      (fi_keyword)
      (for_keyword)
      (if_keyword)
      (in_keyword)
      (then_keyword)
      (until_keyword)
      (while_keyword)] @font-lock-keyword-face)

   :feature 'function
   '((function_definition
      name: (fname) @font-lock-function-name-face))

   :feature 'command
   '(((cmd_name
       (word
        (literal) @font-lock-function-call-face))
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-function-call-face))
     ((cmd_word
       (word
        (literal) @font-lock-function-call-face))
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-function-call-face)))

   :feature 'string
   '(((literal) @font-lock-string-face
      (:pred sh-ts-mode--string-literal-p @font-lock-string-face)
      (:pred sh-ts-mode--outside-pattern-interior-p @font-lock-string-face))
     ((single_quoted
       "'" @font-lock-string-face)
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-string-face))
     ((double_quoted
       "\"" @font-lock-string-face)
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-string-face))
     ((dollar_single_quoted
       ["$" "'"] @font-lock-string-face)
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-string-face))
     ((backquote_substitution
       "`" @font-lock-string-face)
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-string-face))
     ([(single_quote_content)
       (double_quote_text)
       (dollar_single_quote_text)
       (here_document_text)
       (quoted_here_document_text)] @font-lock-string-face
       (:pred sh-ts-mode--outside-pattern-interior-p
              @font-lock-string-face))
     (here_document_end_text) @font-lock-string-face)

   :feature 'string
   (sh-ts-mode-font-lock--pattern-source-query
    '@font-lock-string-face #'sh-ts-mode--plain-pattern-source-p)

   :feature 'number
   '(([(arithmetic_number) (io_number)] @font-lock-number-face
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-number-face)))

   :feature 'constant
   (sh-ts-mode-font-lock--pattern-source-query
    '@font-lock-constant-face #'sh-ts-mode--tilde-pattern-source-p
    #'sh-ts-mode--outside-pattern-interior-p)

   :feature 'constant
   '(((parameter_expansion
       parameter: [(positional_parameter)
                   (special_parameter)] @font-lock-constant-face)
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-constant-face))
     ((tilde_expansion
       "~" @font-lock-constant-face)
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-constant-face))
     ((tilde_expansion
       user: (tilde_user
              (literal) @font-lock-constant-face))
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-constant-face)))

   :feature 'variable
   '(((assignment_word
       name: (variable_name) @font-lock-variable-name-face)
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-variable-name-face))
     ((for_clause
       name: (name) @font-lock-variable-name-face)
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-variable-name-face))
     ((parameter_expansion
       "$" @font-lock-variable-use-face
       parameter: (_))
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-variable-use-face))
     ((parameter_expansion
       parameter: (variable_name) @font-lock-variable-use-face)
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-variable-use-face))
     ((arithmetic_variable) @font-lock-variable-use-face
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-variable-use-face)))

   :feature 'escape
   '(([(escaped_character)
       (dollar_single_quote_escape)
       (double_quote_escape)
       (here_document_escape)] @font-lock-escape-face
       (:pred sh-ts-mode--outside-pattern-interior-p
              @font-lock-escape-face)))

   :feature 'pattern
   (sh-ts-mode-font-lock--pattern-source-query
    '@font-lock-constant-face #'sh-ts-mode--tilde-pattern-source-p
    #'sh-ts-mode--pattern-interior-p)

   :feature 'pattern
   '(([(pattern_bracket_character_source)
       (pattern_bracket_hyphen_source)
       (pattern_character_class_content_source)
       (pattern_collating_symbol_character_source)
       (pattern_equivalence_class_character_source)
       (pattern_question_source)
       (pattern_star_source)] @font-lock-constant-face
       (:pred sh-ts-mode--shell-pattern-source-p @font-lock-constant-face))
     ((pattern_bracket_source
       ["[" "]"] @font-lock-bracket-face)
      (:pred sh-ts-mode--shell-pattern-source-p @font-lock-bracket-face))
     ((pattern_bracket_negation_source) @font-lock-negation-char-face
      (:pred sh-ts-mode--shell-pattern-source-p @font-lock-negation-char-face))
     ((pattern_bracket_range_operator_source) @font-lock-operator-face
      (:pred sh-ts-mode--shell-pattern-source-p @font-lock-operator-face))
     ((pattern_character_class_source
       ["[" "]"] @font-lock-bracket-face)
      (:pred sh-ts-mode--shell-pattern-source-p @font-lock-bracket-face))
     ((pattern_character_class_source
       ":" @font-lock-punctuation-face)
      (:pred sh-ts-mode--shell-pattern-source-p @font-lock-punctuation-face))
     ((pattern_collating_symbol_source
       ["[" "]"] @font-lock-bracket-face)
      (:pred sh-ts-mode--shell-pattern-source-p @font-lock-bracket-face))
     ((pattern_collating_symbol_source
       "." @font-lock-punctuation-face)
      (:pred sh-ts-mode--shell-pattern-source-p @font-lock-punctuation-face))
     ((pattern_equivalence_class_source
       ["[" "]"] @font-lock-bracket-face)
      (:pred sh-ts-mode--shell-pattern-source-p @font-lock-bracket-face))
     ((pattern_equivalence_class_source
       "=" @font-lock-punctuation-face)
      (:pred sh-ts-mode--shell-pattern-source-p @font-lock-punctuation-face))
     (pattern_list "|" @font-lock-operator-face))

   :feature 'pattern
   '(((cmd_name (word (literal) @font-lock-function-call-face))
      (:pred sh-ts-mode--pattern-interior-p @font-lock-function-call-face))
     ((cmd_word (word (literal) @font-lock-function-call-face))
      (:pred sh-ts-mode--pattern-interior-p @font-lock-function-call-face))
     ((literal) @font-lock-string-face
      (:pred sh-ts-mode--string-literal-p @font-lock-string-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-string-face))
     ((single_quoted
       "'" @font-lock-string-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-string-face))
     ((double_quoted
       "\"" @font-lock-string-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-string-face))
     ((dollar_single_quoted
       ["$" "'"] @font-lock-string-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-string-face))
     ((backquote_substitution
       "`" @font-lock-string-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-string-face))
     ([(single_quote_content)
       (double_quote_text)
       (dollar_single_quote_text)] @font-lock-string-face
       (:pred sh-ts-mode--pattern-interior-p @font-lock-string-face))
     ((tilde_expansion
       "~" @font-lock-constant-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-constant-face))
     ((tilde_expansion
       user: (tilde_user
              (literal) @font-lock-constant-face))
      (:pred sh-ts-mode--pattern-interior-p @font-lock-constant-face))
     ([(arithmetic_number) (io_number)] @font-lock-number-face
      (:pred sh-ts-mode--pattern-interior-p @font-lock-number-face))
     ((parameter_expansion
       parameter: [(positional_parameter)
                   (special_parameter)] @font-lock-constant-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-constant-face))
     ((parameter_expansion
       "$" @font-lock-variable-use-face
       parameter: (_))
      (:pred sh-ts-mode--pattern-interior-p
             @font-lock-variable-use-face))
     ((parameter_expansion
       parameter: (variable_name) @font-lock-variable-use-face)
      (:pred sh-ts-mode--pattern-interior-p
             @font-lock-variable-use-face))
     ((arithmetic_variable) @font-lock-variable-use-face
      (:pred sh-ts-mode--pattern-interior-p
             @font-lock-variable-use-face))
     ([(escaped_character)
       (dollar_single_quote_escape)
       (double_quote_escape)
       (here_document_escape)] @font-lock-escape-face
       (:pred sh-ts-mode--pattern-interior-p @font-lock-escape-face)))

   :feature 'operator
   '((assignment_word
      "=" @font-lock-operator-face)
     [(and_if)
      (arithmetic_operator)
      (bang)
      (clobber)
      (dgreat)
      (dless)
      (dlessdash)
      (dsemi)
      (greatand)
      (lessand)
      (lessgreat)
      (or_if)
      (parameter_length_operator)
      (parameter_pattern_operator)
      (parameter_value_operator)
      (semi_and)] @font-lock-operator-face
     (separator_op
      "&" @font-lock-operator-face)
     (pipe_sequence
      "|" @font-lock-operator-face)
     (io_file
      operator: ["<" ">"] @font-lock-operator-face))

   :feature 'punctuation
   '((separator_op
      ";" @font-lock-punctuation-face)
     (sequential_sep
      ";" @font-lock-punctuation-face)
     (line_continuation) @font-lock-punctuation-face
     (command_substitution
      "$" @font-lock-punctuation-face)
     (arithmetic_expansion
      "$" @font-lock-punctuation-face))

   :feature 'bracket
   '((parameter_expansion
      ["{" "}"] @font-lock-bracket-face)
     (command_substitution
      ["(" ")"] @font-lock-bracket-face)
     (arithmetic_expansion
      ["(" ")"] @font-lock-bracket-face)
     (function_definition
      ["(" ")"] @font-lock-bracket-face)
     (pattern_list
      "(" @font-lock-bracket-face)
     (case_item
      ")" @font-lock-bracket-face)
     (case_item_ns
      ")" @font-lock-bracket-face)
     (brace_group
      ["{" "}"] @font-lock-bracket-face)
     (subshell
      ["(" ")"] @font-lock-bracket-face)
     (parenthesized_arithmetic
      ["(" ")"] @font-lock-bracket-face)
     (parenthesized_arithmetic_source
      ["(" ")"] @font-lock-bracket-face)
     (parenthesized_arithmetic_dynamic_source
      ["(" ")"] @font-lock-bracket-face))

   :feature 'punctuation
   :override t
   '(((line_continuation) @font-lock-punctuation-face
      (:pred sh-ts-mode--continued-lexical-token-p
             @font-lock-punctuation-face)))))

;;;;; Setup

(defun sh-ts-mode-font-lock--setup ()
  "Configure font lock for the current buffer."
  (setq-local treesit-font-lock-feature-list
              sh-ts-mode-font-lock--feature-list)
  (setq-local treesit-font-lock-settings
              (sh-ts-mode-font-lock--settings)))

;;;; Navigation

(defconst sh-ts-mode-navigation--function-definition-regexp
  "^function_definition$"
  "Regexp matching POSIX sh function definitions.")

(defconst sh-ts-mode-navigation--settings
  `((sh
     (sexp ,(rx string-start
                (or "word" "assignment_word" "name" "fname"
                    "single_quoted" "double_quoted" "dollar_single_quoted"
                    "parameter_expansion" "arithmetic_expansion"
                    "command_substitution" "backquote_substitution"
                    "compound_command" "function_definition")
                string-end))
     (defun ,sh-ts-mode-navigation--function-definition-regexp)))
  "Tree-sitter thing definitions for POSIX sh.")

(defun sh-ts-mode-navigation--setup ()
  "Configure navigation for the current buffer."
  (setq-local treesit-thing-settings
              sh-ts-mode-navigation--settings))

;;;; Imenu

(defun sh-ts-mode-imenu--name (node)
  "Return the source name of NODE, or nil if it has no name."
  (when (treesit-node-match-p
         node sh-ts-mode-navigation--function-definition-regexp)
    (let ((name (treesit-node-child-by-field-name node "name")))
      (when (and name (equal (treesit-node-type name) "fname"))
        (treesit-node-text name t)))))

(defconst sh-ts-mode-imenu--settings
  `(("Function" ,sh-ts-mode-navigation--function-definition-regexp nil nil))
  "Tree-sitter Imenu settings for POSIX sh.")

(defun sh-ts-mode-imenu--setup ()
  "Configure Imenu for the current buffer."
  (setq-local treesit-defun-name-function
              #'sh-ts-mode-imenu--name)
  (setq-local treesit-simple-imenu-settings
              sh-ts-mode-imenu--settings))

;;;; Indentation

(defcustom sh-ts-mode-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'natnum
  :group 'sh-ts)

(defconst sh-ts-mode-indent--rules
  '((sh
     ((and (node-is "}") (parent-is "brace_group")) standalone-parent 0)
     ((and (node-is ")")
           (or (parent-is "subshell") (parent-is "command_substitution")))
      standalone-parent 0)
     ((node-is "then_keyword") parent-bol 0)
     ((node-is "else_part") parent-bol 0)
     ((node-is "fi_keyword") parent-bol 0)
     ((node-is "do_group") standalone-parent 0)
     ((node-is "done_keyword") parent-bol 0)
     ((node-is "esac_keyword") parent-bol 0)
     ((node-is "case_list") parent-bol sh-ts-mode-indent-offset)
     ((node-is "case_item") parent-bol 0)
     ((field-is "terminator") parent-bol sh-ts-mode-indent-offset)
     ((lambda (_node parent _bol)
        (and (equal (treesit-node-type parent) "newline_list")
             (equal (treesit-node-type
                     (treesit-node-parent (treesit-node-parent parent)))
                    "compound_list")))
      standalone-parent sh-ts-mode-indent-offset)
     ((parent-is "compound_list") standalone-parent sh-ts-mode-indent-offset)
     ((parent-is "term") first-sibling 0)
     ((parent-is "and_or") parent-bol sh-ts-mode-indent-offset)
     ((parent-is "pipe_sequence") parent-bol sh-ts-mode-indent-offset)
     ((parent-is "program") column-0 0)
     ((parent-is "complete_commands") column-0 0)))
  "Tree-sitter indentation rules for POSIX sh.")

(defun sh-ts-mode-indent--setup ()
  "Configure indentation for the current buffer."
  (setq-local treesit-simple-indent-rules
              sh-ts-mode-indent--rules))

;;;; Mode

(defun sh-ts-mode--setup ()
  "Configure `sh-ts-mode' in the current buffer."
  (sh-ts-mode--ensure-grammar 'sh)
  (setq-local treesit-primary-parser (treesit-parser-create 'sh))
  (sh-ts-mode-syntax--setup)
  (sh-ts-mode-electric-pair--setup)
  (sh-ts-mode-font-lock--setup)
  (sh-ts-mode-navigation--setup)
  (sh-ts-mode-imenu--setup)
  (sh-ts-mode-indent--setup)
  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode sh-ts-mode prog-mode "Sh-TS"
  "Major mode for editing POSIX sh."
  :syntax-table sh-ts-mode-syntax-table
  :group 'sh-ts
  (sh-ts-mode--setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-ts-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("sh" . sh-ts-mode))

(provide 'sh-ts-mode)

;;; sh-ts-mode.el ends here
