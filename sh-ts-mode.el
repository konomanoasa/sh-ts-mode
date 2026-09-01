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

(defgroup sh-ts nil
  "Tree-sitter mode for POSIX sh."
  :group 'languages)

(defconst sh-ts-mode--grammar-sources
  '((sh "https://github.com/konomanoasa/tree-sitter-sh"
        :revision "v0.7.0"))
  "Tree-sitter grammar sources for POSIX sh.")

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

(defconst sh-ts-mode--pathname-pattern-types
  '("pattern_bracket_source" "pattern_question_source"
    "pattern_star_source")
  "Node types that identify pathname patterns.")

(defconst sh-ts-mode--pattern-scope-types
  (append sh-ts-mode--pattern-context-types
          sh-ts-mode--pattern-boundary-types)
  "Node types whose nested patterns belong to another pattern scope.")

(defun sh-ts-mode--ancestor-state (node inside-types outside-types)
  "Return whether NODE enters INSIDE-TYPES before OUTSIDE-TYPES."
  (let (state)
    (while (and node (not state))
      (let ((type (treesit-node-type node)))
        (cond
         ((member type inside-types) (setq state 'inside))
         ((member type outside-types) (setq state 'outside))))
      (setq node (treesit-node-parent node)))
    (eq state 'inside)))

(defun sh-ts-mode--active-pattern-p (node)
  "Return non-nil when NODE is in an active shell pattern."
  (sh-ts-mode--ancestor-state
   node sh-ts-mode--pattern-context-types
   sh-ts-mode--pattern-boundary-types))

(defun sh-ts-mode--pathname-pattern-word-p (word)
  "Return non-nil when WORD contains a pathname pattern."
  (let ((owner (treesit-node-parent word))
        pending found)
    (when (and owner
               (member (treesit-node-type owner)
                       sh-ts-mode--pathname-owner-types))
      (setq pending (list word))
      (while (and pending (not found))
        (let* ((node (pop pending))
               (type (treesit-node-type node)))
          (cond
           ((member type sh-ts-mode--pathname-pattern-types)
            (setq found t))
           ((member type sh-ts-mode--pattern-scope-types))
           (t
            (let ((index (1- (treesit-node-child-count node t))))
              (while (>= index 0)
                (push (treesit-node-child node index t) pending)
                (setq index (1- index)))))))))
    found))

(defun sh-ts-mode--pathname-pattern-p (node)
  "Return non-nil when NODE is inside a pathname pattern word."
  (let (result)
    (while node
      (let ((parent (treesit-node-parent node))
            (type (treesit-node-type node)))
        (cond
         ((member type sh-ts-mode--pattern-boundary-types)
          (setq node nil))
         ((and parent
               (equal type "word")
               (member (treesit-node-type parent)
                       sh-ts-mode--pathname-owner-types))
          (setq result (sh-ts-mode--pathname-pattern-word-p node)
                node nil))
         (t (setq node parent)))))
    result))

(defun sh-ts-mode--pattern-interior-p (node)
  "Return non-nil when NODE is inside any shell pattern."
  (or (sh-ts-mode--active-pattern-p node)
      (sh-ts-mode--pathname-pattern-p node)))

(defun sh-ts-mode--outside-pattern-interior-p (node)
  "Return non-nil when NODE is outside every shell pattern."
  (not (sh-ts-mode--pattern-interior-p node)))

(defun sh-ts-mode--plain-literal-p (node)
  "Return non-nil when NODE is a plain literal."
  (let* ((parent (treesit-node-parent node))
         (owner (and parent (treesit-node-parent parent))))
    (and (sh-ts-mode--outside-pattern-interior-p node)
         (not (and parent
                   (equal (treesit-node-type parent) "tilde_user")))
         (not (and parent owner
                   (equal (treesit-node-type parent) "word")
                   (member (treesit-node-type owner)
                           '("cmd_name" "cmd_word")))))))

;;;; Syntax

(defvar sh-ts-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (dolist (character '(?# ?\" ?\\ ?\( ?\) ?\[ ?\] ?{ ?}))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `sh-ts-mode'.")

(defvar sh-ts-mode-syntax--query-cache nil
  "Cached syntax query.")

;;;;; Syntax Queries

(defun sh-ts-mode-syntax--query ()
  "Return the cached syntax query."
  (or sh-ts-mode-syntax--query-cache
      (setq sh-ts-mode-syntax--query-cache
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
                ["(" ")"] @delimiter))
             t))))

(defun sh-ts-mode-syntax--captures (parser &optional start end)
  "Return syntax captures from PARSER between START and END."
  (with-current-buffer (treesit-parser-buffer parser)
    (save-restriction
      (widen)
      (let ((start (or start (point-min)))
            (end (or end (point-max)))
            (query (sh-ts-mode-syntax--query))
            captures)
        (dolist (capture
                 (treesit-query-capture
                  (treesit-parser-root-node parser)
                  query start end))
          (let ((node (cdr capture)))
            (push (list (car capture)
                        (treesit-node-start node)
                        (treesit-node-end node))
                  captures)))
        (sort captures
              (lambda (left right) (< (nth 1 left) (nth 1 right))))))))

;;;;; Propertization

(defun sh-ts-mode-syntax--delimiter-syntax (position)
  "Return syntax-table syntax for the delimiter at POSITION."
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
        (remove-text-properties (point-min) start '(syntax-table nil))
        (setq start (point-min))
        (syntax-ppss-flush-cache start))
      (dolist (capture (sh-ts-mode-syntax--captures
                        treesit-primary-parser start end))
        (let* ((name (car capture))
               (position (if (eq name 'comment)
                             (nth 1 capture)
                           (1- (nth 2 capture)))))
          (put-text-property
           position (1+ position) 'syntax-table
           (if (eq name 'comment)
               (string-to-syntax "<")
             (sh-ts-mode-syntax--delimiter-syntax position))))))))

;;;;; Setup

(defun sh-ts-mode-syntax-setup ()
  "Configure syntax handling for the current buffer."
  (setq-local syntax-propertize-function
              #'sh-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#[[:blank:]]*")
  (setq-local comment-use-syntax t))

;;;; Font Lock

;;;;; Features

(defconst sh-ts-mode-font-lock--feature-list
  '((comment)
    (keyword function command string)
    (number constant variable escape)
    (pattern operator punctuation bracket))
  "Font-lock features by decoration level.")

;;;;; Settings

(defun sh-ts-mode-font-lock--settings ()
  "Return the font-lock settings."
  (treesit-font-lock-rules
   :default-language 'sh

   :feature 'comment
   '((comment) @font-lock-comment-face)

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
      (:pred sh-ts-mode--plain-literal-p
             @font-lock-string-face))
     ((single_quoted
       "'" @font-lock-string-face)
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-string-face))
     ((double_quoted
       "\"" @font-lock-string-face)
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-string-face))
     ((dollar_single_quoted
       ["$'" "'"] @font-lock-string-face)
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
     (here_document_end) @font-lock-string-face)

   :feature 'number
   '(([(arithmetic_number) (io_number)] @font-lock-number-face
      (:pred sh-ts-mode--outside-pattern-interior-p
             @font-lock-number-face)))

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
       parameter: [(variable_name)
                   (positional_parameter)
                   (special_parameter)])
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
   '(([(pattern_bracket_character_source)
       (pattern_bracket_hyphen_source)
       (pattern_character_class_content_source)
       (pattern_collating_symbol_character_source)
       (pattern_equivalence_class_character_source)
       (pattern_question_source)
       (pattern_star_source)] @font-lock-constant-face
      (:pred sh-ts-mode--pattern-interior-p @font-lock-constant-face))
     ((pattern_bracket_source
       ["[" "]"] @font-lock-bracket-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-bracket-face))
     ((pattern_bracket_negation_source
       "!" @font-lock-negation-char-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-negation-char-face))
     ((pattern_bracket_range_operator_source) @font-lock-operator-face
      (:pred sh-ts-mode--pattern-interior-p @font-lock-operator-face))
     ((pattern_character_class_source
       ["[" "]"] @font-lock-bracket-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-bracket-face))
     ((pattern_character_class_source
       ":" @font-lock-punctuation-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-punctuation-face))
     ((pattern_collating_symbol_source
       ["[" "]"] @font-lock-bracket-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-bracket-face))
     ((pattern_collating_symbol_source
       "." @font-lock-punctuation-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-punctuation-face))
     ((pattern_equivalence_class_source
       ["[" "]"] @font-lock-bracket-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-bracket-face))
     ((pattern_equivalence_class_source
       "=" @font-lock-punctuation-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-punctuation-face))
     ((pattern_list
       "|" @font-lock-operator-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-operator-face)))

   :feature 'pattern
   '(((literal) @font-lock-string-face
      (:pred sh-ts-mode--pattern-interior-p @font-lock-string-face))
     ((single_quoted
       "'" @font-lock-string-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-string-face))
     ((double_quoted
       "\"" @font-lock-string-face)
      (:pred sh-ts-mode--pattern-interior-p @font-lock-string-face))
     ((dollar_single_quoted
       ["$'" "'"] @font-lock-string-face)
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
       parameter: [(variable_name)
                   (positional_parameter)
                   (special_parameter)])
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
      ["(" ")"] @font-lock-bracket-face))))

;;;;; Setup

(defun sh-ts-mode-font-lock-setup ()
  "Configure font locking for the current buffer."
  (setq-local treesit-font-lock-feature-list
              sh-ts-mode-font-lock--feature-list)
  (setq-local treesit-font-lock-settings
              (sh-ts-mode-font-lock--settings)))

;;;; Navigation

(defconst sh-ts-mode--function-definition-regexp
  "^function_definition$"
  "Regexp matching POSIX sh function definitions.")

(defconst sh-ts-mode-thing-settings
  `((sh
     (defun ,sh-ts-mode--function-definition-regexp)))
  "Tree-sitter thing definitions for POSIX sh.")

(defun sh-ts-mode-navigation-setup ()
  "Configure navigation for the current buffer."
  (setq-local treesit-thing-settings sh-ts-mode-thing-settings))

;;;; Imenu

(defconst sh-ts-mode-imenu-settings
  `((nil ,sh-ts-mode--function-definition-regexp nil nil))
  "Tree-sitter Imenu settings for POSIX sh.")

(defun sh-ts-mode--defun-name (node)
  "Return the name of the function definition NODE."
  (when (treesit-node-match-p
         node sh-ts-mode--function-definition-regexp)
    (let ((name (treesit-node-child-by-field-name node "name")))
      (when (and name (equal (treesit-node-type name) "fname"))
        (treesit-node-text name t)))))

(defun sh-ts-mode-imenu-setup ()
  "Configure Imenu for the current buffer."
  (setq-local treesit-defun-name-function #'sh-ts-mode--defun-name)
  (setq-local treesit-simple-imenu-settings sh-ts-mode-imenu-settings))

;;;; Indentation

(defcustom sh-ts-mode-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'natnum
  :group 'sh-ts)

(defconst sh-ts-mode-indent-rules
  '((sh
     ((node-is "}") standalone-parent 0)
     ((node-is ")") standalone-parent 0)
     ((node-is "then_keyword") parent-bol 0)
     ((node-is "else_part") parent-bol 0)
     ((node-is "fi_keyword") parent-bol 0)
     ((node-is "do_group") standalone-parent 0)
     ((node-is "done_keyword") parent-bol 0)
     ((node-is "esac_keyword") parent-bol 0)
     ((node-is "case_list") parent-bol sh-ts-mode-indent-offset)
     ((node-is "case_item") parent-bol 0)
     ((field-is "terminator") parent-bol sh-ts-mode-indent-offset)
     ((parent-is "compound_list") standalone-parent sh-ts-mode-indent-offset)
     ((parent-is "term") first-sibling 0)
     ((parent-is "and_or") parent-bol sh-ts-mode-indent-offset)
     ((parent-is "pipe_sequence") parent-bol sh-ts-mode-indent-offset)
     ((parent-is "program") column-0 0)
     ((parent-is "complete_commands") column-0 0)))
  "Tree-sitter indentation rules for POSIX sh.")

(defun sh-ts-mode-indent-setup ()
  "Configure indentation for the current buffer."
  (setq-local treesit-simple-indent-rules
              sh-ts-mode-indent-rules))

;;;; Mode

(defun sh-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language sh-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (treesit-ensure-installed language)))

(defun sh-ts-mode--setup ()
  "Configure `sh-ts-mode' in the current buffer."
  (unless (sh-ts-mode--ensure-grammar 'sh)
    (user-error "Tree-sitter grammar `sh' is unavailable"))
  (setq-local treesit-primary-parser (treesit-parser-create 'sh))
  (sh-ts-mode-syntax-setup)
  (sh-ts-mode-font-lock-setup)
  (sh-ts-mode-navigation-setup)
  (sh-ts-mode-imenu-setup)
  (sh-ts-mode-indent-setup)
  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode sh-ts-mode prog-mode "Sh-TS"
  "Major mode for editing POSIX sh."
  :syntax-table sh-ts-mode-syntax-table
  :group 'sh-ts
  (sh-ts-mode--setup))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("sh" . sh-ts-mode))

(provide 'sh-ts-mode)

;;; sh-ts-mode.el ends here
