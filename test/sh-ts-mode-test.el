;;; sh-ts-mode-test.el --- Tests for sh-ts-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 konomanoasa
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

;;; Code:

(require 'ert)
(require 'imenu)
(require 'loaddefs-gen)
(require 'newcomment)
(require 'sh-ts-mode)

(dolist (language '(sh))
  (unless (treesit-ready-p language t)
    (error "The %s grammar is required to run the tests" language)))

;;;; Helpers

(defun sh-ts-mode-test--position (fragment &optional line)
  (save-excursion
    (goto-char (point-min))
    (when line
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (if (equal line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
              (setq found t)
            (forward-line 1)))
        (unless found (ert-fail (format "Missing fixture line: %S" line)))))
    (unless (search-forward fragment (and line (line-end-position)) t)
      (ert-fail (format "Missing fixture fragment: %S" fragment)))
    (- (point) (length fragment))))

(defun sh-ts-mode-test--face (fragment &optional offset line)
  (get-text-property (+ (sh-ts-mode-test--position fragment line)
                        (or offset 0)) 'face))

(defun sh-ts-mode-test--comment-p (fragment &optional offset line)
  (syntax-propertize (point-max))
  (nth 4 (syntax-ppss (+ (sh-ts-mode-test--position fragment line)
                         (or offset 0)))))

(defun sh-ts-mode-test--syntax-class (fragment &optional offset line)
  (syntax-propertize (point-max))
  (syntax-class (syntax-after (+ (sh-ts-mode-test--position fragment line)
                                 (or offset 0)))))

(defun sh-ts-mode-test--should-have-faces (cases)
  (pcase-dolist (`(,line ,fragment ,face) cases)
    (ert-info ((format "%S: %S" line fragment))
      (should (eq (sh-ts-mode-test--face fragment nil line) face)))))

(defun sh-ts-mode-test--indent (source &optional offset)
  (with-temp-buffer
    (insert source)
    (sh-ts-mode)
    (setq-local indent-tabs-mode nil)
    (when offset
      (setq-local sh-ts-mode-indent-offset offset))
    (indent-region (point-min) (point-max))
    (let ((indented (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) indented))
      indented)))

(defun sh-ts-mode-test--buffer-state ()
  (font-lock-ensure)
  (syntax-propertize (point-max))
  (let (state)
    (dotimes (offset (- (point-max) (point-min)))
      (let ((position (+ (point-min) offset)))
        (push (list (get-text-property position 'face) (syntax-after position)) state)))
    (nreverse state)))

(defun sh-ts-mode-test--should-match-fresh-buffer (level)
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (state (sh-ts-mode-test--buffer-state))
        (file buffer-file-name))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (let ((treesit-font-lock-level level)) (sh-ts-mode))
      (should (equal state (sh-ts-mode-test--buffer-state))))))

;;;; Grammar

(ert-deftest sh-ts-mode-respects-grammar-sources ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)) received)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (language)
                  (setq received (assq language treesit-language-source-alist))
                  t))
          (dolist (source sh-ts-mode--grammar-sources)
            (let* ((language (car source))
                   (custom (list language "/local/grammar" :revision "custom")))
              (dolist (configured (list nil (list custom)))
                (let ((treesit-language-source-alist configured))
                  (should (sh-ts-mode--ensure-grammar language))
                  (should (equal received (if configured custom source)))
                  (should (eq treesit-language-source-alist configured)))))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest sh-ts-mode-reports-unavailable-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)))
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed (lambda (_language) nil))
          (with-temp-buffer
            (let ((buffer-file-name nil))
              (should-error (sh-ts-mode) :type 'user-error)
              (should-not (treesit-parser-list)))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest sh-ts-mode-starts-and-reuses-parser ()
  (with-temp-buffer
    (insert "printf hello\n")
    (sh-ts-mode)
    (should (eq major-mode 'sh-ts-mode))
    (should (eq (treesit-parser-language treesit-primary-parser) 'sh))
    (should (equal (treesit-node-type (treesit-parser-root-node treesit-primary-parser))
                   "program"))
    (sh-ts-mode)
    (should (equal (treesit-parser-list) (list treesit-primary-parser)))))

;;;; Mode Selection

(ert-deftest sh-ts-mode-selects-files ()
  (let ((original (copy-tree auto-mode-alist)))
    (pcase-dolist (`(,file ,prefix ,expected)
                   '(("/tmp/script.sh" "" t)
                     ("/tmp/script.sh.gz" "" t)
                     ("/tmp/script.sh.extra" "" nil)
                     ("/tmp/script.bash" "" nil)
                     ("/tmp/script.sh" "# comment\n" t)
                     ("/tmp/script.sh" "#!/bin/bash\n" nil)
                     ("/tmp/script.sh" "#!/bin/zsh\n" nil)
                     ("/tmp/script.sh" "#!/usr/bin/env bash\n" nil)
                     ("/tmp/script.sh" "#!/usr/bin/env -S bash -e\n" nil)
                     ("/tmp/script.sh" "#!/usr/bin/unknown\n" nil)
                     ("/tmp/script.sh" "#!/usr/bin/env\n" nil)
                     ("/tmp/script.sh" "#!\n" nil)))
      (ert-info ((format "%s: %S" file prefix))
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert prefix "printf hello\n")
          (set-auto-mode)
          (should (eq (eq major-mode 'sh-ts-mode) expected)))))
    (should (equal auto-mode-alist original))))

(ert-deftest sh-ts-mode-selects-interpreters ()
  (should (equal (alist-get "sh" interpreter-mode-alist nil nil #'equal)
                 'sh-ts-mode))
  (dolist (file '("/tmp/example" "/tmp/example.sh"))
    (dolist (shebang '("#!/bin/sh\n"
                       "#!/bin/sh -e\n"
                       "#!/usr/bin/env sh\n"
                       "#!/usr/bin/env -S sh -e\n"))
      (with-temp-buffer
        (setq buffer-file-name file)
        (insert shebang "printf '%s\\n' hello\n")
        (set-auto-mode)
        (should (eq major-mode 'sh-ts-mode))))))

(ert-deftest sh-ts-mode-generates-autoloads ()
  (let ((output (make-temp-file "sh-ts-mode-loaddefs-"))
        (directory (file-name-directory (locate-library "sh-ts-mode"))))
    (unwind-protect
        (progn
          (loaddefs-generate directory output nil nil nil t)
          (with-temp-buffer
            (insert-file-contents output)
            (dolist (form '("(autoload 'sh-ts-mode"
                            "(autoload 'sh-ts-mode--auto-mode"
                            "(add-to-list 'auto-mode-alist"
                            "(add-to-list 'interpreter-mode-alist"))
              (goto-char (point-min))
              (should (search-forward form nil t)))))
      (delete-file output))))

;;;; Syntax

(ert-deftest sh-ts-mode-classifies-delimiters ()
  (with-temp-buffer
    (let ((function "f() {")
          (literal "  printf '%s\\n' '()[]{}' {fd}>out")
          (expansions
           "  printf '%s\\n' \"${x#[[:alpha:]]} $((1 + (2))) $(printf x)\"")
          (closing "}")
          (parenthesized-case "  (foo) :;;")
          (plain-case "  bar) :"))
      (insert function "\n" literal "\n" expansions "\n" closing "\n"
              "case x in\n" parenthesized-case "\n" plain-case "\nesac\n")
      (sh-ts-mode)
      (dolist (pair '((?\( . ?\)) (?\[ . ?\]) (?{ . ?})))
        (should (eq (matching-paren (car pair)) (cdr pair))))
      (dolist (expectation
               `((,function "(" 0 4)
                 (,function ")" 0 5)
                 (,function "{" 0 4)
                 (,expansions "${" 0 1)
                 (,expansions "${" 1 4)
                 (,expansions "]]}" 2 5)
                 (,expansions "$((" 0 1)
                 (,expansions "$((" 1 4)
                 (,expansions "$((" 2 4)
                 (,expansions "(2)" 0 4)
                 (,expansions "(2)" 2 5)
                 (,expansions "2)))" 2 5)
                 (,expansions "2)))" 3 5)
                 (,expansions "$(printf" 0 1)
                 (,expansions "$(printf" 1 4)
                 (,expansions "x)\"" 1 5)
                 (,closing "}" 0 5)
                 (,parenthesized-case "(" 0 4)
                 (,parenthesized-case ")" 0 5)))
        (pcase-let ((`(,line ,fragment ,offset ,class) expectation))
          (should
           (= (sh-ts-mode-test--syntax-class fragment offset line)
              class))))
      (dolist (expectation
               `((,literal "()" 0)
                 (,literal "()" 1)
                 (,literal "[]" 0)
                 (,literal "[]" 1)
                 (,literal "{}" 0)
                 (,literal "{}" 1)
                 (,literal "{fd}" 0)
                 (,literal "{fd}" 3)
                 (,literal "'" 0)
                 (,literal "\\n" 0)
                 (,expansions "\"" 0)
                 (,expansions "[[" 0)
                 (,expansions "[[" 1)
                 (,expansions "]]" 0)
                 (,expansions "]]" 1)
                 (,plain-case ")" 0)))
        (pcase-let ((`(,line ,fragment ,offset) expectation))
          (should
           (= (sh-ts-mode-test--syntax-class fragment offset line)
              1)))))))

(ert-deftest sh-ts-mode-classifies-comments ()
  (with-temp-buffer
    (let ((lines
           '("#!/bin/sh"
             "# note"
             "  # indented"
             "printf x;# trailing"
             "printf value#suffix"
             "printf '%s\\n' '# single' \"# double\" \\#escaped"
             "printf '%s\\n' '\"' # after-quote"
             "printf '%s\\n' \"$#\""
             "trimmed=${value#pattern}"
             "longest=${value##pattern}"
             "cat <<EOF"
             "# here-body"
             "EOF"
             "cat <<'QUOTED'"
             "# quoted-here-body"
             "QUOTED"
             "value=\"$(printf one"
             "# in-substitution"
             ")\""
             "# backslash \\"
             "printf next"
             "printf continued \\"
             "# after-continuation"
             "after"
             "echo \"`a # folded \\"
             "continued"
             "`\""
             ": tail")))
      (insert (mapconcat #'identity lines "\n") "\n")
      (sh-ts-mode)
      (dolist (expectation
               '(("#!/bin/sh" "!/bin/sh")
                 ("# note" "note")
                 ("  # indented" "indented")
                 ("printf x;# trailing" "trailing")
                 ("printf '%s\\n' '\"' # after-quote" "after-quote")
                 ("# in-substitution" "in-substitution")
                 ("# backslash \\" "backslash")
                 ("# after-continuation" "after-continuation")
                 ("echo \"`a # folded \\" "folded")
                 ("continued" "continued")))
        (pcase-let ((`(,line ,fragment) expectation))
          (should (sh-ts-mode-test--comment-p fragment nil line))))
      (dolist (expectation
               '(("printf value#suffix" "suffix" 0)
                 ("printf '%s\\n' '# single' \"# double\" \\#escaped"
                  "# single" 1)
                 ("printf '%s\\n' '# single' \"# double\" \\#escaped"
                  "# double" 1)
                 ("printf '%s\\n' '# single' \"# double\" \\#escaped"
                  "#escaped" 1)
                 ("printf '%s\\n' \"$#\"" "#" 1)
                 ("trimmed=${value#pattern}" "#" 1)
                 ("longest=${value##pattern}" "#" 1)
                 ("# here-body" "here-body" 0)
                 ("# quoted-here-body" "quoted-here-body" 0)
                 ("printf next" "next" 0)
                 ("after" "after" 0)
                 (": tail" "tail" 0)))
        (pcase-let ((`(,line ,fragment ,offset) expectation))
          (should-not
           (sh-ts-mode-test--comment-p fragment offset line)))))))

;;;; Comment Commands

(ert-deftest sh-ts-mode-comments-and-uncomments ()
  (with-temp-buffer
    (insert "printf hello\n")
    (sh-ts-mode)
    (comment-region (point-min) (point-max))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "# printf hello\n"))
    (uncomment-region (point-min) (point-max))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "printf hello\n"))))

(ert-deftest sh-ts-mode-uncomments-multiline-source ()
  (dolist (case
           '(("cat <<EOF\nhello\nEOF\n" "# cat <<EOF\n# hello\n# EOF\n")
             ("printf 'one\n# two'\n" "# printf 'one\n# # two'\n")))
    (with-temp-buffer
      (insert (car case))
      (sh-ts-mode)
      (let ((state (sh-ts-mode-test--buffer-state)))
        (comment-or-uncomment-region (point-min) (point-max))
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       (cadr case)))
        (comment-or-uncomment-region (point-min) (point-max))
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       (car case)))
        (should (equal (sh-ts-mode-test--buffer-state) state))))))

;;;; Electric Pair

(ert-deftest sh-ts-mode-supplies-electric-pairs ()
  (let ((electric-pair-pairs '((?% . ?%)))
        (electric-pair-mode nil))
    (pcase-dolist (`(,prefix ,opening ,expected)
                   '(("" ?{ "{}")
                     ("" ?\( "()")
                     ("printf " ?\[ "printf []")))
      (ert-info ((format "%S / %c" prefix opening))
        (with-temp-buffer
          (insert prefix)
          (sh-ts-mode)
          (should-not electric-pair-mode)
          (should (local-variable-p 'electric-pair-pairs))
          (should (equal (assq ?% electric-pair-pairs) '(?% . ?%)))
          (electric-pair-local-mode 1)
          (let ((last-command-event opening)) (self-insert-command 1))
          (should (equal (buffer-string) expected))
          (should (= (point) (1- (point-max)))))))
    (should (equal electric-pair-pairs '((?% . ?%)))))
  (let ((electric-pair-pairs '((?{ . ?>))))
    (pcase-dolist (`(,prefix ,opening ,expected)
                   '(("" ?{ "{>")))
      (with-temp-buffer
        (insert prefix)
        (sh-ts-mode)
        (electric-pair-local-mode 1)
        (let ((last-command-event opening)) (self-insert-command 1))
        (should (equal (buffer-string) expected))
        (should (= (point) (1- (point-max))))))
    (should (equal electric-pair-pairs '((?{ . ?>))))))

(ert-deftest sh-ts-mode-respects-pair-newline-preferences ()
  (let ((calls 0))
    (pcase-dolist (`(,setting ,expected)
                   (list (list nil "{\n}")
                         (list t "{\n\n}")
                         (list (lambda () (setq calls (1+ calls)) nil) "{\n}")
                         (list (lambda () (setq calls (1+ calls)) t) "{\n\n}")))
      (setq calls 0)
      (let ((electric-pair-open-newline-between-pairs setting))
        (with-temp-buffer
          (insert "{}")
          (sh-ts-mode)
          (electric-indent-local-mode -1)
          (electric-pair-local-mode 1)
          (goto-char 2)
          (call-interactively (key-binding (kbd "RET")))
          (should (equal (buffer-string) expected)))
        (should (eq (> calls 0) (functionp setting)))
        (should (eq electric-pair-open-newline-between-pairs setting))))))

(ert-deftest sh-ts-mode-pairs-delimiters-and-indents-on-return ()
  (pcase-dolist (`(,prefix ,suffix ,offset ,expand ,expected ,column)
                 '(("func() " "" 2 t "func() {\n  \n}" 2)
                   ("" "" 4 t "{\n    \n}" 4)
                   ("{\n  " "\n}" 2 t
                    "{\n  {\n    \n  }\n}" 4)
                   ("func() " "" 2 nil "func() {\n}" 0)))
    (ert-info ((format "Expand %S, offset %s: %S" expand offset prefix))
      (let ((electric-pair-open-newline-between-pairs expand))
        (with-temp-buffer
          (insert prefix suffix)
          (sh-ts-mode)
          (setq-local indent-tabs-mode nil)
          (setq-local sh-ts-mode-indent-offset offset)
          (electric-indent-local-mode 1)
          (electric-pair-local-mode 1)
          (goto-char (1+ (length prefix)))
          (let ((last-command-event ?{))
            (self-insert-command 1))
          (call-interactively (key-binding (kbd "RET")))
          (should (equal (buffer-string) expected))
          (should (= (current-column) column))
          (when expand
            (should (eolp))
            (should (= (line-number-at-pos) (1+ (length (split-string prefix "\n")))))))))))

(ert-deftest sh-ts-mode-restricts-pair-newlines-to-cst-contexts ()
  (pcase-dolist (`(,before ,after ,expected)
                 '(("{" "}" "{\n\n}")
                   ("func() {" "}" "func() {\n\n}")
                   ("(" ")" "(\n\n)")
                   ("printf $(" ")" "printf $(\n\n)")
                   ("printf ${name:-{" "}}" "printf ${name:-{\n}}")
                   ("printf ${" "name}" "printf ${\nname}")
                   ("func(" ") { :; }" "func(\n) { :; }")
                   ("case x in (" ") :;; esac" "case x in (\n) :;; esac")
                   ("printf [" "]" "printf [\n]")
                   ("printf '{" "}'" "printf '{\n}'")
                   ("printf \"(" ")\"" "printf \"(\n)\"")
                   ("# {" "}" "# {\n}")
                   ("cat <<EOF\n{" "}\nEOF\n" "cat <<EOF\n{\n}\nEOF\n")))
    (ert-info ((format "%S / %S" before after))
      (let ((electric-pair-open-newline-between-pairs t))
        (with-temp-buffer
          (insert before after)
          (sh-ts-mode)
          (electric-indent-local-mode -1)
          (electric-pair-local-mode 1)
          (goto-char (1+ (length before)))
          (call-interactively (key-binding (kbd "RET")))
          (should (equal (buffer-substring-no-properties (point-min) (point-max))
                         expected)))))))

(ert-deftest sh-ts-mode-pairs-language-quotes ()
  (let ((electric-pair-pairs nil)
        (electric-pair-text-pairs nil))
    (pcase-dolist (`(,prefix ,quote ,opened ,closed)
                   '(("printf " ?\" "printf \"\"" "printf \"x\"")
                     ("printf " ?\' "printf ''" "printf 'x'")
                     ("printf " ?` "printf ``" "printf `x`")))
      (with-temp-buffer
        (insert prefix)
        (sh-ts-mode)
        (electric-pair-local-mode 1)
        (let ((last-command-event quote)) (self-insert-command 1))
        (should (equal (buffer-string) opened))
        (should (= (point) (1- (point-max))))
        (let ((last-command-event ?x)) (self-insert-command 1))
        (let ((last-command-event quote)) (self-insert-command 1))
        (should (equal (buffer-string) closed))
        (should (eobp))))
    (should-not electric-pair-pairs)))

;;;; Font Lock

(ert-deftest sh-ts-mode-fontifies-by-level ()
  (dolist (level '(1 2 3 4))
    (with-temp-buffer
      (insert "# note\nif printf 'text' \"$value\"; then :; fi\n")
      (let ((treesit-font-lock-level level)) (sh-ts-mode))
      (font-lock-ensure)
      (pcase-dolist (`(,fragment ,minimum ,face)
                     '(("note" 1 font-lock-comment-face) ("if" 2 font-lock-keyword-face)
                       ("text" 2 font-lock-string-face) ("value" 3 font-lock-variable-use-face)
                       (";" 4 font-lock-punctuation-face)))
        (ert-info ((format "Level %s: %S" level fragment))
          (should (eq (sh-ts-mode-test--face fragment) (and (>= level minimum) face))))))))

(ert-deftest sh-ts-mode-fontifies-language-syntax ()
  (with-temp-buffer
    (let ((assignment "value=hello")
          (definition "worker() {")
          (conditional "  if printf '$value' 2>output; then")
          (background "    printf one &")
          (arithmetic "    result=$((value + 2))")
          (redirection "    exec {saved}>&1"))
      (insert "# note\n"
              assignment "\n"
              definition "\n"
              conditional "\n"
              background "\n"
              arithmetic "\n"
              redirection "\n"
              "  fi\n"
              "}\n")
      (let ((treesit-font-lock-level 4)) (sh-ts-mode))
      (font-lock-ensure)
      (sh-ts-mode-test--should-have-faces
       `(("# note" "#" font-lock-comment-face)
         (,assignment "value" font-lock-variable-name-face)
         (,assignment "=" font-lock-operator-face)
         (,assignment "hello" font-lock-string-face)
         (,definition "worker" font-lock-function-name-face)
         (,definition "(" font-lock-bracket-face)
         (,definition "{" font-lock-bracket-face)
         (,conditional "if" font-lock-keyword-face)
         (,conditional "printf" font-lock-function-call-face)
         (,conditional "'" font-lock-string-face)
         (,conditional "$value" font-lock-string-face)
         (,conditional "2" font-lock-number-face)
         (,conditional ">" font-lock-operator-face)
         (,conditional ";" font-lock-punctuation-face)
         (,background "&" font-lock-operator-face)
         (,arithmetic "$" font-lock-punctuation-face)
         (,arithmetic "value" font-lock-variable-use-face)
         (,arithmetic "+" font-lock-operator-face)
         (,arithmetic "2" font-lock-number-face)
         (,redirection "{saved}" font-lock-string-face)
         (,redirection ">&" font-lock-operator-face)
         ("  fi" "fi" font-lock-keyword-face))))))

(ert-deftest sh-ts-mode-fontifies-parameters-and-expansion-syntax ()
  (with-temp-buffer
    (let ((variable "printf \"$name ${name}\"")
          (line "value=$(printf \"${1:-$?} $((2 + (3)))\")")
          (unclassified "printf \"${00}\"")
          (backquote "printf `printf \\${name}`"))
      (insert variable "\n" line "\n" unclassified "\n" backquote "\n")
      (let ((treesit-font-lock-level 4)) (sh-ts-mode))
      (font-lock-ensure)
      (should (eq (sh-ts-mode-test--face "${" nil variable)
                  'font-lock-variable-use-face))
      (dolist (expectation '(("${" 1) ("}" 0)))
        (should (eq (sh-ts-mode-test--face (car expectation) (cadr expectation) variable)
                    'font-lock-bracket-face)))
      (should (eq (sh-ts-mode-test--face "name" nil variable)
                  'font-lock-variable-use-face))
      (should (eq (sh-ts-mode-test--face "$name" nil variable)
                  'font-lock-variable-use-face))
      (dolist (expectation '(("${1" 2) ("$?" 1)))
        (should (eq (sh-ts-mode-test--face (car expectation) (cadr expectation) line)
                    'font-lock-constant-face)))
      (should (eq (sh-ts-mode-test--face "$?" nil line)
                  'font-lock-variable-use-face))
      (should (eq (sh-ts-mode-test--face "${" nil line)
                  'font-lock-variable-use-face))
      (dolist (expectation '(("${" 1) ("}" 0)))
        (should (eq (sh-ts-mode-test--face (car expectation) (cadr expectation) line)
                    'font-lock-bracket-face)))
      (should (eq (sh-ts-mode-test--face "$(" nil line)
                  'font-lock-punctuation-face))
      (should (eq (sh-ts-mode-test--face "$((" nil line)
                  'font-lock-punctuation-face))
      (should (eq (sh-ts-mode-test--face "(3)" nil line)
                  'font-lock-bracket-face))
      (should-not (sh-ts-mode-test--face "${" nil unclassified))
      (dolist (expectation '(("${" 1) ("}" 0)))
        (should (eq (sh-ts-mode-test--face (car expectation) (cadr expectation) unclassified)
                    'font-lock-bracket-face)))
      (dolist (fragment '("{name}" "}`"))
        (should (eq (sh-ts-mode-test--face fragment nil backquote)
                    'font-lock-bracket-face)))
      (should (eq (sh-ts-mode-test--face "${name}" nil backquote)
                  'font-lock-variable-use-face))
      (should (eq (sh-ts-mode-test--face "name}" nil backquote)
                  'font-lock-variable-use-face)))))

(ert-deftest sh-ts-mode-fontifies-shell-pattern-syntax ()
  (with-temp-buffer
    (let ((line
           "  ([!a-c]|[-]|[[:alpha:]]|[[.x.]]|[[=x=]]|foo\\*) :;;"))
      (insert "case $value in\n" line "\nesac\n")
      (let ((treesit-font-lock-level 4)) (sh-ts-mode))
      (font-lock-ensure)
      (dolist (fragment '("[" "]"))
        (should (eq (sh-ts-mode-test--face fragment nil line)
                    'font-lock-bracket-face)))
      (should (eq (sh-ts-mode-test--face "!" nil line)
                  'font-lock-negation-char-face))
      (should (eq (sh-ts-mode-test--face "-" nil line)
                  'font-lock-operator-face))
      (dolist (expectation '(("[:" 0) (":]" 1)
                             ("[." 0) (".]" 1)
                             ("[=" 0) ("=]" 1)))
        (should (eq (sh-ts-mode-test--face (car expectation) (cadr expectation) line)
                    'font-lock-bracket-face)))
      (should (eq (sh-ts-mode-test--face "[-]" 1 line)
                  'font-lock-constant-face))
      (dolist (expectation '(("[:" 1) (":]" 0)
                             ("[." 1) (".]" 0)
                             ("[=" 1) ("=]" 0)))
        (should (eq (sh-ts-mode-test--face (car expectation) (cadr expectation) line)
                    'font-lock-punctuation-face)))
      (should (eq (sh-ts-mode-test--face "(" nil line)
                  'font-lock-bracket-face))
      (should (eq (sh-ts-mode-test--face ")" nil line)
                  'font-lock-bracket-face))
      (dolist (fragment '("a" "c" "alpha" "x"))
        (should (eq (sh-ts-mode-test--face fragment nil line)
                    'font-lock-constant-face)))
      (should (eq (sh-ts-mode-test--face "|" nil line)
                  'font-lock-operator-face))
      (should (eq (sh-ts-mode-test--face "\\*" nil line)
                  'font-lock-escape-face)))))

(ert-deftest sh-ts-mode-fontifies-pattern-contexts-by-font-lock-level ()
  (pcase-dolist (`(,source ,expectations)
                 '(("value=*" (("*" 2 font-lock-string-face)))
                   ("case * in\n(foo$var\\*) :;;\nesac"
                    (("* in" 2 font-lock-string-face)
                     ("foo" 4 font-lock-string-face)
                     ("$var" 4 font-lock-variable-use-face)
                     ("var" 4 font-lock-variable-use-face)
                     ("\\*" 4 font-lock-escape-face)))
                   ("echo *.txt [a-z]x ${x-*}"
                    (("*." 4 font-lock-constant-face)
                     (".txt" 4 font-lock-string-face)
                     ("[" 4 font-lock-bracket-face)
                     ("a-z" 4 font-lock-constant-face)
                     ("-z" 4 font-lock-operator-face)
                     ("z]" 4 font-lock-constant-face)
                     ("]" 4 font-lock-bracket-face)
                     ("*}" 4 font-lock-constant-face)))
                   ("echo ${x}*.txt" (("x}" 4 font-lock-variable-use-face)))
                   ("echo f\\ o*" (("\\ " 4 font-lock-escape-face)))
                   ("echo foo${x:-*}bar"
                    (("foo" 4 font-lock-string-face) ("bar" 4 font-lock-string-face)))
                   ("trimmed=${value#$(printf \"$nested\")}"
                    (("nested" 3 font-lock-variable-use-face)))
                   ("echo ~a*b?/file" (("~a" 3 font-lock-constant-face)))
                   ("cat <<E*?\nbody\nE*?" (("E*?" 2 font-lock-string-face)))
                   ("value=${x:-[!a-c]*?}" (("[" 2 font-lock-string-face)))))
    (dolist (level '(1 2 3 4))
      (ert-info ((format "Level %s: %S" level source))
        (with-temp-buffer
          (insert source "\n")
          (let ((treesit-font-lock-level level)) (sh-ts-mode))
          (font-lock-ensure)
          (pcase-dolist (`(,fragment ,minimum ,face) expectations)
            (goto-char (point-min))
            (search-forward fragment)
            (should (eq (get-text-property (- (point) (length fragment)) 'face)
                        (and (>= level minimum) face)))))))))

(ert-deftest sh-ts-mode-fontifies-escapes-inside-shell-pattern-brackets ()
  (dolist (line '("  ([\\*]) :;;"
                  "  ([a-\\*]) :;;"
                  "  ([[:\\*:]]) :;;"
                  "  ([[.\\*.]]) :;;"
                  "  ([[=\\*=]]) :;;"))
    (with-temp-buffer
      (insert "case value in\n" line "\nesac\n")
      (let ((treesit-font-lock-level 4)) (sh-ts-mode))
      (font-lock-ensure)
      (should (eq (sh-ts-mode-test--face "\\*" nil line)
                  'font-lock-escape-face))))
  (with-temp-buffer
    (let ((line "trimmed=${value#[\\*]}"))
      (insert line "\n")
      (let ((treesit-font-lock-level 4)) (sh-ts-mode))
      (font-lock-ensure)
      (should (eq (sh-ts-mode-test--face "\\*" nil line)
                  'font-lock-escape-face)))))

(ert-deftest sh-ts-mode-fontifies-literal-delimiters-and-escapes ()
  (with-temp-buffer
    (let ((quotes "printf 'one' \"two\\$\" $'three\\n' foo\\ bar ~alice")
          (continued "printf one \\")
          (continuation "two")
          (declaration "cat <<EOF")
          (body "body text")
          (delimiter "EOF"))
      (insert quotes "\n"
              continued "\n" continuation "\n"
              declaration "\n" body "\n" delimiter "\n")
      (let ((treesit-font-lock-level 4)) (sh-ts-mode))
      (font-lock-ensure)
      (dolist (fragment '("'" "one" "\"" "two" "$'" "three"))
        (should (eq (sh-ts-mode-test--face fragment nil quotes)
                    'font-lock-string-face)))
      (dolist (fragment '("~" "alice"))
        (should (eq (sh-ts-mode-test--face fragment nil quotes)
                    'font-lock-constant-face)))
      (dolist (fragment '("\\$" "\\n" "\\ "))
        (should (eq (sh-ts-mode-test--face fragment nil quotes)
                    'font-lock-escape-face)))
      (should (eq (sh-ts-mode-test--face "\\" nil continued)
                  'font-lock-punctuation-face))
      (should (eq (sh-ts-mode-test--face "<<" nil declaration)
                  'font-lock-operator-face))
      (sh-ts-mode-test--should-have-faces
       `((,declaration "EOF" font-lock-string-face)
         (,body "body" font-lock-string-face)
         (,delimiter "EOF" font-lock-string-face))))))

(ert-deftest sh-ts-mode-fontifies-continued-lexical-tokens ()
  (pcase-dolist (`(,source ,fragments ,face)
                 '(("pri\\\nntf x\n" ("pri" "ntf") font-lock-function-call-face)
                   ("pri\\\nn\\\ntf x\n" ("pri" "n" "tf") font-lock-function-call-face)
                   (": va\\\nlue\n" ("va" "lue") font-lock-string-face)
                   ("i\\\nf :; then :; fi\n" ("i" "f") font-lock-keyword-face)
                   ("wor\\\nker() { :; }\n" ("wor" "ker") font-lock-function-name-face)
                   ("VA\\\nR=x\n" ("VA" "R") font-lock-variable-name-face)
                   (": $va\\\nr\n" ("va" "r") font-lock-variable-use-face)
                   (": 1\\\n2>out\n" ("1" "2") font-lock-number-face)
                   (": &\\\n& :\n" ("&" "&") font-lock-operator-face)
                   ("cat <<END\nbody\nEN\\\nD\n" ("EN" "D") font-lock-string-face)
                   ("echo \"`a #head\\\nbody\n`\"\n"
                    ("#head" "body") font-lock-comment-face)))
    (ert-info ((format "%S" source))
      (with-temp-buffer
        (insert source)
        (let ((treesit-font-lock-level 4)) (sh-ts-mode))
        (font-lock-ensure)
        (goto-char (point-min))
        (when (string-prefix-p "cat <<" source)
          (forward-line 2))
        (dolist (fragment fragments)
          (search-forward fragment)
          (should-not (text-property-not-all (- (point) (length fragment))
                                             (point) 'face face)))
        (goto-char (point-min))
        (while (search-forward "\\" nil t)
          (should (eq (get-text-property (1- (point)) 'face)
                      'font-lock-punctuation-face))
          (should (eq (get-text-property (point) 'face) face)))))))

(ert-deftest sh-ts-mode-fontifies-tilde-prefix-pattern-characters-as-constants ()
  (dolist (source '("echo ~a*b?/file"
                    "value=~[!a-c]/file"
                    "echo ~[!a-c[:alpha:][.x.][=y=]]/file"
                    "echo ~[[.a.]-[.z.]]/file"))
    (with-temp-buffer
      (insert source "\n")
      (let ((treesit-font-lock-level 4)) (sh-ts-mode))
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "~")
      (let ((start (1- (point))))
        (search-forward "/")
        (should-not
         (text-property-not-all start (1- (point)) 'face
                                'font-lock-constant-face))))))

(ert-deftest sh-ts-mode-keeps-pattern-source-owners-and-nested-escapes-distinct ()
  (dolist (level '(2 3 4))
    (pcase-dolist (`(,source ,fragment ,minimum ,face)
                   '(("echo ~[a-c]/*.txt" "[a-c]" 4 font-lock-constant-face)
                     ("value=${x#~[a-c]}" "[a-c]" 4 font-lock-constant-face)
                     ("echo ~a\\*b/file" "\\*" 3 font-lock-escape-face)
                     ("echo ${#-\\}}" "\\}" 3 font-lock-escape-face)
                     ("echo \"${#-\\\\}\"" "\\\\" 3 font-lock-escape-face)
                     ("echo \"${#?\\}}\"" "\\}" 3 font-lock-escape-face)
                     ("echo ${#?\\\\}" "\\\\" 3 font-lock-escape-face)
                     (": $'\\x1234g'" "\\x1234" 3 font-lock-escape-face)
                     (": `: \\$'\\\\''`" "\\" 1 nil)
                     (": `: $'\\\\''`" "\\\\'" 3 font-lock-escape-face)
                     ("cat <<[a-\\*]\nbody\n[a-*]" "\\*" 3 font-lock-escape-face)
                     ("cat <<[a-'z']\nbody\n[a-z]" "'z'" 2 font-lock-string-face)
                     ("cat <<[[:'alpha':]]\nbody\n[[:alpha:]]"
                      "'alpha'" 2 font-lock-string-face)
                     ("echo ~$(printf x)/file" "printf" 2 font-lock-function-call-face)))
      (with-temp-buffer
        (insert source "\n")
        (let ((treesit-font-lock-level level)) (sh-ts-mode))
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward fragment)
        (should-not
         (text-property-not-all (- (point) (length fragment)) (point) 'face
                                (and (>= level minimum) face)))))))

(ert-deftest sh-ts-mode-fontifies-collating-range-endpoints-in-every-pattern-context ()
  (dolist (source '("[[.a.]-[.z.]]"
                    "value=x [[.a.]-[.z.]]"
                    "echo [[.a.]-[.z.]]"
                    "for value in [[.a.]-[.z.]]; do :; done"
                    "cat >[[.a.]-[.z.]]"
                    "case x in [[.a.]-[.z.]]) :;; esac"
                    "value=${x#[[.a.]-[.z.]]}"))
    (with-temp-buffer
      (insert source "\n")
      (let ((treesit-font-lock-level 4)) (sh-ts-mode))
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "[[.a.]-[.z.]]")
      (let ((start (- (point) 13))
            (faces '(font-lock-bracket-face font-lock-bracket-face
                                            font-lock-punctuation-face font-lock-constant-face
                                            font-lock-punctuation-face font-lock-bracket-face
                                            font-lock-operator-face font-lock-bracket-face
                                            font-lock-punctuation-face font-lock-constant-face
                                            font-lock-punctuation-face font-lock-bracket-face
                                            font-lock-bracket-face)))
        (dolist (face faces)
          (should (eq (get-text-property start 'face) face))
          (setq start (1+ start)))))))

(ert-deftest sh-ts-mode-preserves-literal-faces-in-pathname-patterns ()
  (dolist (pattern '("*" "?" "[!a-z]"))
    (pcase-dolist (`(,prefix ,suffix ,face)
                   '(("" "" font-lock-function-call-face)
                     ("name=value " "" font-lock-function-call-face)
                     ("echo " "" font-lock-string-face)
                     ("for item in " "; do :; done" font-lock-string-face)
                     ("cat >" "" font-lock-string-face)))
      (with-temp-buffer
        (let ((line (concat prefix "pre\"quoted\"mid" pattern
                            "tail'quoted'end" suffix)))
          (insert line "\n")
          (let ((treesit-font-lock-level 4)) (sh-ts-mode))
          (font-lock-ensure)
          (dolist (fragment '("pre" "mid" "tail" "end"))
            (should (eq (sh-ts-mode-test--face fragment nil line) face)))
          (should (eq (sh-ts-mode-test--face "quoted" nil line)
                      'font-lock-string-face)))))))

(ert-deftest sh-ts-mode-fontifies-inactive-pattern-source-as-strings ()
  (dolist (source '("value=[!a-c[:alpha:][.x.][=y=]]*?"
                    "case [!a-c]*? in x) :;; esac"
                    "value=${x:-[!a-c]*?}"))
    (with-temp-buffer
      (insert source "\n")
      (let ((treesit-font-lock-level 4)) (sh-ts-mode))
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "[")
      (let ((start (1- (point))))
        (search-forward "?")
        (should-not
         (text-property-not-all start (point) 'face
                                'font-lock-string-face))))))

(ert-deftest sh-ts-mode-fontifies-only-here-document-terminator-text ()
  (pcase-dolist (`(,source ,start ,end)
                 '(("cat <<-END\n\tbody\n\tEND\n" 19 22)
                   ("cat <<END\nbody\nEND" 16 19)
                   ("cat <<''\nbody\n\n" 15 15)))
    (with-temp-buffer
      (insert source)
      (let ((treesit-font-lock-level 4)) (sh-ts-mode))
      (font-lock-ensure)
      (should-not (text-property-not-all start end 'face 'font-lock-string-face))
      (when (< end (point-max))
        (should-not (get-text-property end 'face)))
      (when (eq (char-before start) ?\t)
        (should-not (get-text-property (1- start) 'face))))))

(ert-deftest sh-ts-mode-captures-only-leaves ()
  (dolist (source '("if ! :; then x=${#value}; else x=${value:-$?}; fi\n"
                    "value=$(( (x += 2) << 1 )) && echo \\* >>out\n"
                    "cat <<-END\n\t$value\n\tEND\n"
                    ": `: \\`if :; then :; fi\\``\n"
                    "echo pre[!a-c[:alpha:][.x.][=y=]]tail\n"
                    "wor\\\nker() { i\\\nf :; then pri\\\nntf va\\\nlue; fi; }\n"
                    "echo \"`a #x\\\nb\n`\"\n"))
    (with-temp-buffer
      (insert source)
      (let ((treesit-font-lock-level 4)) (sh-ts-mode))
      (let ((root (treesit-buffer-root-node 'sh)))
        (should-not (treesit-node-check root 'has-error))
        (dolist (setting treesit-font-lock-settings)
          (dolist (capture (treesit-query-capture root (car setting)))
            (ert-info ((treesit-node-type (cdr capture)))
              (when (nth 3 setting)
                (should (eq (car capture) 'font-lock-punctuation-face))
                (should (equal (treesit-node-type (cdr capture))
                               "line_continuation"))
                (should (treesit-node-check (cdr capture) 'named))
                (let ((parent (treesit-node-parent (cdr capture))))
                  (dotimes (index (treesit-node-child-count parent))
                    (should (equal (treesit-node-type (treesit-node-child parent index))
                                   "line_continuation")))))
              (dotimes (index (treesit-node-child-count (cdr capture)))
                (let ((child (treesit-node-child (cdr capture) index)))
                  (should (equal (treesit-node-type child) "line_continuation"))
                  (should (treesit-node-check child 'named))
                  (should (= (treesit-node-child-count child) 0)))))))))))

;;;; Navigation

(ert-deftest sh-ts-mode-navigates-structures ()
  (with-temp-buffer
    (insert "printf before\n"
            "first() {\n  :\n}\n"
            "printf between\n"
            "second() (\n  :\n)\n"
            "printf after\n")
    (sh-ts-mode)
    (goto-char (point-max))
    (beginning-of-defun)
    (should (looking-at-p "second()"))
    (beginning-of-defun)
    (should (looking-at-p "first()"))
    (end-of-defun)
    (should (equal (buffer-substring-no-properties
                    (line-beginning-position 0) (point))
                   "}\n"))))

(ert-deftest sh-ts-mode-navigates-cst-sexps ()
  (pcase-dolist (`(,prefix ,unit ,suffix)
                 '(("" "printf" " value\n")
                   ("printf " "foo${bar}" " tail\n")
                   ("printf " "'two words'" " tail\n")
                   ("printf " "\"$name\"" " tail\n")
                   ("printf " "$'two words'" " tail\n")
                   ("printf pre" "${value:-other}" "post\n")
                   ("printf pre" "$(printf value)" "post\n")
                   ("printf pre" "`printf value`" "post\n")
                   ("printf pre" "$((1 + 2))" "post\n")
                   ("" "name=two" " printf value\n")
                   ("for " "item" " in one; do :; done\n")
                   ("printf before\n" "if :; then :; else :; fi" "\nprintf after\n")
                   ("printf before\n" "for x in a; do :; done" "\nprintf after\n")
                   ("printf before\n" "while :; do :; done" "\nprintf after\n")
                   ("printf before\n" "until :; do :; done" "\nprintf after\n")
                   ("printf before\n" "case x in x) :;; esac" "\nprintf after\n")
                   ("printf before\n" "{ :; }" "\nprintf after\n")
                   ("printf before\n" "( : )" "\nprintf after\n")
                   ("printf before\n" "worker() { :; }" "\nprintf after\n")
                   ("printf " "va\\\nlue" " tail\n")))
    (ert-info ((format "%S" (list prefix unit suffix)))
      (with-temp-buffer
        (insert prefix unit suffix)
        (sh-ts-mode)
        (should-not (treesit-node-check (treesit-buffer-root-node 'sh) 'has-error))
        (should (eq forward-sexp-function #'treesit-forward-sexp))
        (let ((start (1+ (length prefix)))
              (end (1+ (+ (length prefix) (length unit)))))
          (goto-char start)
          (forward-sexp)
          (should (= (point) end))
          (backward-sexp)
          (should (= (point) start)))))))

(ert-deftest sh-ts-mode-navigates-sexps-across-comments-and-after-edits ()
  (with-temp-buffer
    (insert "printf one # comment\nprintf two\n")
    (sh-ts-mode)
    (goto-char (point-min))
    (forward-sexp 4)
    (should (= (point) (1- (point-max))))
    (backward-sexp 4)
    (should (= (point) (point-min)))
    (search-forward "one")
    (replace-match "foo${bar}" t t)
    (backward-sexp)
    (should (= (point) 8))
    (forward-sexp)
    (should (= (point) 17))))

(ert-deftest sh-ts-mode-navigates-nested-sexp-boundaries ()
  (pcase-dolist (`(,source ,start ,direction ,expected)
                 '(("printf pre${value}post\n" 8 1 23)
                   ("printf pre${value}post\n" 23 -1 8)
                   ("printf pre${value}post\n" 11 1 19)
                   ("printf pre${value}post\n" 19 -1 11)
                   ("printf pre${value}post\n" 15 1 19)
                   ("printf pre${value}post\n" 15 -1 11)
                   ("printf pre${value}\n" 11 1 19)
                   ("printf pre${value}\n" 19 -1 8)))
    (ert-info ((format "%S from %s by %s" source start direction))
      (with-temp-buffer
        (insert source)
        (sh-ts-mode)
        (goto-char start)
        (forward-sexp direction)
        (should (= (point) expected))))))

;;;; Imenu

(ert-deftest sh-ts-mode-indexes-definitions ()
  (with-temp-buffer
    (insert "printf before\n"
            "first() {\n  :\n}\n"
            "printf between\n"
            "second() (\n  :\n)\n"
            "printf after\n")
    (sh-ts-mode)
    (let ((index (funcall imenu-create-index-function)))
      (should (equal (mapcar #'car index) '("Function")))
      (setq index (cdr (assoc "Function" index)))
      (should (equal (mapcar #'car index) '("first" "second")))
      (should
       (equal
        (mapcar (lambda (entry) (marker-position (cdr entry))) index)
        (list (sh-ts-mode-test--position "first" "first() {")
              (sh-ts-mode-test--position "second" "second() (")))))
    (let ((function (treesit-thing-next (point-min) 'defun))
          (root (treesit-buffer-root-node 'sh)))
      (should (equal (treesit-defun-name function) "first"))
      (should-not (treesit-defun-name root)))))

(ert-deftest sh-ts-mode-keeps-imenu-source-spelling-and-duplicate-names ()
  (with-temp-buffer
    (insert "same() { :; }\nsa\\\nme() { :; }\nsame() { :; }\n")
    (sh-ts-mode)
    (let ((entries (cdr (assoc "Function" (funcall imenu-create-index-function)))))
      (should (equal (mapcar #'car entries) '("same" "sa\\\nme" "same")))
      (should (equal (mapcar (lambda (entry) (marker-position (cdr entry))) entries)
                     '(1 15 31))))))

;;;; Indentation

(ert-deftest sh-ts-mode-indents-after-return ()
  (pcase-dolist (`(,source ,line ,offset ,expected)
                 '(("worker() {\n  :\n}" 0 2 "worker() {\n  \n  :\n}")
                   ("if :; then\n  :\nfi" 0 2 "if :; then\n  \n  :\nfi")
                   ("while :; do\n  :\ndone" 0 2 "while :; do\n  \n  :\ndone")
                   ("{\n  :\n}" 1 2 "{\n  :\n  \n}")
                   ("if :; then\n  :\nelse\n  :\nfi" 2 2 "if :; then\n  :\nelse\n  \n  :\nfi")
                   ("case x in\n  a)\n    :;;\nesac" 1 2 "case x in\n  a)\n    \n    :;;\nesac")
                   ("{\n  {\n    :\n  }\n}" 1 2 "{\n  {\n    \n    :\n  }\n}")
                   ("{\n  {\n    :\n  }\n}" 3 2 "{\n  {\n    :\n  }\n  \n}")
                   ("{\n  {\n    {\n      :\n    }\n  }\n}" 4 2 "{\n  {\n    {\n      :\n    }\n    \n  }\n}")
                   ("{\n  {\n    :\n  } # done\n}" 3 2 "{\n  {\n    :\n  } # done\n  \n}")
                   ("{\n    {\n        :\n    }\n}" 3 4 "{\n    {\n        :\n    }\n    \n}")
                   ("{\n  if :; then\n    :\n  fi\n}" 3 2 "{\n  if :; then\n    :\n  fi\n  \n}")
                   ("{\n  while :; do\n    :\n  done\n}" 3 2 "{\n  while :; do\n    :\n  done\n  \n}")
                   ("{\n  :\n}" 2 2 "{\n  :\n}\n")
                   ("{\n    :\n}" 0 4 "{\n    \n    :\n}")
                   (":\n:" 0 2 ":\n\n:")
                   ("cat <<EOF\ntext\nEOF" 1 2 "cat <<EOF\ntext\n\nEOF")))
    (ert-info ((format "Return on line %s: %S" line source))
      (with-temp-buffer
        (insert source)
        (sh-ts-mode)
        (setq-local indent-tabs-mode nil)
        (setq-local sh-ts-mode-indent-offset offset)
        (electric-indent-local-mode 1)
        (goto-char (point-min))
        (forward-line line)
        (end-of-line)
        (call-interactively (key-binding (kbd "RET")))
        (should (equal (buffer-string) expected))))))

(ert-deftest sh-ts-mode-indents-consecutive-returns ()
  (pcase-dolist (`(,prefix ,suffix ,column)
                 '(("f() {\n  echo ok" "\n}" 2)
                   ("if :; then\n  if :; then\n    echo ok" "\n  fi\nfi" 4)
                   ("if :; then\n  if :; then\n    echo ok\n  fi" "\nfi" 2)
                   ("x=$(\n  echo ok" "\n)" 2)
                   ("x=$(echo ok" "\necho more)" 2)
                   ("x=`\n  echo ok" "\n`" 2)
                   ("f() {\n  x=$(\n    echo ok" "\n  )\n}" 4)
                   ("x=$(\n  echo $(\n    echo ok" "\n  )\n)" 4)
                   ("f() {\n  echo a |" "\n    cat\n}" 4)
                   ("f() {\n  echo a &&" "\n    echo b\n}" 4)
                   ("f() {\n  echo a ||" "\n    echo b\n}" 4)
                   ("echo a |" "\n  cat\n" 2)
                   ("echo ok" "" 0)))
    (ert-info ((format "%S / %S" prefix suffix))
      (with-temp-buffer
        (insert prefix suffix)
        (sh-ts-mode)
        (setq-local indent-tabs-mode nil)
        (electric-indent-local-mode 1)
        (goto-char (1+ (length prefix)))
        (dotimes (_ 3)
          (call-interactively (key-binding (kbd "RET")))
          (should (= (current-column) column)))))))

(ert-deftest sh-ts-mode-indents-structures ()
  (pcase-dolist (`(,source ,offset ,expected)
                 '(("if ready\nthen\nprintf yes &&\nprintf more\nelse\nprintf no\nfi\ncase x in\na)\nprintf a\n;;\nesac\n" 2 "if ready\nthen\n  printf yes &&\n    printf more\nelse\n  printf no\nfi\ncase x in\n  a)\n    printf a\n    ;;\nesac\n")
                   ("{\nprintf body\n}\n" 4 "{\n    printf body\n}\n")
                   ("{\nprintf body\n        }\n" 2 "{\n  printf body\n}\n")
                   ("(\nprintf body\n        )\n" 2 "(\n  printf body\n)\n")
                   ("while ready\n        do\nprintf x\n        done\n" 2 "while ready\ndo\n  printf x\ndone\n")
                   ("if a\n        then\n:\n        elif b\nthen\n:\n        fi\n" 2 "if a\nthen\n  :\nelif b\nthen\n  :\nfi\n")
                   ("worker() {\nprintf body\n}\n" 2 "worker() {\n  printf body\n}\n")
                   ("x=$(\nprintf a\nprintf b\n)\n" 2 "x=$(\n  printf a\n  printf b\n)\n")
                   ("x=`\nprintf a\nprintf b\n`\n" 2 "x=`\n  printf a\n  printf b\n`\n")
                   ("true && {\nprintf x\n}\n" 2 "true && {\n  printf x\n}\n")
                   ("while ready; do\nprintf x\ndone\n" 2 "while ready; do\n  printf x\ndone\n")
                   ("{\nprintf a\nprintf b\n}\n" 2 "{\n  printf a\n  printf b\n}\n")
                   ("if x\nthen\nprintf a\nprintf b\nfi\n" 2 "if x\nthen\n  printf a\n  printf b\nfi\n")
                   ("case x in\na)\n:\n;;\nb)\n:\n;;\nesac\n" 2 "case x in\n  a)\n    :\n    ;;\n  b)\n    :\n    ;;\nesac\n")))
    (ert-info ((format "Offset %s: %S" offset source))
      (should (equal (sh-ts-mode-test--indent source offset) expected)))))

(ert-deftest sh-ts-mode-preserves-expansion-boundaries-during-indentation ()
  (pcase-dolist (`(,prefix ,suffix)
                 '(("if :; then\n  " "\nfi\n")
                   ("worker() {\n  " "\n}\n")))
    (pcase-dolist (`(,left ,right)
                   '(("value=$((count + 1)" ")")
                     ("value=${name" "}")
                     ("value=${#name" "}")
                     ("value=${name:-fallback" "}")
                     ("value=${name:-" "}")
                     ("value=${name%pat" "}")
                     ("value=\"${name:-fallback" "}\"")
                     ("value=${name:-fallback" "  }")))
      (dolist (operation '(newline-and-indent indent-region))
        (ert-info ((format "%s: %S" operation (concat prefix left right suffix)))
          (with-temp-buffer
            (insert prefix left right suffix)
            (sh-ts-mode)
            (goto-char (+ 1 (length prefix) (length left)))
            (insert "\\")
            (if (eq operation 'newline-and-indent)
                (newline-and-indent)
              (newline)
              (indent-region (point-min) (point-max)))
            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                           (concat prefix left "\\\n" right suffix)))
            (should-not (treesit-node-check (treesit-buffer-root-node 'sh)
                                            'has-error))))))))

(ert-deftest sh-ts-mode-keeps-here-document-bodies-unindented ()
  (should
   (equal
    (sh-ts-mode-test--indent
     "{\ncat <<EOF\nbody text\n  spaced\nEOF\n: after\n}\n")
    "{\n  cat <<EOF\nbody text\n  spaced\nEOF\n  : after\n}\n")))

;;;; Updates

(ert-deftest sh-ts-mode-refreshes-syntax-before-multiline-errors ()
  (dolist (narrow '(nil t))
    (with-temp-buffer
      (insert "f() {\n  echo ok\n}\n")
      (let ((treesit-font-lock-level 4)) (sh-ts-mode))
      (syntax-propertize (point-max))
      (should (= (syntax-class (syntax-after 2)) 4))
      (goto-char (point-min))
      (search-forward "}")
      (backward-char 1)
      (let ((position (point)) (removed (char-after)))
        (save-restriction
          (when narrow (narrow-to-region (line-beginning-position) (point-max)))
          (delete-char 1)
          (syntax-propertize (point-max)))
        (sh-ts-mode-test--should-match-fresh-buffer 4)
        (should (= (syntax-class (syntax-after 2)) 1))
        (goto-char position)
        (insert removed)
        (sh-ts-mode-test--should-match-fresh-buffer 4)
        (should (= (syntax-class (syntax-after 2)) 4))))))

(ert-deftest sh-ts-mode-keeps-commands-after-closed-here-document-substitutions ()
  (dolist (source '("x=$(cat <<EOF)\nhi\nEOF\n"
                    "x=`cat <<EOF`\nhi\nEOF\n"))
    (with-temp-buffer
      (insert source)
      (let ((treesit-font-lock-level 4)) (sh-ts-mode))
      (font-lock-ensure)
      (should-not (treesit-node-check (treesit-buffer-root-node 'sh) 'has-error))
      (dolist (command '("hi" "EOF"))
        (should (eq (sh-ts-mode-test--face command nil command)
                    'font-lock-function-call-face))))))

(ert-deftest sh-ts-mode-restores-continued-closing-keywords-after-body-repair ()
  (pcase-dolist (`(,prefix ,suffix)
                 '(("" "") ("cat <<END\n$(\n" ")\nEND\n")))
    (pcase-dolist (`(,header ,ending)
                   '(("if true; then\n" "f\\\ni\n")
                     ("while true; do\n" "do\\\nne\n")))
      (with-temp-buffer
        (insert prefix header ":\n" ending suffix)
        (let ((treesit-font-lock-level 4)) (sh-ts-mode))
        (font-lock-ensure)
        (goto-char (+ (point-min) (length prefix) (length header)))
        (delete-char 2)
        (font-lock-ensure)
        (insert ":\n")
        (font-lock-ensure)
        (should-not (treesit-node-check (treesit-buffer-root-node 'sh) 'has-error))
        (should (eq (sh-ts-mode-test--face ending) 'font-lock-keyword-face))
        (should (eq (sh-ts-mode-test--face "\\") 'font-lock-punctuation-face))
        (sh-ts-mode-test--should-match-fresh-buffer 4)))))

(ert-deftest sh-ts-mode-updates-like-fresh-buffer ()
  (pcase-dolist (`(,source ,old ,new ,fragment ,face)
                 '(("echo ${00}\n" "00" "01" "01" font-lock-constant-face)
                   ("fi\n" "fi" "if :; then :; fi" "fi" font-lock-keyword-face)
                   ("cat <<EOF\nbody\n" "body" "body\nEOF" "body" font-lock-string-face)))
    (ert-info ((format "%S: %S -> %S" source old new))
      (with-temp-buffer

        (insert source)
        (let ((treesit-font-lock-level 4)) (sh-ts-mode))
        (sh-ts-mode-test--buffer-state)
        (goto-char (sh-ts-mode-test--position old))
        (delete-char (length old))
        (insert new)
        (font-lock-ensure)
        (should (eq (sh-ts-mode-test--face fragment) face))
        (sh-ts-mode-test--should-match-fresh-buffer 4)))))

(ert-deftest sh-ts-mode-reclassifies-delimiters-after-edits ()
  (with-temp-buffer
    (insert "{\nprintf body\n}\n")
    (sh-ts-mode)
    (should (= (sh-ts-mode-test--syntax-class "}" nil "}") 5))
    (goto-char (point-min))
    (delete-char 1)
    (should (= (sh-ts-mode-test--syntax-class "}" nil "}") 1))
    (goto-char (point-min))
    (insert "{")
    (should (= (sh-ts-mode-test--syntax-class "}" nil "}") 5))))

(ert-deftest sh-ts-mode-recomputes-comment-syntax-after-edits ()
  (with-temp-buffer
    (insert "printf value#suffix\n")
    (sh-ts-mode)
    (should-not
     (sh-ts-mode-test--comment-p "suffix" nil "printf value#suffix"))
    (goto-char
     (sh-ts-mode-test--position "#" "printf value#suffix"))
    (insert " ")
    (should
     (sh-ts-mode-test--comment-p "suffix" nil "printf value #suffix"))
    (goto-char
     (sh-ts-mode-test--position " #" "printf value #suffix"))
    (delete-char 1)
    (should-not
     (sh-ts-mode-test--comment-p "suffix" nil "printf value#suffix")))
  (with-temp-buffer
    (insert "echo \"`a # head\nbody\n`\"\n: tail\n")
    (let ((treesit-font-lock-level 4)) (sh-ts-mode))
    (dolist (continued '(t nil t))
      (goto-char (point-min))
      (search-forward "# head")
      (if continued (insert "\\") (delete-char 1))
      (narrow-to-region
       (sh-ts-mode-test--position "body" "body") (point-max))
      (syntax-propertize (point-max))
      (widen)
      (should (eq (sh-ts-mode-test--comment-p "body" nil "body") continued))
      (should (sh-ts-mode-test--comment-p "head" nil (if continued "echo \"`a # head\\" "echo \"`a # head")))
      (should-not (sh-ts-mode-test--comment-p "tail" nil ": tail"))
      (font-lock-flush)
      (font-lock-ensure)
      (sh-ts-mode-test--should-match-fresh-buffer 4))))

(ert-deftest sh-ts-mode-preserves-syntax-when-narrowed ()
  (with-temp-buffer
    (insert "echo a # first\necho b\necho c # third\n")
    (sh-ts-mode)
    (narrow-to-region
     (sh-ts-mode-test--position "echo" "echo b")
     (point-max))
    (syntax-propertize (point-max))
    (widen)
    (should (sh-ts-mode-test--comment-p "first" nil "echo a # first"))
    (should (sh-ts-mode-test--comment-p "third" nil "echo c # third"))))

(ert-deftest sh-ts-mode-updates-pattern-context-after-edits ()
  (with-temp-buffer
    (let ((pattern-line "value=${x##*}")
          (value-line "value=${x:-*}"))
      (insert pattern-line "\n")
      (let ((treesit-font-lock-level 4)) (sh-ts-mode))
      (font-lock-ensure)
      (should (eq (sh-ts-mode-test--face "*" nil pattern-line)
                  'font-lock-constant-face))
      (goto-char (point-min))
      (search-forward "##")
      (replace-match ":-")
      (font-lock-flush (point-min) (point-max))
      (font-lock-ensure)
      (should (eq (sh-ts-mode-test--face "*" nil value-line)
                  'font-lock-string-face)))))

(ert-deftest sh-ts-mode-reclassifies-backquote-words-after-enclosing-quote-edits ()
  (with-temp-buffer
    (insert "echo `printf %s \\\"a b\\\"`\n")
    (let ((treesit-font-lock-level 4)) (sh-ts-mode))
    (dolist (quoted '(nil t nil))
      (goto-char 6)
      (when (eq (char-after) ?\")
        (delete-char 1)
        (goto-char (1- (point-max)))
        (delete-char -1))
      (when quoted
        (goto-char 6)
        (insert "\"")
        (goto-char (1- (point-max)))
        (insert "\""))
      (font-lock-flush)
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "\\")
      (should (eq (get-text-property (1- (point)) 'face)
                  (unless quoted 'font-lock-escape-face)))
      (should (eq (get-text-property (point) 'face)
                  (if quoted 'font-lock-string-face 'font-lock-escape-face)))
      (search-forward "a b")
      (should (eq (get-text-property (- (point) 2) 'face)
                  (and quoted 'font-lock-string-face)))
      (sh-ts-mode-test--should-match-fresh-buffer 4))))

(ert-deftest sh-ts-mode-fontifies-continued-constructs-after-completion ()
  (pcase-dolist (`(,incomplete ,completion)
                 '(("i\\\nf\n" "true; then :; fi\n")
                   ("w\\\nhile\n" "true; do :; done\n")
                   ("f\\\nor item in one\n" "do :; done\n")
                   ("c\\\nase x in\nx)" " :;; esac\n")
                   ("f\\\n() {\n" ":;\n}\n")
                   ("e\\\ncho inner |\n" "cat\n")
                   ("e\\\ncho ${value:-" "inner}\n")
                   ("e\\\ncho $(" "echo inner)\n")))
    (with-temp-buffer
      (insert "#!/bin/sh\n\nprintf before\n\n")
      (let ((treesit-font-lock-level 4)) (sh-ts-mode))
      (font-lock-ensure)
      (let ((start (point)))
        (insert incomplete)
        (font-lock-ensure)
        (insert completion)
        (font-lock-ensure)
        (should-not (treesit-node-check (treesit-buffer-root-node 'sh) 'has-error))
        (sh-ts-mode-test--should-match-fresh-buffer 4)
        (goto-char start)
        (search-forward "\\")
        (should (eq (get-text-property (1- (point)) 'face)
                    'font-lock-punctuation-face))
        (delete-region start (point-max))
        (font-lock-ensure)
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       "#!/bin/sh\n\nprintf before\n\n"))
        (sh-ts-mode-test--should-match-fresh-buffer 4)))))

(ert-deftest sh-ts-mode-rebuilds-imenu-after-edits ()
  (with-temp-buffer
    (insert "first() { :; }\n")
    (sh-ts-mode)
    (should (equal (mapcar #'car (cdr (assoc "Function"
                                             (funcall imenu-create-index-function))))
                   '("first")))
    (goto-char (point-max))
    (insert "second() { :; }\n")
    (should (equal (mapcar #'car (cdr (assoc "Function"
                                             (funcall imenu-create-index-function))))
                   '("first" "second")))))

(provide 'sh-ts-mode-test)

;;; sh-ts-mode-test.el ends here
