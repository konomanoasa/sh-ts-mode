;;; sh-ts-mode-test.el --- Tests for sh-ts-mode  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'sh-ts-mode)

(defun sh-ts-mode-test--require-grammar ()
  (unless (treesit-ready-p 'sh t)
    (ert-skip "The sh grammar is unavailable")))

(defun sh-ts-mode-test--grammar-source (configured)
  (let ((treesit-language-source-alist configured)
        (ensure (symbol-function 'treesit-ensure-installed))
        source)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (installed-language)
                  (setq source
                        (assq installed-language
                              treesit-language-source-alist))
                  t))
          (should (sh-ts-mode--ensure-grammar 'sh))
          source)
      (fset 'treesit-ensure-installed ensure))))

(defun sh-ts-mode-test--enable-font-lock-level (level)
  (let ((treesit-font-lock-level level))
    (sh-ts-mode)))

(defun sh-ts-mode-test--position-in-line (line fragment)
  (let (line-start)
    (save-excursion
      (goto-char (point-min))
      (while (and (not line-start) (not (eobp)))
        (let ((start (line-beginning-position))
              (end (line-end-position)))
          (if (equal line (buffer-substring-no-properties start end))
              (setq line-start start)
            (forward-line 1)))))
    (unless line-start
      (ert-fail (format "Test line not found: %s" line)))
    (let ((offset (string-search fragment line)))
      (unless offset
        (ert-fail (format "Fragment %s not found in test line: %s"
                          fragment line)))
      (+ line-start offset))))

(defun sh-ts-mode-test--face-in-line (line fragment &optional offset)
  (get-text-property
   (+ (sh-ts-mode-test--position-in-line line fragment)
      (or offset 0))
   'face))

(defun sh-ts-mode-test--comment-in-line-p (line fragment &optional offset)
  (syntax-propertize (point-max))
  (nth 4 (syntax-ppss
          (+ (sh-ts-mode-test--position-in-line line fragment)
             (or offset 0)))))

(defun sh-ts-mode-test--syntax-class-in-line
    (line fragment &optional offset)
  (syntax-propertize (point-max))
  (syntax-class
   (syntax-after
    (+ (sh-ts-mode-test--position-in-line line fragment)
       (or offset 0)))))

(defun sh-ts-mode-test--should-fontify (cases)
  (pcase-dolist (`(,line ,fragment ,face) cases)
    (should (equal (list line fragment
                         (sh-ts-mode-test--face-in-line line fragment))
                   (list line fragment face)))))

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

(ert-deftest sh-ts-mode-starts-a-posix-sh-parser ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (insert "printf '%s\\n' hello\n")
    (sh-ts-mode)
    (should (eq major-mode 'sh-ts-mode))
    (should
     (equal (treesit-node-type (treesit-buffer-root-node 'sh))
            "program"))))

(ert-deftest sh-ts-mode-provides-a-default-grammar-source ()
  (should
   (equal
    (sh-ts-mode-test--grammar-source nil)
    '(sh "https://github.com/konomanoasa/tree-sitter-sh"
         :revision "v0.7.0"))))

(ert-deftest sh-ts-mode-preserves-a-user-grammar-source ()
  (let ((custom '(sh . ("custom-source"))))
    (should (equal (sh-ts-mode-test--grammar-source (list custom))
                   custom))))

(ert-deftest sh-ts-mode-classifies-only-shell-delimiters-as-parens ()
  (sh-ts-mode-test--require-grammar)
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
      (dolist (character '(?\( ?\) ?\[ ?\] ?{ ?}))
        (should (eq (char-syntax character) ?.)))
      (dolist (expectation
               `((,function "(" 0 4)
                 (,function ")" 0 5)
                 (,function "{" 0 4)
                 (,expansions "${" 0 2)
                 (,expansions "${" 1 4)
                 (,expansions "]]}" 2 5)
                 (,expansions "$((" 0 2)
                 (,expansions "$((" 1 4)
                 (,expansions "$((" 2 4)
                 (,expansions "(2)" 0 4)
                 (,expansions "(2)" 2 5)
                 (,expansions "2)))" 2 5)
                 (,expansions "2)))" 3 5)
                 (,expansions "$(printf" 0 2)
                 (,expansions "$(printf" 1 4)
                 (,expansions "x)\"" 1 5)
                 (,closing "}" 0 5)
                 (,parenthesized-case "(" 0 4)
                 (,parenthesized-case ")" 0 5)))
        (pcase-let ((`(,line ,fragment ,offset ,class) expectation))
          (should
           (= (sh-ts-mode-test--syntax-class-in-line
               line fragment offset)
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
                 (,expansions "[[" 0)
                 (,expansions "[[" 1)
                 (,expansions "]]" 0)
                 (,expansions "]]" 1)
                 (,plain-case ")" 0)))
        (pcase-let ((`(,line ,fragment ,offset) expectation))
          (should
           (= (sh-ts-mode-test--syntax-class-in-line
               line fragment offset)
              1)))))))

(ert-deftest sh-ts-mode-reclassifies-delimiters-after-edits ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (insert "{\nprintf body\n}\n")
    (sh-ts-mode)
    (should (= (sh-ts-mode-test--syntax-class-in-line "}" "}") 5))
    (goto-char (point-min))
    (delete-char 1)
    (should (= (sh-ts-mode-test--syntax-class-in-line "}" "}") 1))
    (goto-char (point-min))
    (insert "{")
    (should (= (sh-ts-mode-test--syntax-class-in-line "}" "}") 5))))

(ert-deftest sh-ts-mode-configures-posix-sh-line-comments ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (sh-ts-mode)
    (should (equal comment-start "# "))
    (should (equal comment-end ""))
    (should (equal comment-start-skip "#[[:blank:]]*"))
    (should comment-use-syntax)
    (should (eq (char-syntax ?#) ?.))
    (should (eq (char-syntax ?\") ?.))
    (should (eq (char-syntax ?\\) ?.))
    (should (eq (char-syntax ?\n) ?>))))

(ert-deftest sh-ts-mode-recognizes-only-tree-sitter-comments ()
  (sh-ts-mode-test--require-grammar)
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
             "after")))
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
                 ("# after-continuation" "after-continuation")))
        (pcase-let ((`(,line ,fragment) expectation))
          (should (sh-ts-mode-test--comment-in-line-p line fragment))))
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
                 ("after" "after" 0)))
        (pcase-let ((`(,line ,fragment ,offset) expectation))
          (should-not
           (sh-ts-mode-test--comment-in-line-p line fragment offset)))))))

(ert-deftest sh-ts-mode-recomputes-comment-syntax-after-edits ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (insert "printf value#suffix\n")
    (sh-ts-mode)
    (should-not
     (sh-ts-mode-test--comment-in-line-p "printf value#suffix" "suffix"))
    (goto-char
     (sh-ts-mode-test--position-in-line "printf value#suffix" "#"))
    (insert " ")
    (should
     (sh-ts-mode-test--comment-in-line-p "printf value #suffix" "suffix"))
    (goto-char
     (sh-ts-mode-test--position-in-line "printf value #suffix" " #"))
    (delete-char 1)
    (should-not
     (sh-ts-mode-test--comment-in-line-p "printf value#suffix" "suffix"))))

(ert-deftest sh-ts-mode-recomputes-comment-syntax-outside-a-restriction ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (insert "echo a # first\necho b\necho c # third\n")
    (sh-ts-mode)
    (narrow-to-region
     (sh-ts-mode-test--position-in-line "echo b" "echo")
     (point-max))
    (syntax-propertize (point-max))
    (widen)
    (should (sh-ts-mode-test--comment-in-line-p "echo a # first" "first"))
    (should (sh-ts-mode-test--comment-in-line-p "echo c # third" "third"))))

(ert-deftest sh-ts-mode-propertizes-comments-at-the-end-of-wide-programs ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (dotimes (number 10000)
      (insert (format "printf value%d # comment%d\n" number number)))
    (sh-ts-mode)
    (goto-char (point-max))
    (search-backward "# comment9999")
    (let ((comment-start (point)))
      (funcall syntax-propertize-function comment-start (point-max))
      (should-not (nth 4 (syntax-ppss (1- comment-start))))
      (should (nth 4 (syntax-ppss (+ comment-start 2)))))))

(ert-deftest sh-ts-mode-uses-standard-tree-sitter-navigation ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (sh-ts-mode)
    (should (equal (alist-get 'sh treesit-thing-settings)
                   (alist-get 'sh sh-ts-mode-thing-settings)))
    (should-not (treesit-thing-defined-p 'sexp 'sh))
    (should-not (treesit-thing-defined-p 'sentence 'sh))
    (should-not (eq forward-sexp-function #'treesit-forward-sexp))
    (should (eq beginning-of-defun-function
                #'treesit-beginning-of-defun))
    (should (eq end-of-defun-function #'treesit-end-of-defun))))

(ert-deftest sh-ts-mode-navigates-function-definitions-as-defuns ()
  (sh-ts-mode-test--require-grammar)
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

(ert-deftest sh-ts-mode-uses-standard-tree-sitter-imenu ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (sh-ts-mode)
    (should (equal treesit-simple-imenu-settings
                   sh-ts-mode-imenu-settings))
    (should (eq treesit-defun-name-function
                #'sh-ts-mode--defun-name))
    (should (eq imenu-create-index-function #'treesit-simple-imenu))))

(ert-deftest sh-ts-mode-indexes-only-function-definitions ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (insert "printf before\n"
            "first() {\n  :\n}\n"
            "printf between\n"
            "second() (\n  :\n)\n"
            "printf after\n")
    (sh-ts-mode)
    (let ((index (funcall imenu-create-index-function)))
      (should (equal (mapcar #'car index) '("first" "second")))
      (should
       (equal
        (mapcar (lambda (entry) (marker-position (cdr entry))) index)
        (list (sh-ts-mode-test--position-in-line "first() {" "first")
              (sh-ts-mode-test--position-in-line "second() (" "second")))))
    (let ((function (treesit-thing-next (point-min) 'defun))
          (root (treesit-buffer-root-node 'sh)))
      (should (equal (treesit-defun-name function) "first"))
      (should-not (treesit-defun-name root)))))

(ert-deftest sh-ts-mode-rebuilds-imenu-after-edits ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (insert "first() { :; }\n")
    (sh-ts-mode)
    (should (equal (mapcar #'car (funcall imenu-create-index-function))
                   '("first")))
    (goto-char (point-max))
    (insert "second() { :; }\n")
    (should (equal (mapcar #'car (funcall imenu-create-index-function))
                   '("first" "second")))))

(ert-deftest sh-ts-mode-uses-standard-tree-sitter-indentation ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (sh-ts-mode)
    (should (eq indent-line-function #'treesit-indent))
    (should (eq indent-region-function #'treesit-indent-region))
    (should (equal (alist-get 'sh treesit-simple-indent-rules)
                   (alist-get 'sh sh-ts-mode-indent-rules)))))

(ert-deftest sh-ts-mode-indents-structural-rules ()
  (sh-ts-mode-test--require-grammar)
  (should
   (equal
    (sh-ts-mode-test--indent
     "if ready\nthen\nprintf yes &&\nprintf more\nelse\nprintf no\nfi\ncase x in\na)\nprintf a\n;;\nesac\n")
    "if ready\nthen\n  printf yes &&\n    printf more\nelse\n  printf no\nfi\ncase x in\n  a)\n    printf a\n    ;;\nesac\n")))

(ert-deftest sh-ts-mode-indent-offset-controls-rules ()
  (sh-ts-mode-test--require-grammar)
  (should
   (equal
    (sh-ts-mode-test--indent
     "{\nprintf body\n}\n" 4)
    "{\n    printf body\n}\n")))

(ert-deftest sh-ts-mode-indents-closers-from-wrong-columns ()
  (sh-ts-mode-test--require-grammar)
  (should
   (equal
    (sh-ts-mode-test--indent
     "{\nprintf body\n        }\n")
    "{\n  printf body\n}\n"))
  (should
   (equal
    (sh-ts-mode-test--indent
     "(\nprintf body\n        )\n")
    "(\n  printf body\n)\n"))
  (should
   (equal
    (sh-ts-mode-test--indent
     "while ready\n        do\nprintf x\n        done\n")
    "while ready\ndo\n  printf x\ndone\n"))
  (should
   (equal
    (sh-ts-mode-test--indent
     "if a\n        then\n:\n        elif b\nthen\n:\n        fi\n")
    "if a\nthen\n  :\nelif b\nthen\n  :\nfi\n")))

(ert-deftest sh-ts-mode-indents-bodies-after-inline-openers ()
  (sh-ts-mode-test--require-grammar)
  (should
   (equal
    (sh-ts-mode-test--indent
     "worker() {\nprintf body\n}\n")
    "worker() {\n  printf body\n}\n"))
  (should
   (equal
    (sh-ts-mode-test--indent
     "true && {\nprintf x\n}\n")
    "true && {\n  printf x\n}\n"))
  (should
   (equal
    (sh-ts-mode-test--indent
     "while ready; do\nprintf x\ndone\n")
    "while ready; do\n  printf x\ndone\n")))

(ert-deftest sh-ts-mode-indents-every-command-in-a-body ()
  (sh-ts-mode-test--require-grammar)
  (should
   (equal
    (sh-ts-mode-test--indent
     "{\nprintf a\nprintf b\n}\n")
    "{\n  printf a\n  printf b\n}\n"))
  (should
   (equal
    (sh-ts-mode-test--indent
     "if x\nthen\nprintf a\nprintf b\nfi\n")
    "if x\nthen\n  printf a\n  printf b\nfi\n")))

(ert-deftest sh-ts-mode-indents-every-case-item ()
  (sh-ts-mode-test--require-grammar)
  (should
   (equal
    (sh-ts-mode-test--indent
     "case x in\na)\n:\n;;\nb)\n:\n;;\nesac\n")
    "case x in\n  a)\n    :\n    ;;\n  b)\n    :\n    ;;\nesac\n")))

(ert-deftest sh-ts-mode-keeps-here-document-bodies-unindented ()
  (sh-ts-mode-test--require-grammar)
  (should
   (equal
    (sh-ts-mode-test--indent
     "{\ncat <<EOF\nbody text\n  spaced\nEOF\n: after\n}\n")
    "{\n  cat <<EOF\nbody text\n  spaced\nEOF\n  : after\n}\n")))

(ert-deftest sh-ts-mode-does-not-claim-file-extensions ()
  (dolist (entry auto-mode-alist)
    (should-not (eq (cdr entry) 'sh-ts-mode))))

(ert-deftest sh-ts-mode-generates-mode-and-interpreter-autoloads ()
  (require 'loaddefs-gen)
  (let ((output (make-temp-file "sh-ts-mode-loaddefs-"))
        (directory
         (file-name-directory (locate-library "sh-ts-mode"))))
    (unwind-protect
        (progn
          (loaddefs-generate directory output nil nil nil t)
          (with-temp-buffer
            (insert-file-contents output)
            (dolist (form '("(autoload 'sh-ts-mode"
                            "(add-to-list 'interpreter-mode-alist"))
              (goto-char (point-min))
              (should (search-forward form nil t)))
            (goto-char (point-min))
            (should-not (search-forward "(add-to-list 'auto-mode-alist" nil t))))
      (delete-file output))))

(ert-deftest sh-ts-mode-selects-sh-interpreters ()
  (sh-ts-mode-test--require-grammar)
  (should (equal (alist-get "sh" interpreter-mode-alist nil nil #'equal)
                 'sh-ts-mode))
  (dolist (shebang '("#!/bin/sh\n"
                     "#!/usr/bin/env sh\n"
                     "#!/usr/bin/env -S sh\n"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/example")
      (insert shebang "printf '%s\\n' hello\n")
      (set-auto-mode)
      (should (eq major-mode 'sh-ts-mode)))))

(ert-deftest sh-ts-mode-fontifies-posix-shell-syntax-by-concept ()
  (sh-ts-mode-test--require-grammar)
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
      (sh-ts-mode-test--enable-font-lock-level 4)
      (font-lock-ensure)
      (sh-ts-mode-test--should-fontify
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
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((variable "printf \"$name ${name}\"")
          (line "value=$(printf \"${1:-$?} $((2 + (3)))\")")
          (unclassified "printf \"${00}\"")
          (backquote "printf `printf \\${name}`"))
      (insert variable "\n" line "\n" unclassified "\n" backquote "\n")
      (sh-ts-mode-test--enable-font-lock-level 4)
      (font-lock-ensure)
      (should (eq (sh-ts-mode-test--face-in-line variable "${")
                  'font-lock-variable-use-face))
      (dolist (expectation '(("${" 1) ("}" 0)))
        (should (eq (sh-ts-mode-test--face-in-line
                     variable (car expectation) (cadr expectation))
                    'font-lock-bracket-face)))
      (should (eq (sh-ts-mode-test--face-in-line variable "name")
                  'font-lock-variable-use-face))
      (should (eq (sh-ts-mode-test--face-in-line variable "$name")
                  'font-lock-variable-use-face))
      (dolist (expectation '(("${1" 2) ("$?" 1)))
        (should (eq (sh-ts-mode-test--face-in-line
                     line (car expectation) (cadr expectation))
                    'font-lock-constant-face)))
      (should (eq (sh-ts-mode-test--face-in-line line "$?")
                  'font-lock-variable-use-face))
      (should (eq (sh-ts-mode-test--face-in-line line "${")
                  'font-lock-variable-use-face))
      (dolist (expectation '(("${" 1) ("}" 0)))
        (should (eq (sh-ts-mode-test--face-in-line
                     line (car expectation) (cadr expectation))
                    'font-lock-bracket-face)))
      (should (eq (sh-ts-mode-test--face-in-line line "$(")
                  'font-lock-punctuation-face))
      (should (eq (sh-ts-mode-test--face-in-line line "$((")
                  'font-lock-punctuation-face))
      (should (eq (sh-ts-mode-test--face-in-line line "(3)")
                  'font-lock-bracket-face))
      (should-not (sh-ts-mode-test--face-in-line unclassified "${"))
      (dolist (expectation '(("${" 1) ("}" 0)))
        (should (eq (sh-ts-mode-test--face-in-line
                     unclassified (car expectation) (cadr expectation))
                    'font-lock-bracket-face)))
      (dolist (fragment '("{name}" "}`"))
        (should (eq (sh-ts-mode-test--face-in-line backquote fragment)
                    'font-lock-bracket-face)))
      (should (eq (sh-ts-mode-test--face-in-line backquote "${name}")
                  'font-lock-variable-use-face))
      (should (eq (sh-ts-mode-test--face-in-line backquote "name}")
                  'font-lock-variable-use-face)))))

(ert-deftest sh-ts-mode-fontifies-grouping-and-substitution-brackets ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((subshell "! (printf x)")
          (command-substitution "value=$(printf x)")
          (backquote-substitution "value=`printf x`")
          (arithmetic-expansion "value=$((1 + (2)))"))
      (insert subshell "\n"
              command-substitution "\n"
              backquote-substitution "\n"
              arithmetic-expansion "\n")
      (sh-ts-mode-test--enable-font-lock-level 4)
      (font-lock-ensure)
      (should (eq (sh-ts-mode-test--face-in-line subshell "!")
                  'font-lock-operator-face))
      (dolist (fragment '("(" ")"))
        (should (eq (sh-ts-mode-test--face-in-line subshell fragment)
                    'font-lock-bracket-face)))
      (dolist (line (list command-substitution arithmetic-expansion))
        (should (eq (sh-ts-mode-test--face-in-line line "$")
                    'font-lock-punctuation-face)))
      (dolist (expectation
               `((,command-substitution "$(" 1)
                 (,command-substitution ")" 0)
                 (,arithmetic-expansion "$((" 1)
                 (,arithmetic-expansion "$((" 2)
                 (,arithmetic-expansion "(2)" 0)
                 (,arithmetic-expansion "(2)" 2)
                 (,arithmetic-expansion "2)))" 2)
                 (,arithmetic-expansion "2)))" 3)))
        (pcase-let ((`(,line ,fragment ,offset) expectation))
          (should (eq (sh-ts-mode-test--face-in-line
                      line fragment offset)
                      'font-lock-bracket-face))))
      (dolist (expectation
               `((,backquote-substitution "`printf" 0)
                 (,backquote-substitution "x`" 1)))
        (pcase-let ((`(,line ,fragment ,offset) expectation))
          (should (eq (sh-ts-mode-test--face-in-line
                       line fragment offset)
                      'font-lock-string-face)))))))

(ert-deftest sh-ts-mode-fontifies-case-item-delimiters ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((parenthesized "  (one | two) :;;")
          (plain "  three) :"))
      (insert "case value in\n"
              parenthesized "\n"
              plain "\n"
              "esac\n")
      (sh-ts-mode-test--enable-font-lock-level 4)
      (font-lock-ensure)
      (dolist (fragment '("(" ")"))
        (should (eq (sh-ts-mode-test--face-in-line
                     parenthesized fragment)
                    'font-lock-bracket-face)))
      (should (eq (sh-ts-mode-test--face-in-line plain ")")
                  'font-lock-bracket-face))
      (should (eq (sh-ts-mode-test--face-in-line parenthesized "|")
                  'font-lock-operator-face)))))

(ert-deftest sh-ts-mode-fontifies-shell-pattern-syntax ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((line
           "  ([!a-c]|[-]|[[:alpha:]]|[[.x.]]|[[=x=]]|foo\\*) :;;"))
      (insert "case $value in\n" line "\nesac\n")
      (sh-ts-mode-test--enable-font-lock-level 4)
      (font-lock-ensure)
      (dolist (fragment '("[" "]"))
        (should (eq (sh-ts-mode-test--face-in-line line fragment)
                    'font-lock-bracket-face)))
      (should (eq (sh-ts-mode-test--face-in-line line "!")
                  'font-lock-negation-char-face))
      (should (eq (sh-ts-mode-test--face-in-line line "-")
                  'font-lock-operator-face))
      (dolist (expectation '(("[:" 0) (":]" 1)
                             ("[." 0) (".]" 1)
                             ("[=" 0) ("=]" 1)))
        (should (eq (sh-ts-mode-test--face-in-line
                     line (car expectation) (cadr expectation))
                    'font-lock-bracket-face)))
      (should (eq (sh-ts-mode-test--face-in-line line "[-]" 1)
                  'font-lock-constant-face))
      (dolist (expectation '(("[:" 1) (":]" 0)
                             ("[." 1) (".]" 0)
                             ("[=" 1) ("=]" 0)))
        (should (eq (sh-ts-mode-test--face-in-line
                     line (car expectation) (cadr expectation))
                    'font-lock-punctuation-face)))
      (should (eq (sh-ts-mode-test--face-in-line line "(")
                  'font-lock-bracket-face))
      (should (eq (sh-ts-mode-test--face-in-line line ")")
                  'font-lock-bracket-face))
      (dolist (fragment '("a" "c" "alpha" "x"))
        (should (eq (sh-ts-mode-test--face-in-line line fragment)
                    'font-lock-constant-face)))
      (should (eq (sh-ts-mode-test--face-in-line line "|")
                  'font-lock-operator-face))
      (should (eq (sh-ts-mode-test--face-in-line line "\\*")
                  'font-lock-escape-face)))))

(ert-deftest sh-ts-mode-fontifies-shell-patterns-at-level-four ()
  (sh-ts-mode-test--require-grammar)
  (dolist (level '(1 2 3 4))
    (with-temp-buffer
      (let ((assignment "value=*")
            (case-word "case * in")
            (pathname "echo *.txt [a-z]x ${x-*}")
            (active "  (foo$var\\*) :;;"))
        (insert assignment "\n" case-word "\n" active "\nesac\n"
                pathname "\n")
        (sh-ts-mode-test--enable-font-lock-level level)
        (font-lock-ensure)
        (should-not
         (sh-ts-mode-test--face-in-line assignment "*"))
        (should-not
         (sh-ts-mode-test--face-in-line case-word "*"))
        (if (= level 4)
            (progn
              (dolist (fragment '("*" "a" "z"))
                (should
                 (eq (sh-ts-mode-test--face-in-line pathname fragment)
                     'font-lock-constant-face)))
              (should
               (eq (sh-ts-mode-test--face-in-line pathname ".txt")
                   'font-lock-string-face))
              (dolist (fragment '("[" "]"))
                (should
                 (eq (sh-ts-mode-test--face-in-line pathname fragment)
                     'font-lock-bracket-face)))
              (should
               (eq (sh-ts-mode-test--face-in-line pathname "-")
                   'font-lock-operator-face))
              (should
               (eq (sh-ts-mode-test--face-in-line pathname "x-*" 2)
                   'font-lock-constant-face))
              (should
               (eq (sh-ts-mode-test--face-in-line active "foo")
                   'font-lock-string-face))
              (should
               (eq (sh-ts-mode-test--face-in-line active "$")
                   'font-lock-variable-use-face))
              (should
               (eq (sh-ts-mode-test--face-in-line active "var")
                   'font-lock-variable-use-face))
              (should
               (eq (sh-ts-mode-test--face-in-line active "\\*")
                   'font-lock-escape-face)))
          (dolist (fragment '("*" ".txt" "[" "a" "-" "z" "]"))
            (should-not
             (sh-ts-mode-test--face-in-line pathname fragment)))
          (dolist (fragment '("foo" "$" "var" "\\*"))
            (should-not
             (sh-ts-mode-test--face-in-line active fragment))))))))

(ert-deftest sh-ts-mode-updates-pattern-context-after-edits ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((pattern-line "value=${x##*}")
          (value-line "value=${x:-*}"))
      (insert pattern-line "\n")
      (sh-ts-mode-test--enable-font-lock-level 4)
      (font-lock-ensure)
      (should (eq (sh-ts-mode-test--face-in-line pattern-line "*")
                  'font-lock-constant-face))
      (goto-char (point-min))
      (search-forward "##")
      (replace-match ":-")
      (font-lock-flush (point-min) (point-max))
      (font-lock-ensure)
      (should-not (sh-ts-mode-test--face-in-line value-line "*")))))

(ert-deftest sh-ts-mode-defers-pattern-word-interiors-to-level-four ()
  (sh-ts-mode-test--require-grammar)
  (dolist (level '(3 4))
    (with-temp-buffer
      (let ((expansion "echo ${x}*.txt")
            (escape "echo f\\ o*"))
        (insert expansion "\n" escape "\n")
        (sh-ts-mode-test--enable-font-lock-level level)
        (font-lock-ensure)
        (if (= level 4)
            (progn
              (should (eq (sh-ts-mode-test--face-in-line expansion "x")
                          'font-lock-variable-use-face))
              (should (eq (sh-ts-mode-test--face-in-line escape "\\ ")
                          'font-lock-escape-face)))
          (should-not (sh-ts-mode-test--face-in-line expansion "x"))
          (should-not (sh-ts-mode-test--face-in-line escape "\\ "))))))
  (dolist (level '(2 4))
    (with-temp-buffer
      (let ((mixed "echo foo${x:-*}bar"))
        (insert mixed "\n")
        (sh-ts-mode-test--enable-font-lock-level level)
        (font-lock-ensure)
        (if (= level 4)
            (dolist (fragment '("foo" "bar"))
              (should (eq (sh-ts-mode-test--face-in-line mixed fragment)
                          'font-lock-string-face)))
          (dolist (fragment '("foo" "bar"))
            (should-not
             (sh-ts-mode-test--face-in-line mixed fragment))))))))

(ert-deftest sh-ts-mode-resets-pattern-levels-in-command-substitutions ()
  (sh-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((line "trimmed=${value#$(printf \"$nested\")}"))
      (insert line "\n")
      (sh-ts-mode-test--enable-font-lock-level 3)
      (font-lock-ensure)
      (should
       (eq (sh-ts-mode-test--face-in-line line "nested")
           'font-lock-variable-use-face)))))

(ert-deftest sh-ts-mode-fontifies-escapes-inside-shell-pattern-brackets ()
  (sh-ts-mode-test--require-grammar)
  (dolist (line '("  ([\\*]) :;;"
                  "  ([a-\\*]) :;;"
                  "  ([[:\\*:]]) :;;"
                  "  ([[.\\*.]]) :;;"
                  "  ([[=\\*=]]) :;;"))
    (with-temp-buffer
      (insert "case value in\n" line "\nesac\n")
      (sh-ts-mode-test--enable-font-lock-level 4)
      (font-lock-ensure)
      (should (eq (sh-ts-mode-test--face-in-line line "\\*")
                  'font-lock-escape-face))))
  (with-temp-buffer
    (let ((line "trimmed=${value#[\\*]}"))
      (insert line "\n")
      (sh-ts-mode-test--enable-font-lock-level 4)
      (font-lock-ensure)
      (should (eq (sh-ts-mode-test--face-in-line line "\\*")
                  'font-lock-escape-face)))))

(ert-deftest sh-ts-mode-fontifies-literal-delimiters-and-escapes ()
  (sh-ts-mode-test--require-grammar)
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
      (sh-ts-mode-test--enable-font-lock-level 4)
      (font-lock-ensure)
      (dolist (fragment '("'" "one" "\"" "two" "$'" "three"))
        (should (eq (sh-ts-mode-test--face-in-line quotes fragment)
                    'font-lock-string-face)))
      (dolist (fragment '("~" "alice"))
        (should (eq (sh-ts-mode-test--face-in-line quotes fragment)
                    'font-lock-constant-face)))
      (dolist (fragment '("\\$" "\\n" "\\ "))
        (should (eq (sh-ts-mode-test--face-in-line quotes fragment)
                    'font-lock-escape-face)))
      (should (eq (sh-ts-mode-test--face-in-line continued "\\")
                  'font-lock-punctuation-face))
      (should (eq (sh-ts-mode-test--face-in-line declaration "<<")
                  'font-lock-operator-face))
      (sh-ts-mode-test--should-fontify
       `((,declaration "EOF" font-lock-string-face)
         (,body "body" font-lock-string-face)
         (,delimiter "EOF" font-lock-string-face))))))

(ert-deftest sh-ts-mode-fontifies-by-font-lock-level ()
  (sh-ts-mode-test--require-grammar)
  (dolist (case '((1 font-lock-comment-face nil nil nil nil nil)
                  (2 font-lock-comment-face font-lock-keyword-face
                     font-lock-string-face nil nil nil)
                  (3 font-lock-comment-face font-lock-keyword-face
                     font-lock-string-face font-lock-constant-face
                     font-lock-variable-use-face nil)
                  (4 font-lock-comment-face font-lock-keyword-face
                     font-lock-string-face font-lock-constant-face
                     font-lock-variable-use-face
                     font-lock-punctuation-face)))
    (pcase-let ((`(,level ,comment-face ,keyword-face ,string-face
                          ,constant-face ,variable-face ,separator-face)
                 case))
      (with-temp-buffer
        (insert "# note\nif printf 'text' \"$value\"; then :; fi\n")
        (sh-ts-mode-test--enable-font-lock-level level)
        (font-lock-ensure)
        (sh-ts-mode-test--should-fontify
         `(("# note" "note" ,comment-face)
           ("if printf 'text' \"$value\"; then :; fi"
            "if" ,keyword-face)
           ("if printf 'text' \"$value\"; then :; fi"
            "text" ,string-face)
           ("if printf 'text' \"$value\"; then :; fi"
            "$" ,variable-face)
           ("if printf 'text' \"$value\"; then :; fi"
            "value" ,variable-face)
           ("if printf 'text' \"$value\"; then :; fi"
            ";" ,separator-face)))))))

(ert-deftest sh-ts-mode-reports-an-unavailable-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)))
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed (lambda (_) nil))
          (with-temp-buffer
            (let ((error-data (should-error (sh-ts-mode) :type 'user-error)))
              (should (string-match-p
                       "sh" (error-message-string error-data)))
              (should-not (treesit-parser-list nil nil t)))))
      (fset 'treesit-ensure-installed ensure))))

(provide 'sh-ts-mode-test)

;;; sh-ts-mode-test.el ends here
