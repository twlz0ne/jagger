;;; test-jagger-move.el --- Test jagger-move -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Gong QiJian <gongqijian@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'jagger-move)
(require 'test-util)

(defun test-jagger-move--common (fn initial expected stop-at-pattern repeat-times)
  (with-temp-buffer
    (insert initial)
    (lisp-mode)
    (goto-char (point-min))
    (re-search-forward stop-at-pattern)
    (dotimes (_ repeat-times)
      (funcall fn))
    (should (equal (buffer-substring (point-min) (point-max))
                   expected))))

(defun test-jagger-move-with-region--common (fn initial expected region-pattern repeat-times)
  (with-temp-buffer
    (insert initial)
    (lisp-mode)
    (goto-char (point-min))
    (re-search-forward region-pattern)
    (set-mark (- (point) (length region-pattern)))
    (when noninteractive
      (transient-mark-mode)) ;; MUST
    (dotimes (_ repeat-times)
      (funcall fn))
    (should (and (region-active-p)
                 (equal (buffer-substring (region-beginning) (region-end))
                        region-pattern)
                 (equal (buffer-substring (point-min) (point-max))
                        expected)))))

(ert-deftest test-jagger-move-sexp-forward ()
  (test-jagger-move--common 'jagger-move-sexp-forward
                            "(foo (qux \"quux\") bar)"
                            "((qux \"quux\") foo bar)"
                            "foo"
                            1))

(ert-deftest test-jagger-move-sexp-forward-with-region ()
  (test-jagger-move-with-region--common
   'jagger-move-sexp-forward
   "(foo (qux \"quux\") bar)"
   "((qux \"quux\") foo bar)"
   "foo"
   1))

(ert-deftest test-jagger-move-sexp-backward ()
  (test-jagger-move--common 'jagger-move-sexp-backward
                            "(foo (qux \"quux\") bar)"
                            "((qux \"quux\") foo bar)"
                            "(qux \"quux\")"
                            1))

(ert-deftest test-jagger-move-sexp-backward-with-region ()
  (test-jagger-move-with-region--common
   'jagger-move-sexp-backward
   "(foo (qux \"quux\") bar)"
   "((qux \"quux\") foo bar)"
   "(qux \"quux\")"
   1))

(ert-deftest test-jagger-move-word-forward ()
  (test-jagger-move--common 'jagger-move-word-forward
                            ";; foo-quux-bar"
                            ";; quux-foo-bar"
                            "foo"
                            1))

(ert-deftest test-jagger-move-word-backward ()
  (test-jagger-move--common 'jagger-move-word-backward
                            ";; foo-quux-bar"
                            ";; quux-foo-bar"
                            "quux"
                            1))

(require 'cl)

(cl-defun test-jagger-move-line--common (&key initial expected from step)
  (with-temp-buffer
    (insert initial)
    (goto-line from)
    (jagger-move-line step)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   expected))))

(cl-defun test-jagger-move-multiple-lines--common
    (&key initial expected region-pattern expected-region step)
  (with-temp-buffer
    (insert initial)
    (when noninteractive
      (transient-mark-mode))
    (goto-char (point-min))
    (re-search-forward region-pattern)
    (set-mark (- (point) (length region-pattern)))
    (jagger-move-line step)
    (should (and (equal (buffer-substring-no-properties (point-min) (point-max))
                        expected)
                 (equal (buffer-substring-no-properties (region-beginning) (region-end))
                        expected-region)))))

(ert-deftest test-jagger-move-line ()
  ;; move line down
  (test-jagger-move-line--common
   :initial  "foo\n(qux quux)\nbar"
   :expected "(qux quux)\nfoo\nbar"
   :from     1
   :step     1)
  (test-jagger-move-line--common
   :initial  "foo\n(qux quux)\nbar"
   :expected "(qux quux)\nbar\nfoo"
   :from     1
   :step     2)
  (test-jagger-move-line--common
   :initial  "foo\n(qux quux)\nbar"
   :expected "(qux quux)\nbar\nfoo"
   :from     1
   :step     3)

  ;; move down (with trailing newline)
  (test-jagger-move-line--common
   :initial  "foo\n(qux quux)\nbar\n"
   :expected "(qux quux)\nfoo\nbar\n"
   :from     1
   :step     1)
  (test-jagger-move-line--common
   :initial  "foo\n(qux quux)\nbar\n"
   :expected "(qux quux)\nbar\nfoo\n"
   :from     1
   :step     2)
  (test-jagger-move-line--common
   :initial  "foo\n(qux quux)\nbar\n"
   :expected "(qux quux)\nbar\nfoo\n"
   :from     1
   :step     3)

  ;; move line down
  (test-jagger-move-line--common
   :initial  "foo\n(qux quux)\nbar"
   :expected "(qux quux)\nfoo\nbar"
   :from     2
   :step     -1)
  (test-jagger-move-line--common
   :initial  "foo\n(qux quux)\nbar"
   :expected "(qux quux)\nfoo\nbar"
   :from     2
   :step     -2)

  ;; move line down (from end of buffer)
  (test-jagger-move-line--common
   :initial  "foo\n(qux quux)\nbar"
   :expected "foo\nbar\n(qux quux)"
   :from     3
   :step     -1)
  (test-jagger-move-line--common
   :initial  "foo\n(qux quux)\nbar"
   :expected "bar\nfoo\n(qux quux)"
   :from     3
   :step     -2)
  (test-jagger-move-line--common
   :initial  "foo\n(qux quux)\nbar"
   :expected "bar\nfoo\n(qux quux)"
   :from     3
   :step     -3)

  ;; move line down (from end of buffer with trailing newline)
  (test-jagger-move-line--common
   :initial  "foo\n(qux quux)\nbar\n"
   :expected "foo\nbar\n(qux quux)\n"
   :from     3
   :step     -1)
  (test-jagger-move-line--common
   :initial  "foo\n(qux quux)\nbar\n"
   :expected "bar\nfoo\n(qux quux)\n"
   :from     3
   :step     -2)
  (test-jagger-move-line--common
   :initial  "foo\n(qux quux)\nbar\n"
   :expected "bar\nfoo\n(qux quux)\n"
   :from     3
   :step     -3)

  ;; move selected lines down
  (test-jagger-move-multiple-lines--common
   :initial  "foo\nqux\nquux\nbar"
   :expected "quux\nfoo\nqux\nbar"
   :region-pattern "foo\nqux\n"
   :expected-region "foo\nqux\n"
   :step     1)
  (test-jagger-move-multiple-lines--common
   :initial  "foo\nqux\nquux\nbar"
   :expected "quux\nbar\nfoo\nqux"
   :region-pattern "foo\nqux\n"
   :expected-region "foo\nqux"
   :step     2)
  (test-jagger-move-multiple-lines--common
   :initial  "foo\nqux\nquux\nbar"
   :expected "quux\nbar\nfoo\nqux"
   :region-pattern "foo\nqux\n"
   :expected-region "foo\nqux"
   :step     3)

  ;; move selected lines up
  (test-jagger-move-multiple-lines--common
   :initial  "foo\nqux\nquux\nbar"
   :expected "foo\nquux\nbar\nqux"
   :region-pattern "quux\nbar"
   :expected-region "quux\nbar\n"
   :step     -1)
  (test-jagger-move-multiple-lines--common
   :initial  "foo\nqux\nquux\nbar"
   :expected "quux\nbar\nfoo\nqux"
   :region-pattern "quux\nbar"
   :expected-region "quux\nbar\n"
   :step     -2)
  (test-jagger-move-multiple-lines--common
   :initial  "foo\nqux\nquux\nbar"
   :expected "quux\nbar\nfoo\nqux"
   :region-pattern "quux\nbar"
   :expected-region "quux\nbar\n"
   :step     -3)
  )

(provide 'test-jagger-move)

;;; test-jagger-move.el ends here

