;;; test-jagger-util.el --- Test jagger-util -*- lexical-binding: t; -*-

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
(require 'jagger-util)
(require 'test-util)

(ert-deftest test-jagger-util--bounds-of-thing-at-point--line-1 ()
  (with-temp-buffer
    ;;       12344567889012
    ;;       [ ]|
    (insert "foo\nbar\nquux")
    (goto-char 0)
    (should
     (equal (jagger-util--bounds-of-thing-at-point 'line)
            (cons 1 4)))))

(ert-deftest test-jagger-util--bounds-of-thing-at-point--line-2 ()
  (with-temp-buffer
    ;;       12344567889012
    ;;            [ ]|
    (insert "foo\nbar\nquux")
    (goto-char 5)
    (should
     (equal (jagger-util--bounds-of-thing-at-point 'line)
            (cons 5 8)))))

(ert-deftest test-jagger-util--bounds-of-thing-at-point--line-n ()
  (with-temp-buffer
    ;;       123445678890123
    ;;                 [  ]|
    (insert "foo\nbar\nquux")
    (goto-char 9)
    (should
     (equal (jagger-util--bounds-of-thing-at-point 'line)
            (cons 9 13)))))

(defun test-jagger-util--bounds-of-line-at-point--with-region--common
    (initial expected region-pattern fn)
  (with-temp-buffer
    (insert initial)
    (goto-char (point-min))
    (re-search-forward region-pattern)
    (when noninteractive
      (transient-mark-mode)) ;; MUST
    (set-mark (- (point) (length region-pattern)))
    (should
     (equal expected
            (funcall fn 'line (if (eq fn 'jagger-util--bounds-of-forward-thing)
                                  (region-end)
                                (region-beginning)))))))

(ert-deftest test-jagger-util--bounds-of-thing-at-point--line-with-region-1 ()
  ;; backward

  (test-jagger-util--bounds-of-line-at-point--with-region--common
   ;;234566789012345677890123
   "(foo)\n(qux quux)\n(bar)"
   (cons 1 6)
   "(qux quux)\n(bar)"
   'jagger-util--bounds-of-backward-thing)

  (test-jagger-util--bounds-of-line-at-point--with-region--common
   ;;234566789012345677890123
   "(foo)\n(qux quux)\n(bar)\n"
   (cons 1 6)
   "(qux quux)\n(bar)\n"
   'jagger-util--bounds-of-backward-thing)

  (test-jagger-util--bounds-of-line-at-point--with-region--common
   ;;234566789012345677890123
   "(foo)\n(qux quux)\n(bar)"
   nil
   "(foo)\n(qux quux)\n(bar)"
   'jagger-util--bounds-of-backward-thing)

  ;; forward

  (test-jagger-util--bounds-of-line-at-point--with-region--common
   ;;234566789012345677890123
   "(foo)\n(qux quux)\n(bar)"
   (cons 18 23)
   "(foo)\n(qux quux)"
   'jagger-util--bounds-of-forward-thing)

  (test-jagger-util--bounds-of-line-at-point--with-region--common
   ;;234566789012345677890123
   "(foo)\n(qux quux)\n(bar)"
   (cons 18 23)
   "(foo)\n(qux quux)\n"
   'jagger-util--bounds-of-forward-thing)

  (test-jagger-util--bounds-of-line-at-point--with-region--common
   ;;234566789012345677890123
   "(foo)\n(qux quux)\n(bar)"
   nil
   "(foo)\n(qux quux)\n(bar)"
   'jagger-util--bounds-of-forward-thing))

(ert-deftest test-jagger-util--bounds-and-sexps-at-point-1 ()
  (with-temp-buffer
    ;;       123456789012345678901234567
    ;;        [  ][                ][  ]
    (insert "(foo (abc def ghi jkl) bar)")
    (lisp-mode)
    (goto-char (point-min))
    (re-search-forward "bar")
    (should
     (equal (jagger-util--bounds-and-sexps-at-point)
            '(((2 . 5) . "foo") ((6 . 23) . "(abc def ghi jkl)") ((24 . 27) . "bar"))))))

(ert-deftest test-jagger-util--bounds-and-sexps-at-point-2 ()
  (with-temp-buffer
    ;;       1234567890123456789012
    ;;             [  ][  ][  ][  ]
    (insert "(foo (abc def ghi jkl) bar)")
    (lisp-mode)
    (goto-char (point-min))
    (re-search-forward "ghi")
    (should
     (equal (jagger-util--bounds-and-sexps-at-point)
            '(((7 . 10) . "abc") ((11 . 14) . "def") ((15 . 18) . "ghi") ((19 . 22) . "jkl"))))))

(ert-deftest tset-jagger-util--region-overlapped-p ()
  (should-not (jagger-util--region-overlapped-p '(1 . 2) '(3 . 4)))
  (should-not (jagger-util--region-overlapped-p '(3 . 4) '(1 . 2)))

  (should-not (jagger-util--region-overlapped-p '(1 . 2) '(2 . 4)))
  (should-not (jagger-util--region-overlapped-p '(2 . 4) '(1 . 2)))

  (should-not (jagger-util--region-overlapped-p '(1 . 4) '(4 . 4)))
  (should-not (jagger-util--region-overlapped-p '(4 . 4) '(1 . 4)))

  (should-not (jagger-util--region-overlapped-p '(1 . 1) '(1 . 4)))
  (should-not (jagger-util--region-overlapped-p '(1 . 4) '(1 . 1)))

  (should (jagger-util--region-overlapped-p '(1 . 3) '(2 . 4)))
  (should (jagger-util--region-overlapped-p '(2 . 4) '(1 . 3)))

  (should (jagger-util--region-overlapped-p '(1 . 4) '(2 . 3)))
  (should (jagger-util--region-overlapped-p '(2 . 3) '(1 . 4)))

  (should (jagger-util--region-overlapped-p '(1 . 4) '(2 . 4)))
  (should (jagger-util--region-overlapped-p '(2 . 4) '(1 . 4)))

  (should (jagger-util--region-overlapped-p '(1 . 4) '(1 . 4)))
  (should (jagger-util--region-overlapped-p '(1 . 4) '(1 . 4))))

(provide 'test-jagger-util)

;;; test-jagger-util.el ends here
