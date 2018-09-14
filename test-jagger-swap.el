;;; test-jagger-swap.el --- tests of jagger-swap -*- lexical-binding: t; -*-

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
(require 'jagger-swap)
(require 'test-util)

(defun test-jagger-swap-regions--set-mark (buffer content mark-fn &optional erase-p)
  "Fill `BUFFER' with `CONTENT' and call `MARK-FN' to set mark.
The `CONTENT' must contain a string surround by `[]', example:

        [foo] bar

If ERASE-P not nil, erase the buffer."
  (with-current-buffer (get-buffer-create buffer)
    (if erase-p
        (erase-buffer)
      (goto-char (point-max)))
    ;; (when (region-active-p)
      (deactivate-mark)
      ;; )
    (insert content)
    (goto-char (point-min))
    (re-search-forward "\\[\\([^]]+\\)\\]" nil t 1)
    (replace-match (format "%s" (match-string 1)))
    (goto-char (match-beginning 0))
    (set-mark (match-end 0))
    (when noninteractive
      (transient-mark-mode)) ;; MUST
    (funcall-interactively mark-fn)
    ))

(ert-deftest test-jagger-swap-regions ()
  (test-jagger-swap-regions--set-mark "buf0" "[foo] " 'jagger-swap-regions-mark-region t)
  (test-jagger-swap-regions--set-mark "buf0" "[bar]" 'jagger-swap-regions)
  (should
   (equal "bar foo" (with-current-buffer "buf0" (buffer-string)))))

(ert-deftest test-jagger-swap-regions-cross-buffer ()
  (test-jagger-swap-regions--set-mark "buf1" "foo [bar]" 'jagger-swap-regions-mark-region t)
  (test-jagger-swap-regions--set-mark "buf2" "[foo] bar" 'jagger-swap-regions t)
  (should
   (and (equal "foo foo" (with-current-buffer "buf1" (buffer-string)))
        (equal "bar bar" (with-current-buffer "buf2" (buffer-string))))))

(defun test-jagger-sort-sexps-at-point--common (init-data expected-data stop-at-pattern line-down-n)
  "Common code of test jagger-sort-sexps-at-point-in-temp-buffer.
INIT-DATA EXPECTED-DATA STOP-AT-PATTERN LINE-DOWN-N."
  (with-current-buffer (get-buffer-create "*.el")
    (erase-buffer)
    (insert init-data)
    (lisp-mode)
    (goto-char (point-min))
    (re-search-forward stop-at-pattern)
    (jagger-sort-sexps-at-point-in-temp-buffer)
    (jagger-move-line-down line-down-n)
    (jagger-sort-sexps-at-point--edit-commit))
  (with-current-buffer (get-buffer "*.el")
    (let ((actual (replace-regexp-in-string "\n[\s]+" " " (buffer-substring-no-properties (point-min) (point-max)))))
      (should (equal actual expected-data)))))

(ert-deftest test-jagger-sort-sexps-at-point-1 ()
  (test-jagger-sort-sexps-at-point--common
   "(foo (qux
      \"quux\")
     bar)"
   "((qux \"quux\") bar foo)"
   "bar"
   2))

(ert-deftest test-jagger-sort-sexps-at-point-2 ()
  (test-jagger-sort-sexps-at-point--common
   "(foo (qux
      \"quux\")
     bar)"
   "(foo (\"quux\" qux) bar)"
   "quux"
   1))

(provide 'test-jagger-swap)

;;; test-jagger-swap.el ends here
