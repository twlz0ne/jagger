;;; jagger-util.el --- Util functions -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Gong QiJian <gongqijian@gmail.com>

;; Author: Gong QiJian <gongqijian@gmail.com>
;; Created: 2018/08/28
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/twlz0ne/jagger
;; Keywords: convenience editing

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

(defun backward-line ()
  (forward-line -1))

(defun jagger-util--bounds-of-thing-at-point (thing)
  "Determine the start and end buffer locations for the THING at point."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (let ((bounds (bounds-of-thing-at-point thing)))
      (if (and (eq thing 'line)
               (eq (char-before (cdr bounds)) 10))
          (cons (car bounds) (1- (cdr bounds)))
        bounds))))

(defun jagger-util--bounds-of-forward-thing (thing &optional start)
  "Bound of forward `THING' at point or `START'."
  (if (eq thing 'line)
      (save-excursion
        (when start
          (goto-char start))
        (when (= 0 (forward-line))
          (cons (point-at-bol) (point-at-eol))))
    (let ((forwardfn (intern (format "forward-%s" thing)))
          (backwardfn (intern (format "backward-%s" thing))))
      (save-excursion
        (when start
          (goto-char start))
        (funcall forwardfn)
        (cons (save-excursion
                (funcall backwardfn)
                (point))
              (point))))))

(defun jagger-util--bounds-of-backward-thing (thing &optional start)
  "Bound of backward `THING' at point or `START'."
  (if (eq thing 'line)
      (save-excursion
        (when start
          (goto-char start))
        (when (= 0 (backward-line))
          (cons (point-at-bol) (point-at-eol))))
    (let ((forwardfn (intern (format "forward-%s" thing)))
          (backwardfn (intern (format "backward-%s" thing))))
      (save-excursion
        (when start
          (goto-char start))
        (funcall backwardfn)
        (cons (point)
              (save-excursion
                (funcall forwardfn)
                (point)))))))

(defun jagger-util--bounds-and-sexps-at-point ()
  "Bounds of sexps at point."
  (let ((list-bound (bounds-of-thing-at-point 'list))
        (bounds-sexps '()))
    (save-excursion
      (goto-char (cdr list-bound))
      (down-list -1)
      (catch 'break
        (while t
          (condition-case err
              (backward-sexp)
            (error
             (pcase err
               (`(scan-error "Containing expression ends prematurely" . ,rest)
                (throw 'break nil)))))
          (let ((bound (bounds-of-thing-at-point 'sexp))
                (sexp (thing-at-point 'sexp t)))
            (push (cons bound sexp) bounds-sexps)))))
    bounds-sexps))

(defun jagger-util--region-overlapped-p (reg1 reg2)
  "Determine if REG1 overlapped with REG2."
  (let ((beg1 (car reg1))
        (end1 (cdr reg1))
        (beg2 (car reg2))
        (end2 (cdr reg2)))
    (not (or (and (<= end1 beg2) (<= end1 end2)) ;; (beg1 . end1) vs (beg2 . end2)
             (and (<= end2 beg1) (<= end2 end1)) ;; (beg2 . end2) vs (beg1 . end1)
             ))))

(provide 'jagger-util)

;;; jagger-util.el ends here
