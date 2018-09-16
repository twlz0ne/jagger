;;; jagger-swap.el --- Swapping functions -*- lexical-binding: t; -*-

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

(require 'jagger-util)

(defvar jagger-swap-regions-mark-face '(:inherit isearch) "Face of marked region.")

(defvar jagger-swap-regions--marks '() "Region mark list.")

(defun jagger-swap-regions--make-overlay (begin end &optional highlight-p)
  "Make overlay of region from `BEGIN' to `END'.
If `HIGHLIGHT-P' not nil, the region needs to be highlighted."
  (interactive "r")
  (let ((ov (make-overlay begin end nil nil t)))
    (when highlight-p
      (overlay-put ov 'face jagger-swap-regions-mark-face))
    ov))

(defun jagger-swap-regions--marked-content (mark)
  "Return no properties content from `MARK'."
  (let* ((begin (overlay-start (cdr mark)))
         (end (overlay-end (cdr mark))))
    (with-current-buffer (car mark) (buffer-substring-no-properties begin end))))

(defun jagger-swap-regions--swap (mark-A mark-B)
  "Swap region `MARK-A' with region `MARK-B'."
  (save-excursion
    (let* ((A-begin (overlay-start (cdr mark-A)))
           (A-end (overlay-end (cdr mark-A)))
           (A-str (with-current-buffer (car mark-A) (buffer-substring-no-properties A-begin A-end)))
           (B-begin (overlay-start (cdr mark-B)))
           (B-end (overlay-end (cdr mark-B)))
           (B-str (with-current-buffer (car mark-B) (buffer-substring-no-properties B-begin B-end))))
      (with-current-buffer (car mark-B)
        (delete-region B-begin B-end)
        (goto-char B-begin)
        (insert A-str))
      (with-current-buffer (car mark-A)
        (delete-region A-begin A-end)
        (goto-char A-begin)
        (insert B-str)))))

(defun jagger-swap-regions-clean-marks ()
  "Clean all marsk."
  (interactive)
  (while (car jagger-swap-regions--marks)
    (delete-overlay (cdr (pop jagger-swap-regions--marks)))))

(defun jagger-swap-regions-mark-region-1 (reg-begin reg-end &optional highlight-p)
  "Mark region from `REG-BEGIN' to `REG-END' as the first mark.
If `HIGHLIGHT-P' not nil, the region needs to be highlighted."
  (jagger-swap-regions-clean-marks)
  (let* ((ov (jagger-swap-regions--make-overlay reg-begin reg-end highlight-p))
         (mk (cons (buffer-name) ov)))
    (push mk jagger-swap-regions--marks)
    mk))

(defun jagger-swap-regions-mark-region-n (reg-begin reg-end &optional highlight-p)
  (let* ((ov (jagger-swap-regions--make-overlay reg-begin reg-end highlight-p))
         (mk (cons (buffer-name) ov)))
    (push mk jagger-swap-regions--marks)
    mk))

(defun jagger-swap-regions-append-mark ()
  "Add current region to mark list."
  (interactive)
  (when (region-active-p)
    (jagger-swap-regions-mark-region-n (region-beginning) (region-end) t)
    (deactivate-mark)))

(defun jagger-swap-regions-mark-region ()
  "Set current region as the first mark."
  (interactive)
  (when (region-active-p)
    (jagger-swap-regions-mark-region-1 (region-beginning) (region-end) t)
    (deactivate-mark)))

(defun jagger-swap-regions ()
  "Swap current region with latest."
  (interactive)
  (when (region-active-p)
    (let* ((mark-A (car jagger-swap-regions--marks)))
      (when mark-A
        (let* ((ovl-B (jagger-swap-regions--make-overlay (region-beginning) (region-end)))
               (ovl-B-begin (overlay-start ovl-B))
               (ovl-B-end (overlay-end ovl-B))
               (ovl-A (cdr mark-A))
               (ovl-A-begin (overlay-start ovl-A))
               (ovl-A-end (overlay-end ovl-A))
               (swapped t))
          (if (eq (car mark-A) (buffer-name))
              (cond
               ((>= ovl-B-begin ovl-A-end)
                (jagger-swap-regions--swap mark-A (cons (buffer-name) ovl-B)))
               ((<= ovl-B-end ovl-A-begin)
                (jagger-swap-regions--swap (cons (buffer-name) ovl-B) mark-A))
               (t (progn
                    (setq swapped nil))))
            (jagger-swap-regions--swap (cons (buffer-name) ovl-B) mark-A))
          (delete-overlay ovl-B)
          (when swapped
            (deactivate-mark)
            (jagger-swap-regions-clean-marks)))))))

(defun jagger-swap-things-1 (bound1 bound2)
  "Swap `BOUND1' with `BOUND2'."
  (interactive)
  (let ((beg1 (car bound1))
        (end1 (cdr bound1))
        (beg2 (car bound2))
        (end2 (cdr bound2)))
    (if (or (and (< beg2 beg1) (< beg1 end2))
	    (and (< beg1 beg2) (< beg2 end1)))
        (error "Unable to swap overlapping regions")
      (save-excursion
	(insert 
	 (prog1 (delete-and-extract-region beg2 end2)
	   (goto-char beg2)
	   (insert 
	    (delete-and-extract-region beg1 end1))
	   (goto-char beg1)))))))

(defun jagger-swap-things (thing)
  "Swap THINGs around point or region that from `BEG' to `END'."
  (interactive)
  (if (region-active-p)
       (jagger-swap-things-1 (jagger-util--bounds-of-backward-thing thing (region-beginning))
                     (jagger-util--bounds-of-forward-thing thing (region-end)))
    (jagger-swap-things-1 (jagger-util--bounds-of-backward-thing thing)
                  (jagger-util--bounds-of-forward-thing thing))))

(defun jagger-swap-words ()
  "Swap words around point or region that from `BEG' to `END'."
  (interactive)
  (jagger-swap-things 'word))

(defun jagger-swap-sexps ()
  "Swap sexps around point or region that from `BEG' to `END'."
  (interactive)
  (jagger-swap-things 'sexp))

(defun jagger-swap-lines ()
  "Swap lines around point or region that from `BEG' to `END'."
  (interactive)
  (jagger-swap-things 'line))

(provide 'jagger-swap)

;;; jagger-swap.el ends here
