;;; jagger-move.el --- Moving functions -*- lexical-binding: t; -*-

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

(require 'subr-x)
(require 'jagger-util)
(require 'jagger-swap)

(defvar jagger-move--last-region-mark nil
  "Region mark for restore hook.")

(defun jagger-move--restore-region-hook ()
  (remove-hook 'deactivate-mark-hook 'jagger-move--restore-region-hook)
  (let ((reg jagger-move--last-region-mark))
    (when reg
      (if (and (fboundp 'evil-visual-state-p) (evil-visual-state-p))
          (evil-visual-select (car reg) (cdr reg))
        (goto-char (cdr reg))
        (set-mark (car reg))))))

(defun jagger-move--restore-region (beg end)
  "Restore region at new place from BEG to END.
Also add hook to `deactivate-mark-hook' and backup region."
  (add-hook 'deactivate-mark-hook 'jagger-move--restore-region-hook)
  (if (and (fboundp 'evil-visual-state-p) (evil-visual-state-p))
      (evil-visual-select beg (1- end))
    (goto-char end)
    (set-mark beg))
  (setq jagger-move--last-region-mark (car (region-bounds))))

(defun jagger-move-line (n)
  "Move the current line or region up or down by N lines."
  (interactive "p")
  (let* ((restore-region-p (region-active-p))
         (bounds (if (region-active-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'line)))
         (end-line-of-bounds (line-number-at-pos (- (cdr bounds)
                                                    (if (equal 10 (char-before (cdr bounds)))
                                                        1 0))))
         (trailing-newline (> (line-number-at-pos (point-max))
                              (line-number-at-pos (1- (point-max)))))
         (borrow-trailing (and (not trailing-newline)
                               (equal (point-max) (cdr bounds)))))
    (cond
     ((and (< n 0) (= (line-number-at-pos (car bounds))
                      (line-number-at-pos (point-min))))
      (message "Beginning fo buffer"))
     ((and (> n 0) (or (= end-line-of-bounds (line-number-at-pos (point-max)))
                       (= end-line-of-bounds (line-number-at-pos (1- (point-max))))))
      (message "End of buffer"))
     (t
      (let* ((col (current-column))
             (eob (= (point-max) (- (cdr bounds)
                                    (if (equal 10 (char-before (cdr bounds)))
                                        1 0))))
             (to-end (and (> n 0)
                          (>= (+ n (line-number-at-pos (1- (cdr bounds))))
                              (line-number-at-pos (point-max)))))
             (line (delete-and-extract-region (- (car bounds)
                                                 (if borrow-trailing 1 0))
                                              (cdr bounds)))
             ov)
        (if (and eob (< n 0))
            ;; up
            (forward-line (1+ n))
          ;;down
          (forward-line n))

        (when restore-region-p
          (setq ov (jagger-swap-regions--make-overlay
                    (point-at-bol) (point-at-bol) t)))

        ;; "\n<line>" => "<line>\n"
        (when borrow-trailing
          (setq line (concat (string-trim-left line "\n") "\n")))

        (if (and to-end (not trailing-newline))
            ;; "<line>\n" => "\n<line>"
            (insert (concat "\n" (string-trim-right line "\n")))
          (insert line))

        ;; Restore point or region
        (if ov
            (progn
              (jagger-move--restore-region (overlay-start ov)
                                           (overlay-end ov))
              (delete-overlay ov))
          (forward-line -1)
          (forward-char col)))))))

(defun jagger-move-line-up (&optional n)
  "Move the current line up by N lines."
  (interactive "p")
  (jagger-move-line (if (null n) -1 (- n))))

(defun jagger-move-line-down (&optional n)
  "Move the current line down by N lines."
  (interactive "p")
  (jagger-move-line (if (null n) 1 n)))

(defun jagger-move-thing-forward (thing)
  "Move the current THING or region forward by THING.
THING should be ‘sexp’ or ‘word’."
  (interactive)
  (condition-case err
      (let* ((reg-bound (if (region-active-p)
                            (cons (region-beginning) (region-end))))
             (cur-bound (or reg-bound
                            (jagger-util--bounds-of-thing-at-point thing)))
             (dst-bound (jagger-util--bounds-of-forward-thing thing (cdr cur-bound)))
             (ov1       (jagger-swap-regions-mark-region-1 (car cur-bound) (cdr cur-bound) t))
             (ov2       (jagger-swap-regions-mark-region-n (car dst-bound) (cdr dst-bound) t))
             (cur-point (point)))
        (unless (or (not cur-bound)
                    (not dst-bound)
                    (jagger-util--region-overlapped-p cur-bound dst-bound))
          (save-excursion
            (jagger-swap-regions--swap ov1 ov2))
          (if reg-bound
              (jagger-move--restore-region (overlay-start (cdr ov2))
                                           (overlay-end (cdr ov2)))
            (goto-char (- (cdr dst-bound)
                          (- (cdr cur-bound) cur-point))))))
    (error
     (pcase err
       (`(scan-error "Containing expression ends prematurely" . ,rest)
        (message "Already at the boundary")))))
  (jagger-swap-regions-clean-marks))

(defun jagger-move-thing-backward (thing)
  "Move the current THING or region backward by THING.
THING should be ‘sexp’ or ‘word’."
  (interactive)
  (condition-case err
      (let* ((reg-bound (if (region-active-p)
                            (cons (region-beginning) (region-end))))
             (cur-bound (or reg-bound
                            (jagger-util--bounds-of-thing-at-point thing)))
             (dst-bound (jagger-util--bounds-of-backward-thing thing (car cur-bound)))
             (ov1       (jagger-swap-regions-mark-region-1 (car cur-bound) (cdr cur-bound) t))
             (ov2       (jagger-swap-regions-mark-region-n (car dst-bound) (cdr dst-bound) t))
             (cur-point (point)))
        (unless (or (not cur-bound)
                    (not dst-bound)
                    (jagger-util--region-overlapped-p cur-bound dst-bound))
          (save-excursion
            (jagger-swap-regions--swap ov2 ov1))
          (if reg-bound
              (jagger-move--restore-region (overlay-start (cdr ov2))
                                           (overlay-end (cdr ov2)))
            (goto-char (+ (car dst-bound)
                          (- cur-point (car cur-bound)))))))
    (error
     (pcase err
       (`(scan-error "Containing expression ends prematurely" . ,rest)
        (message "Already at the boundary")))))
  (jagger-swap-regions-clean-marks))

(defun jagger-move-sexp-forward ()
  "Move the current sexp forward."
  (interactive)
  (jagger-move-thing-forward 'sexp))

(defun jagger-move-sexp-backward ()
  "Move the current sexp backward."
  (interactive)
  (jagger-move-thing-backward 'sexp))

(defun jagger-move-word-forward ()
  "Move the current word forward."
  (interactive)
  (jagger-move-thing-forward 'word))

(defun jagger-move-word-backward ()
  "Move the current word backward."
  (interactive)
  (jagger-move-thing-backward 'word))

(provide 'jagger-move)

;;; jagger-move.el ends here
