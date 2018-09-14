;;; jagger-sort.el --- Sorting mode  -*- lexical-binding: t; -*-

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

(defcustom jagger-sort-mode-hook nil
  "Hooks called when jagger-sort-mode fires up."
  :type 'hook
  :group 'swap-temp)

(defvar jagger-sort-mode--line-mark nil)

(defvar jagger-sort-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-j") 'jagger-move-line-down)
    (define-key map (kbd "M-k") 'jagger-move-line-up)
    (define-key map (kbd "C-c C-c") 'jagger-sort-sexps-at-point--edit-commit)
    (define-key map (kbd "C-c C-k") 'kill-buffer-and-window)
    map)
  "Keymap for `jagger-sort-mode'.")

(defun jagger-sort-mode--move-down ()
  (interactive)
  (if (region-active-p)
      (message "move %S to %S"
               (cons (region-beginning) (region-end))
               (jagger-util--bounds-of-forward-thing 'line)
               )))

(define-derived-mode jagger-sort-mode fundamental-mode "SwapSort"
  "Major mode for reorder swap items.\\{jagger-sort-map}"
  (setq jagger-sort-mode--line-mark nil))

(defvar jagger-sort--lines-and-sexps '())

(defvar jagger-sort--bounds-and-sexps '())

(defvar jagger-sort-sexps-at-point--temp-buffer nil)

(defun jagger-sort-sexps-at-point--edit-commit ()
  (interactive)
  (let* ((buf (buffer-substring-no-properties (point-min) (point-max)))
         (sorted-lines (reverse (split-string buf "\n"))))
    (pop sorted-lines) ;; drop empty element
    (with-current-buffer (get-buffer jagger-sort-sexps-at-point--temp-buffer)
      (let ((sorted-sexps
             (mapcar (lambda (line) (alist-get line jagger-sort--lines-and-sexps nil nil 'equal)) sorted-lines)))
        (mapcar (lambda (bound-sexp)
                  (let ((bound (car bound-sexp))
                        (sexp (pop sorted-sexps)))
                    (delete-region (car bound) (cdr bound))
                    (goto-char (car bound))
                    (insert sexp)))
                (reverse jagger-sort--bounds-and-sexps))))
    (kill-buffer-and-window)))

(defun jagger-sort-sexps-at-point-in-temp-buffer ()
  "Sort sexps in current list or region."
  (interactive)
  (let* ((tmpbuf "Swap sexps*")
         (bounds-sexps (jagger-util--bounds-and-sexps-at-point))
         (lines-sexps (mapcar
                       (lambda (bound-sexp)
                         (let ((sexp (cdr bound-sexp)))
                           (cons (replace-regexp-in-string "\n[\s]+" " " (format "%s" sexp))
                                 sexp)))
                       bounds-sexps)))
    (setq jagger-sort--lines-and-sexps lines-sexps)
    (setq jagger-sort--bounds-and-sexps bounds-sexps)
    (setq jagger-sort-sexps-at-point--temp-buffer (current-buffer))
    (with-current-buffer (get-buffer-create tmpbuf)
      (erase-buffer)
      (setq jagger-sort-mode-hook '())
      (add-hook
       'jagger-sort-mode-hook
       (lambda ()
         (let ((map jagger-sort-mode-map))
           ;; (define-key map (kbd "M-j") 'jagger-move-line-down)
           ;; (define-key map (kbd "M-k") 'jagger-move-line-up)
           ;; (define-key map (kbd "C-c C-c") 'jagger-sort-sexps-at-point--edit-commit)
           ;; (define-key map (kbd "C-c C-k") 'kill-buffer-and-window)
           )
         (setq header-line-format
               (substitute-command-keys
                "Edit, then exit with `\\[jagger-sort-sexps-at-point--edit-commit]' or abort with `\\[kill-buffer-and-window]'") )))
      (jagger-sort-mode)
      (mapc (lambda (line-sexp)
              (insert (format "%s\n" (car line-sexp))))
            jagger-sort--lines-and-sexps)
      (goto-char (point-min)))
    (switch-to-buffer-other-window tmpbuf)
    ))

(provide 'jagger-sort)

;;; jagger-sort.el ends here
