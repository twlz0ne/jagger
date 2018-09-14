;;; jagger.el --- Move/swap things more convenient in Emacs -*- lexical-binding: t; -*-

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

;;; Commentary:

;; This package provides some functions to make moving/swapping things
;; (including regions, sexps, lines, words) more convenient in Emacs.
;; 
;; For a basic overview, see readme at
;; https://github.com/twlz0ne/jagger

;;; Change Log:

;;  0.1.0  2018/08/28  Initial version.

;;; Code:

(defgroup jagger nil
  "Jagger settings."
  :group 'jagger)

(require 'jagger-util)
(require 'jagger-swap )
(require 'jagger-move )
(require 'jagger-sort)

(provide 'jagger)
