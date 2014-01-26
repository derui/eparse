;;; -*- coding: utf-8; lexical-binding : t -*-
;;; eparse.el --- A parser combinator library for Emacs
;; 
;; Filename: eparse.el
;; Description: A parse combinator library for Emacs
;; Author: derui
;; Maintainer: derui
;; Copyright (C) 2013, derui, all rights reserved.
;; Created: Nov 13 2013
;; Version: 0.0.0
;; Keywords: parser library
;; Compatibility: 24.0 <=
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;; eparse provide some functions to parse and combinator to concatnate
;; functions and construct for it.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; Define parser data type. Internal construction of eparse's parser is as follows.
;; 
;; [(position . text) (parsers)]
;;

(require 'eparse-base)
(require 'eparse-lexer)

(defun eparse:make (&rest parsers)
  "Make empty parser of eparse."
  (vector '(0 . "") parsers)
  )

(defun eparse:parserp (parser)
  "Check if argument is eparse's parser or not."
  (let ((parser-body-p (lambda (x) (and (eq 'integer (type-of (car x)))
                                        (eq 'string (type-of (cdr x)))))))
    (and (vectorp parser)
         (= 2 (length parser))
         (listp (aref parser 0))
         (funcall parser-body-p (aref parser 0))
         (listp (aref parser 1))
         )
    ))

(provide 'eparse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eparse.el ends here
