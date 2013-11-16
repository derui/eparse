;;; -*- coding: utf-8; lexical-binding : t -*-
;;; eparse-base.el --- A base library for eparse.el and eparse-lexer.el
;; 
;; Filename: eparse-base.el
;; Description: A base library for eparse.el and eparse-lexer.el
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

(defun eplib:return (input readsize)
  "Return next input for next lexing that use with it."
  (if (and (numberp readsize)
           (<= 0 readsize))
      (let ((pos (eplib:position-of-input input))
            (text (eplib:text-of-input input)))
        (list
         (substring text pos readsize)
         (eplib:make-input (+ readsize pos) (substring text (+ readsize pos))))
        )
    (list "" input)
  ))

(defun eplib:success (input readsize)
  "Return next input for next lexing with it, and add tag of success"
  (let ((next (eplib:return input readsize)))
    (cons 'success next))
  )

(defun eplib:fail (&rest anything)
  "Return fail tag and empty input informations.
If you use it that is returned this funciton, any lexer with fail."
  '(fail . nil))

(defun eplib:make-input (pos text)
  "Make and return input structure with initial position and text"
  (cons pos text))

(defun eplib:position-of-input (input)
  "Get position that is current reading point from text from argument."
  (and (listp input)
       (car input)))

(defun eplib:text-of-input (input)
  "Get text from argument"
  (and (listp input)
       (cdr input)))


(provide 'eparse-base)
