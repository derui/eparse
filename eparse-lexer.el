;;; -*- coding: utf-8; lexical-binding : t -*-
;;; eparse-lexer.el --- Lexers to be used with eparse.el
;; 
;; Filename: eparse-lexer.el
;; Description: Lexers to be used with eparse.el
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

(require 'eparse-base)

(eplib:define-lexer char source character
                    (let (ch
                          (target (cond ((characterp (car character))
                                         (car character))
                                        ((stringp (car character))
                                         (string-to-char (car character)))
                                        (t
                                         nil))))
                      (setq ch (<<- 1))
                      (if (eql (string-to-char ch) target)
                          (progn
                            (forward-pos 1)
                            (success ch))
                        (fail "no much character"))))

(eplib:define-lexer any-char source any
                    (let (ch)
                      (setq ch (<<- 1))
                      (forward-pos 1)
                      (success ch)))

(provide 'eparse-lexer)
