;;; lsp.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author:  <contact@rond.cc>
;; Maintainer:  <contact@rond.cc>
;; Created: September 21, 2024
;; Modified: September 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/rondDev/lsp
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(with-eval-after-load 'eglot
  (gk/leader-keys
   "ca" '(eglot-code-actions :wk "code actions")))
