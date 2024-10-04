;;; config.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author:  <contact@rond.cc>
;; Maintainer:  <contact@rond.cc>
;; Created: September 21, 2024
;; Modified: September 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/rondDev/config
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package apheleia
  :init
  (apheleia-global-mode +1))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(provide 'config)
;;; config.el ends here
