;;; web.el --- Web development -*- lexical-binding: t -*-

;; Author: rondDev
;; Version: 0.0.1
;; Package-Requires: elpaca
;; Homepage: https://github.com/girlkissers/gkmacs
;; Keywords: programming

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(defun gk-init/web-mode ()
  (use-package emmet-mode)
  :init
  (add-hook 'mhtml-mode-hook #'(lambda () (emmet-mode))))

;; ASTRO
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
             '(astro-mode . ("astro-ls" "--stdio"
                          :initializationOptions
                          (:typescript (:tsdk "./node_modules/typescript/lib"))))))


(use-package web-mode)
(add-hook 'html-mode-hook #'gk-init/web-mode)
(add-hook 'astro-mode-hook #'gk-init/astro-mode)

(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode))
              auto-mode-alist))

;;(provide 'web)
;;; web.el ends here
