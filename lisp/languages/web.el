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

(elpaca (lsp-biome :host github :repo "cxa/lsp-biome"))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; pimeys/emacs-prisma-mode/blob/main/prisma-mode.el
(setq prisma-font-lock-keywords
      (let* (
             ;; We match `model Album {', and highlight `model' as keyword and `Album' as type.
             ;; Same rules for `enum`, `datasource` and `type'.
             (x-keywords-regexp "^\s*\\(model\\|enum\\|datasource\\|generator\\|type\\)\s+\\([a-zA-Z0-9_-]+\\)\s*{")
             ;; Mathces the column name and type, hilighting the type.
             (x-scalar-types-regexp "^\s+[a-zA-Z0-9_-]+\s+\\(Int\\|String\\|Boolean\\|DateTime\\|Float\\|Decimal\\|Json\\|[a-zA-Z0-9_-]+\\)")
             ;; A field attribute, such as `@id' or `@map', comes after the column type.
             (x-field-attributes-regexp "\@\\(id\\|map\\|default\\|relation\\|unique\\|ignore\\)")
             ;; A block attribute, usually at the end of a block such as model definition.
             ;; Example: `@@id([user_name, email])'
             (x-block-attributes-regexp "\@@\\(id\\|map\\|unique\\|index\\|ignore\\|fulltext\\)")
             ;; A native type definition, such as `@db.VarChar(255)'
             (x-native-types-regexp "\@[a-zA-Z0-9_-]+\.[a-zA-Z]+")
             ;; Properties in an attribute, e.g. `fields: [MediaTypeId]'.
             (x-properties-regexp "[a-zA-Z_-]+:")
             ;; Builtin functions. E.g. `autoincrement()'
             (x-attribute-functions-regexp "\\(autoincrement\\|cuid\\|uuid\\|now\\|env\\|dbgenerated\\)\(\.*\)")
             ;; Constants
             (x-constants-regexp "\\(true\\|false\\|null\\)"))
        
        `(
          ;; order matters
          (,x-block-attributes-regexp . font-lock-preprocessor-face)
          (,x-field-attributes-regexp . font-lock-preprocessor-face)
          (,x-attribute-functions-regexp . (1 font-lock-function-name-face))
          (,x-native-types-regexp . font-lock-preprocessor-face)
          (,x-keywords-regexp (1 font-lock-keyword-face) (2 font-lock-type-face))
          (,x-properties-regexp . font-lock-variable-name-face)
          (,x-scalar-types-regexp . (1 font-lock-type-face))
          (,x-constants-regexp . font-lock-constant-face))))


;;;###autoload
(define-derived-mode prisma-mode js-mode "Prisma"
  "Major mode for editing Prisma data models."

  (setq-default indent-tabs-mode nil)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq c-syntactic-indentation nil)
  (setq js-indent-level 2)

  ;; HACK: dont indent after <type>[?!]
  (setq-local js--indent-operator-re "")
  (setq font-lock-defaults '((prisma-font-lock-keywords)))
  (when (eq (bound-and-true-p imenu-create-index-function) 'js--imenu-create-index)
    (setq-local imenu-generic-expression
                '((nil "^\\s-*\\(?:model\\|enum\\)\\s-+\\([[:alnum:]]+\\)\\s-*{" 1)))
    (add-function :before-until (local 'imenu-create-index-function)
                  #'imenu-default-create-index-function)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.prisma$" . prisma-mode))

;;; pimeys/emacs-prisma-mode/blob/main/prisma-mode.el



(use-package web-mode)
(add-hook 'html-mode-hook #'gk-init/web-mode)
;; (add-hook 'astro-mode-hook #'gk-init/astro-mode)

(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode))
              auto-mode-alist))


(define-derived-mode svelte-mode web-mode "svelte")
(setq auto-mode-alist
      (append '((".*\\.svelte\\'" . svelte-mode))
              auto-mode-alist))


(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(astro-mode . ("astro-ls" "--stdio"
                               :initializationOptions
                               (:typescript (:tsdk "./node_modules/typescript/lib"))))
               '(svelte-mode . ("svelteserver" "--stdio"))))


(provide 'web)
;;; web.el ends here
