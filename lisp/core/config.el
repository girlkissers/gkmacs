;;; core/config.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author:  <contact@rond.cc>
;; Maintainer:  <contact@rond.cc>
;; Created: September 21, 2024
;; Modified: September 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/rondDev/gkmacs
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(use-package affe
  :after consult
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-."))

(use-package avy)

(use-package anzu
  :init
  (global-anzu-mode +1))

(use-package better-defaults)

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))
;; (add-hook 'completion-at-point-functions #'cape-history)
;; ...


(use-package corfu
  :init
  (setq corfu-auto t
        corfu-quit-no-match t)
  (global-corfu-mode))

(use-package consult
  :ensure (:wait t))

(use-package dashboard
  :ensure t
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (setq dashboard-item-shortcuts '((recents   . "r")
                                   (bookmarks . "m")
                                   (projects  . "p")
                                   (agenda    . "a")
                                   (registers . "e")))
  (dashboard-setup-startup-hook))

(use-package dumb-jump)

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package evil
  :ensure (:wait t)
  :init
  (setq evil-want-keybinding nil)
  :config
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))

  (add-to-list 'evil-motion-state-modes 'dired-mode)
  (add-to-list 'evil-motion-state-modes 'Custom-mode)
  (add-to-list 'evil-motion-state-modes 'ibuffer-mode)
  (setq evil-kill-on-visual-paste nil)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :init
  :custom (evil-collection-setup-minibuffer t)
  (evil-collection-init)
  (evil-collection-ibuffer-setup))



(use-package evil-goggles)

(use-package evil-nerd-commenter
  :init
  (evilnc-default-hotkeys))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package general
  :ensure (:wait t)
  :config
  (general-evil-setup)

  (general-define-key
   :states '(normal visual motion)
   "L" 'evil-end-of-line
   "H" 'evil-first-non-blank)

  (general-create-definer gk/leader-keys
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "M-SPC")
  (general-create-definer gk/local-leader-keys
    :states '(normal insert visual emacs)
    :prefix ",")
  (general-create-definer gk/evil-keys
    :states '(normal insert visual emacs)))

(use-package helpful)

(use-package projectile)

(use-package magit
  :ensure t)

(use-package marginalia
  :init
  (marginalia-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode))

(use-package vertico
  :ensure (:wait t)
  :custom
  (vertico-cycle t)
  (vertico-count 20)
  (vertico-resize nil)
  :init
  (vertico-mode)
  (savehist-mode)

  (dolist (elt ido-minor-mode-map-entry)
    (when (and (listp elt) (eq (car elt) 'remap))
      (setf (cddr elt) (assq-delete-all 'find-file (cddr elt))))))

(use-package vertico-posframe
  :after vertico
  :config
  (vertico-posframe-mode 1)
  (setq vertico-multiform-commands
        '((consult-line
           posframe
           (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
           (vertico-posframe-border-width . 10)
           ;; NOTE: This is useful when emacs is used in both in X and
           ;; terminal, for posframe do not work well in terminal, so
           ;; vertico-buffer-mode will be used as fallback at the
           ;; moment.
           (vertico-posframe-fallback-mode . vertico-buffer-mode))
          (t posframe)))
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (setq vertico-posframe-border-width 3)
  (vertico-multiform-mode 1))
;; keyboard-escape-quit
;; (abort-recursive-edit)

(use-package which-key
  :config
  (which-key-mode 1))

;; (gk/local-leader-keys
;;  "m" 'test-rond)

(gk/evil-keys dired-mode-map
              "-" 'dired-up-directory)

(gk/leader-keys
 "SPC" '(projectile-find-file :wk "find file in project")
 "b" '(:keymap ibuffer-mode-map)
 "f" '(:ignore t :wk "file")
 "fd" '(dired-jump :wk "open dired")
 "ff" '(find-file :wk "find file")
 "fs" '(save-buffer :wk "save file")
 ;; "h" '(:ignore t :wk "help")
 "h" '(:keymap help-map :wk "help")
 ;; "hf" '(describe-function :wk "describe function")
 ;; "hk" '(helpful-key :wk "describe key")
 ;; "hv" '(describe-variable :wk "describe variable")
 ;; "hm" '(describe-mode :wk "describe mode")
 "p" '(:keymap projectile-command-map)
 ;; "w" '(:ignore t :wk "window")
 "w" '(:keymap evil-window-map :wk "window"))
