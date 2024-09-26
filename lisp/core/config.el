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

;; (use-package consult-projectile
;;   :ensure (:wait t)
;;   :after projectile)

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

(use-package denote)

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
  (setq evil-undo-system 'undo-fu)
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

(use-package helpful
  :defer t)



(use-package ghub
  :defer t)
(use-package magit
  :defer t
  :init
  (add-hook 'git-commit-mode-hook 'evil-insert-state))




(use-package marginalia
  :init
  (marginalia-mode))

(use-package nerd-icons)

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

  ;; Optionally:
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          ;; ...
          (t :style "cod" :icon "code" :face font-lock-warning-face))))
;; Remember to add an entry for `t', the library uses that as default.

;; The Custom interface is also supported for tuning the variable above.


(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package ob-mermaid
  :defer t
  :config
  (add-to-list 'org-babel-load-languages '(mermaid . t))
  (setq ob-mermaid-cli-path "/usr/bin/mmdc"))

(use-package org-modern
  :init
  (with-eval-after-load 'org (global-org-modern-mode)))

;; (use-package org-superstar)
;; (use-package org-fancy-priorities)

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(with-eval-after-load 'org
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "PROJ(p!)" "LOOP(r!)" "STRT(s!)" "WAIT(w!)" "HOLD(h!)" "IDEA(i!)" "|" "DONE(d!)" "KILL(k!)")
          (sequence "[ ](T!)" "[-](S!)" "[?](W!)" "|" "[X](D!)")
          (sequence "|" "OKAY(o!)" "YES(y!)" "NO(n!)"))))

(use-package org-roam
  :defer t
  :init
  (setq org-roam-directory (file-truename "~/org")))


(use-package page-break-lines
  :init
  (page-break-lines-mode))

(use-package parinfer-rust-mode
  :hook emacs-lisp-mode)

(use-package persistent-scratch
  :init
  (persistent-scratch-setup-default))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package projectile
  :init
  (setq projectile-project-search-path '("~/external/" "~/internal/" ("~/projects" . 2))
        projectile-enable-caching t)
  (projectile-mode))

(use-package pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line)
  ;; integration with the `consult' package:
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)

  ;; integration with the built-in `imenu':
  (add-hook 'imenu-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry)
  (pulsar-global-mode 1))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;; Rainbow mode for colour previewing (rainbow-mode.el)
(use-package rainbow-mode
  :ensure t
  :init
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil)

  (defun prot/rainbow-mode-in-themes ()
    (when-let ((file (buffer-file-name))
               ((derived-mode-p 'emacs-lisp-mode))
               ((string-match-p "-theme" file)))
      (rainbow-mode 1)))
  :bind ( :map ctl-x-x-map
          ("c" . rainbow-mode)) ; C-x x c
  :hook (emacs-lisp-mode . prot/rainbow-mode-in-themes))


(use-package smartparens
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

;;;; Increase padding of windows/frames
;; <https://protesilaos.com/codelog/2023-06-03-emacs-spacious-padding/>.
(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :hook (after-init . spacious-padding-mode)
  :bind ("<f8>" . spacious-padding-mode)
  :init
  ;; These are the defaults, but I keep it here for visiibility.
  (setq spacious-padding-widths
        '( :internal-border-width 30
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :left-fringe-width 20
           :right-fringe-width 20))

  ;; (setq spacious-padding-subtle-mode-line
  ;;       `( :mode-line-active ,(if (or (eq prot-emacs-load-theme-family 'modus)
  ;;                                     (eq prot-emacs-load-theme-family 'standard))
  ;;                                 'default
  ;;                               'help-key-binding)
  ;;          :mode-line-inactive window-divider))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as
  ;; it is very flexible.
  (setq spacious-padding-subtle-mode-line '(:mode-line-active "#37f499" :mode-line-inactive shadow)))

(use-package transient)

(use-package undo-fu)

(use-package undo-fu-session
  :init
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

;; (use-package vertico-posframe
;;   :after vertico
;;   :config
;;   (vertico-posframe-mode 1)
;;   (setq vertico-multiform-commands
;;         '((consult-line
;;            posframe
;;            (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
;;            (vertico-posframe-border-width . 10)
;;            ;; NOTE: This is useful when emacs is used in both in X and
;;            ;; terminal, for posframe do not work well in terminal, so
;;            ;; vertico-buffer-mode will be used as fallback at the
;;            ;; moment.
;;            (vertico-posframe-fallback-mode . vertico-buffer-mode))
;;           (t posframe)))
;;   (setq vertico-posframe-parameters
;;         '((left-fringe . 8)
;;           (right-fringe . 8)))
;;   (setq vertico-posframe-border-width 3)
;;   (vertico-multiform-mode 1))
;; keyboard-escape-quit
;; (abort-recursive-edit)

(use-package wakatime-mode
  :init
  (global-wakatime-mode))

(use-package which-key
  :config
  (which-key-mode 1))

;; (gk/local-leader-keys
;;  "m" 'test-rond)

(general-auto-unbind-keys)





;;; Universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like diff-hl and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar doom-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun doom/escape (&optional interactive)
  "Run `doom-escape-hook'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           ;; quit the minibuffer if open.
           (when interactive
             (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
          ;; Run all escape hooks. If any returns non-nil, then stop there.
          ((run-hook-with-args-until-success 'doom-escape-hook))
          ;; don't abort macros
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ;; Back to the default
          ((unwind-protect (keyboard-quit)
             (when interactive
               (setq this-command 'keyboard-quit)))))))

(global-set-key [remap keyboard-quit] #'doom/escape)

;;;###autoload
(defun +evil-escape-a (&rest _)
  "Call `doom/escape' if `evil-force-normal-state' is called interactively."
  (when (called-interactively-p 'any)
    (call-interactively #'doom/escape)))

(advice-add #'evil-force-normal-state :after #'+evil-escape-a)
(advice-add #'evil-force-normal-state :after #'anzu--reset-status)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'doom/escape))

(defadvice split-window (after split-window-after activate)
  (other-window 1))

(gk/evil-keys dired-mode-map
  "-" 'dired-up-directory)

(gk/evil-keys magit-mode-map
  "h" 'evil-backward-char
  "j" 'evil-next-visual-line
  "k" 'evil-previous-line
  "l" 'evil-forward-char)

;; (gk/evil-keys
;;   "gcc" 'evilnc-comment-or-uncomment-lines)

;; (gk/evil-keys
;;   "gc" 'evilnc-comment-or-uncomment-lines)

(gk/leader-keys
  "SPC" '(projectile-find-file :wk "find file in project")
  "b" '(:keymap ibuffer-mode-map)
  "bb" '(switch-to-buffer :wk "switch to buffer")
  "f" '(:ignore t :wk "file")
  "fd" '(dired-jump :wk "open dired")
  "ff" '(find-file :wk "find file")
  "fs" '(save-buffer :wk "save file")
  ;; "h" '(:ignore t :wk "help")
  "h" '(:keymap help-map :wk "help")
  "hf" '(helpful-callable :wk "describe function")
  "hk" '(helpful-key :wk "describe key")
  "hv" '(helpful-variable :wk "describe variable")
  "hm" '(describe-mode :wk "describe mode")
  "hx" '(helpful-command :wk "describe mode")
  "gg" '(magit-status :wk "magit")
  "p" '(:keymap projectile-command-map)
  ;; "w" '(:ignore t :wk "window")
  "w" '(:keymap evil-window-map :wk "window"))
