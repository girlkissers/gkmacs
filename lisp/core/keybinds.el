(use-package general
  :ensure (:wait t)
  :init
  (general-evil-setup)
  (general-auto-unbind-keys)

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
    :states '(motion normal insert visual emacs))
  (general-create-definer gk/evil-m-keys
    :states '(motion))
  (general-create-definer gk/evil-v-keys
    :states '(visual))
  (general-create-definer gk/evil-i-keys
    :states '(insert)))

(general-auto-unbind-keys)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'doom/escape)
  ;; (eldoc-box-hover-mode)
  ;; (setq eldoc-echo-area-use-multiline-p nil)
  (gk/evil-keys
    "K" 'eldoc-box-help-at-point))

(gk/evil-keys dired-mode-map
  "-" 'dired-up-directory)

(gk/evil-keys helpful-mode-map
  "q" 'quit-window)
(gk/evil-keys help-mode-map
  "q" 'quit-window)

(gk/evil-keys org-mode-map
  "RET" 'org-return-and-maybe-indent)

(gk/evil-keys magit-mode-map
  "h" 'evil-backward-char
  "j" 'evil-next-visual-line
  "k" 'evil-previous-line
  "l" 'evil-forward-char)

(gk/evil-m-keys prog-mode-map
  "gcc" 'evilnc-comment-or-uncomment-lines)

(gk/evil-m-keys conf-mode-map
  "gcc" 'evilnc-comment-or-uncomment-lines)

(gk/evil-v-keys prog-mode-map
  "gc" 'evilnc-comment-operator) 



;; (gk/evil-keys
;;   "gcc" 'evilnc-comment-or-uncomment-lines)

;; (gk/evil-keys
;;   "gc" 'evilnc-comment-or-uncomment-lines)

(gk/evil-i-keys
  "C-j" 'tempel-complete)


(gk/leader-keys
  "/" '(evilnc-comment-or-uncomment-lines :wk "comment/uncomment")
  "SPC" '(projectile-find-file :wk "find file in project")
  "TAB" '(:keymap persp-key-map)
  "b" '(:keymap ibuffer-mode-map)
  "bb" '(switch-to-buffer :wk "switch to buffer")
  "bi" '(ibuffer :wk "ibuffer")
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
  "o" '(:ignore t :wk "open")
  "ot" '(multi-vterm-project :wk "open project terminal")
  "p" '(:keymap projectile-command-map)
  ;; "w" '(:ignore t :wk "window")
  "w" '(:keymap evil-window-map :wk "window"))
