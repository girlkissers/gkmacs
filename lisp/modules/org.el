(defun gk//load-org ()
  "Load org mode and co"
  (use-package org-super-agenda)
  (use-package denote
    :init
    (setq denote-directory (file-truename "~/org")))


  (use-package ob-mermaid
    :defer t
    :config
    (add-to-list 'org-babel-load-languages '(mermaid . t))
    (setq ob-mermaid-cli-path "/usr/bin/mmdc"))

  (use-package org-modern
    :ensure t
    :custom
    (org-modern-hide-stars nil) ; adds extra indentation
    (org-modern-table t)
    (org-modern-list 
     '((?- . "-")
       (?* . "•")
       (?+ . "‣")))
    (org-modern-block-name '("" . "")) ; or other chars; so top bracket is drawn promptly
    :hook
    ;; (org-mode . org-modern-mode)
    (org-agenda-finalize . org-modern-agenda))

  ;; (use-package org-superstar)
  ;; (use-package org-fancy-priorities)


  (with-eval-after-load 'org
    (setq org-return-follows-link  t)
    (setq org-todo-keywords
          '((sequence "TODO(t!)" "PROJ(p!)" "LOOP(r!)" "STRT(s!)" "WAIT(w!)" "HOLD(h!)" "IDEA(i!)" "|" "DONE(d!)" "KILL(k!)")
            (sequence "[ ](T!)" "[-](S!)" "[?](W!)" "|" "[X](D!)")
            (sequence "|" "OKAY(o!)" "YES(y!)" "NO(n!)"))))

  (use-package org-roam
    :defer t
    :init
    (org-roam-db-autosync-mode)
    (setq org-roam-directory (file-truename "~/org")))


  (use-package org-bullets
    :init
    (add-hook 'org-mode-hook #'org-bullets-mode))

  (add-hook 'org-mode-hook #'org-indent-mode))


;; (add-hook 'on-first-file-hook gk//load-org)

(gk//load-org)
