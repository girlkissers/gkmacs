;; -*- no-byte-compile: t; -*-

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                       :ref nil :depth 1
                       :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                       :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; Install use-package support
(elpaca elpaca-use-package
        ;; Enable use-package :ensure support for Elpaca.
        (elpaca-use-package-mode)
        ;; Assume :elpaca t unless otherwise specified.
        (setq elpaca-use-package-by-default t))

(use-package base16-theme
  :config
  (load-theme 'base16-oxocarbon-dark t))

(use-package gcmh
  :ensure (:wait t)
  :init
  (gcmh-mode 1))

;; mapc uses the function, in this case "load" on each instance of the second parameter
;; from documentation of "file-expand-wildcards": This returns a list of file names that match PATTERN.
(mapc 'load (file-expand-wildcards (expand-file-name "lisp/*/*.el" user-emacs-directory)))
(setq backup-directory-alist `(("." . ,"/tmp/gkmacs")))
;; (global-display-line-numbers-mode +1)
(setq display-line-numbers t)
(setq display-line-numbers-mode 'relative)
(menu-bar--display-line-numbers-mode-relative)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
