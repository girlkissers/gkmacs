;; (use-package rust-mode
;;   :init
;;   (setq rust-mode-treesitter-derive t)
;;   (add-hook 'rust-mode-hook 'eglot-ensure)
;;   (add-hook 'eglot-managed-mode-hook
;;             ;; Show flymake diagnostics first.
;;             ;; Show all eldoc feedback.
;;             (setq eldoc-documentation-strategy #'eldoc-documentation-compose)))
;;
(use-package rustic
  :init
  (setq rustic-lsp-client 'eglot)
  :config
  (cl-defgeneric lsp-clients-extract-signature-on-hover (contents _server-id)
    "Extract a representative line from CONTENTS, to show in the echo area."
    (car (s-lines (s-trim (lsp--render-element contents))))))

;; (setq sideline-backends-right '(sideline-eglot)))
(use-package eldoc-box)
(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'line) ; 'point to show errors only on point
                                        ; 'line to show errors on the current line
  (setq sideline-backends-right '(sideline-flymake)))

