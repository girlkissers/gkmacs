
(use-package fish-mode)


(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(fish-mode . ("/usr/bin/fish-lsp"))))
