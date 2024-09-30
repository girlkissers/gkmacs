(use-package v-mode)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(v-mode . ("/usr/bin/v-analyzer"))))

