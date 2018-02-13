;;; package --- languages.el
;;; Commentary:
;;; Code:
(require 'use-package)

;;; javascript/TypeScript
(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 4))))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook 'rainbow-mode))

(use-package tide
  :ensure t
  :config)

(use-package mocha
  :ensure t)

;;; angular2
(use-package typescript-mode
  :ensure t)
(use-package ng2-mode
  :ensure t)

;;; common lisp
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;;; php
(use-package php-mode
  :ensure t)

;;; TIP
(use-package tip-mode
  :ensure f
  :config
  (add-to-list 'auto-mode-alist '("\\.tip\\'" . tip-mode)))

;;; haskell
(use-package haskell-mode
  :ensure t)

;;; scala
(use-package ensime
  :ensure t
  :config
  (setq ensime-startup-notification nil)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

(use-package dumb-jump
  :ensure t
  :diminish dumb-jump-mode
  :bind (("C-M-g" . dumb-jump-go)
         ("C-M-p" . dumb-jump-back)
         ("C-M-q" . dumb-jump-quick-look)))

;;; idris
(use-package idris-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.idr\\'" . idris-mode))
  (defun my-idris-mode-hook ()
  (add-to-list 'display-buffer-alist
               '(".*". (display-buffer-reuse-window . ((reusable-frames . t)))))
  (setq idris-stay-in-current-window-on-compiler-error t)
  (setq idris-prover-restore-window-configuration t)

  (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list "*idris-repl*")
  (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list "*idris-notes*")
  (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list "*idris-info*")
  (add-to-list 'frames-only-mode-kill-frame-when-buffer-killed-buffer-list "*idris-holes*"))

(add-hook 'idris-mode-hook #'my-idris-mode-hook))

;;; languages.el ends here
