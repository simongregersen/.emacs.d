;; initialization
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))
(package-initialize)

;; bootstrap and load use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; appearance
(use-package atom-dark-theme
  :ensure t
  :config
  (load-theme 'atom-dark 'NO-CONFIRM))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))

(use-package smartparens
  :ensure t
  :config
  (show-paren-mode 1))

(set-face-attribute 'default nil :height 90)     ; font size
(setq frame-title-format '("" "%b @ %f"))        ; window title
(setq inhibit-startup-message t)     ; dont show the GNU splash screen
(transient-mark-mode t)              ; show selection from mark
(tool-bar-mode 0)                    ; disable toolbar
(menu-bar-mode 0)                    ; disable menu bar
(scroll-bar-mode 0)                  ; disable scroll bar
(blink-cursor-mode 0)                ; disable blinking cursor
(mouse-avoidance-mode 'jump)         ; jump mouse away when typing
(setq visible-bell 1)                ; turn off bip warnings
(auto-compression-mode 1)            ; browse tar archives
(put 'upcase-region 'disabled nil)   ; enable ``upcase-region''
(put 'set-goal-column 'disabled nil) ; enable column positioning
(setq case-fold-search t)            ; make search ignore case
(global-linum-mode t)                ; global line numbers
(fset 'yes-or-no-p 'y-or-n-p)        ; short-hand yes/no selection
(ido-mode 1)                         ; interactive DO mode (better file opening and buffer switching)
(setq-default indent-tabs-mode nil)  ; tabs over spaces
(setq initial-buffer-choice t)       ; initial buffer *scratch*

;; misc. hooks
(add-hook 'before-save-hook 'whitespace-cleanup) ; whitespace-cleanup on save

;; kill terminated shell buffer
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))
(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

;; autosave location (in $TMPDIR/emacs$UID/)
(defconst emacs-tmp-dir (format "%s/%s/" temporary-file-directory "backup"))

(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

;; auto completion
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
        company-echo-delay 0
        company-dabbrev-downcase nil
        company-minimum-prefix-length 3
        ompany-tooltip-limit 20
        company-selection-wrap-around t
        company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance))
  (define-key company-mode-map (kbd "C-M-i") 'company-indent-or-complete-common)
  (global-company-mode))
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; project explorer
(use-package neotree
  :ensure t)

;; printing
(use-package ps-print
  :defer t
  :config
  (setq ps-print-header nil)
  (setq ps-paper-type 'a4)
  (setq ps-print-color-p nil))

;; LaTeX
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (setq TeX-PDF-mode t)
  (setq TeX-clean-confirm nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (add-hook 'kill-buffer-hook 'TeX-clean nil 'make-it-local))))

;; latex auto completion
(use-package company-auctex
  :ensure t
  :config
  (company-auctex-init))

;; latex preview
(use-package latex-preview-pane
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)
  (latex-preview-pane-enable))

;; git
(use-package magit
  :ensure t
  :pin melpa-stable
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  (setq magit-refresh-status-buffer nil)
  (setq vc-handled-backends nil)
  :bind (("C-x g" . magit-status)
         ("C-c g b" . magit-branch-and-checkout)
         ("C-c g c" . magit-checkout)
         ("C-c g l" . magit-log-all)))

;; javascript/TypeScript
(use-package js2-mode
  :ensure t)

;; source code navigation
(use-package tern
  :ensure t)

(use-package company-tern
  :ensure t
  :config
  (add-to-list 'company-backends '(company-tern)))

(use-package tide
  :ensure t
  :config)

;; angular2
(use-package typescript-mode
  :ensure t)
(use-package ng2-mode
  :ensure t)

;; multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("M-<return>" . mc/mark-all-like-this)))

;; snippets
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

;; move selections or a line  up and down
(use-package drag-stuff
  :ensure t
  :config
  (global-set-key (kbd "<C-S-down>") 'drag-stuff-down)
  (global-set-key (kbd "<C-S-up>") 'drag-stuff-up))

;; save point position between session
(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

;; auto-indent
;; use: (add-hook '<some-mode>-hook #'aggressive-indent-mode)
(use-package aggressive-indent
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'html-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode))

;; common functions
(defun load-init ()
  "Runs load-file on ~/.emacs.d/init.el"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer) (buffer-list))))

(defun print ()
  "Prints buffer"
  (interactive)
  (lpr-buffer))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; custom key bindings
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)
