;;; package --- my init.el
;;; Commentary:
;;; Code:

;; initialization
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize))

(setq package-enable-at-startup nil)
(setq package-archives
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 15)))
(package-initialize)

;; bootstrap and load use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; MAC fix
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; local library
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(let ((default-directory (concat user-emacs-directory "lib")))
  (normal-top-level-add-subdirs-to-load-path))

;; appearance
(use-package powerline
  :ensure t
  :config (powerline-default-theme))

(use-package all-the-icons) ; 'M-x all-the-icons-install-fonts' to install resource fonts
(use-package doom-themes
  :init
  (load-theme 'doom-vibrant t)
  (doom-themes-neotree-config))

;; highlight changes
(use-package git-gutter
  :diminish git-gutter-mode
  :config (global-git-gutter-mode))

;; setup path
(add-to-list 'exec-path "/home/gregersen/.cabal/bin")

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; set a default font
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))

;; specify font for all unicode characters
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(set-face-attribute 'default nil :height 100)    ; font size
(setq frame-title-format '("" "%b @ %f"))        ; window title
(setq inhibit-startup-message t)     ; dont show the GNU splash screen
(transient-mark-mode t)              ; show selection from mark
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)      ; disable toolbar
      (menu-bar-mode -1)      ; disable menu bar
      (scroll-bar-mode -1)))  ; disable scroll bar
(blink-cursor-mode 0)                ; disable blinking cursor
(mouse-avoidance-mode 'jump)         ; jump mouse away when typing
(setq visible-bell 1)                ; turn off bip warnings
(auto-compression-mode 1)            ; browse tar archives
(put 'upcase-region 'disabled nil)   ; enable ``upcase-region''
(put 'set-goal-column 'disabled nil) ; enable column positioning
(setq column-number-mode t)          ; show column number
(setq case-fold-search t)            ; make search ignore case
(global-linum-mode 0)                ; global line numbers
(fset 'yes-or-no-p 'y-or-n-p)        ; short-hand yes/no selection
(ido-mode 1)                         ; interactive DO mode (better file opening and buffer switching)
(setq-default indent-tabs-mode nil)  ; tabs over spaces
(delete-selection-mode +1)           ; type over a selected region, instead of deleting before typing.

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; start full screen

;; file extensions to ignore
(push ".ibc" completion-ignored-extensions)

(use-package pretty-mode
  :ensure t
  :config
  (add-hook 'coq-mode 'turn-on-pretty-mode)
  (pretty-activate-groups
   '(:sub-and-superscripts :greek :arithmetic-nary)))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))

(use-package flycheck
  :ensure t
  :config
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  )

;; misc. hooks
(add-hook 'before-save-hook 'whitespace-cleanup) ; whitespace-cleanup on save

;; autosave location (in $TMPDIR/emacs$UID/)
(defconst emacs-tmp-dir (format "%s/%s/" user-emacs-directory "backup"))

(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

(use-package smartparens
  :ensure t
  :config
  (show-paren-mode 1))

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
;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; project explorer
(use-package neotree
  :ensure t
  :config
  (setq neo-smart-open t)
  (global-set-key [f8] 'neotree-toggle))

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

;; save point position between session
(use-package saveplace
  :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

;; auto-indent
;; (use-package aggressive-indent
;;   :ensure t
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;;   (add-hook 'html-mode-hook #'aggressive-indent-mode)
;;   (add-hook 'css-mode-hook #'aggressive-indent-mode)
;;   (add-hook 'LaTeX-mode-hook #'aggressive-indent-mode))

;; custom key bindings
(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)

(global-set-key (kbd "S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-<down>") 'shrink-window)
(global-set-key (kbd "S-<up>") 'enlarge-window)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; load remaining lisp
(load "languages")
(load "functions")
(load "coq")
(setq custom-file (concat user-emacs-directory (convert-standard-filename "lisp/custom.el")))
(load custom-file)

(defun TeX-brace-count-line ()
  "Count number of open/closed braces."
  (save-excursion
    (let ((count 0) (limit (line-end-position)) char)
      (while (progn
               (skip-chars-forward "^{}[]\\\\" limit)
               (when (and (< (point) limit) (not (TeX-in-comment)))
                 (setq char (char-after))
                 (forward-char)
                 (cond ((eq char ?\{)
                        (setq count (+ count TeX-brace-indent-level)))
                       ((eq char ?\})
                        (setq count (- count TeX-brace-indent-level)))
                       ((eq char ?\[)
                        (setq count (+ count TeX-brace-indent-level)))
                       ((eq char ?\])
                        (setq count (- count TeX-brace-indent-level)))
                       ((eq char ?\\)
                        (when (< (point) limit)
                          (forward-char)
                          t))))))
      count)))

;;; init.el ends here
