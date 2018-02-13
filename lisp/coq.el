;;; package --- coq.el
;;; Commentary:
;;; Code:

(use-package proof-site
  :ensure f
  :defer t
  :mode ("\\.v\\'" . coq-mode)
  :load-path "~/.emacs.d/lib/PG/generic/proof-site"
  :config

  (setq proof-splash-seen t)
  (setq proof-three-window-mode-policy 'hybrid)
  (setq proof-script-fly-past-comments t)
  (setq coq-compile-before-require t)

  (use-package company-coq
    :ensure t
    :config
    (add-hook 'coq-mode-hook #'company-coq-mode)
    (add-hook 'coq-mode-hook
              (lambda ()
                (setq-local prettify-symbols-alist
                            '(("Proof." . ?∵) ("Qed." . ?■))))))

  (defconst company-coq-tg--preprocessor-substitutions
    '(("\n"  . " ") ("[ "  . "( OR-GROUP ") (" ]"  . " )")
      (" | " . " OR ") ("; "  . " AND ") ("'" . "’")))
  )


;;; coq.el ends here
