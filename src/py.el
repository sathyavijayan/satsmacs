;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;              ----==| P Y T H O N   ( B E C A U S E ) |==----               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Config adapted from :
;; https://github.com/daviwil/emacs-from-scratch/wiki/LSP-Python-(pyright)-config-in-emacs-from-scratch#wiki-pages-box

;; To activate venv for the project automatically create .dir-locals.el with the following
;; .dir-locals.el
;; (python-mode . ((pyvenv-workon . "venv")))
;; -- venv in the above sexp is the name of the virtualenv folder.

;; pyright config reference:
;; https://emacs-lsp.github.io/lsp-pyright/#configuration
(use-package lsp-pyright
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))


;; You can use M-x pyvenv-activate to activate specific venv
;; TODO: can this be automated when switching to project?
(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.venvs/")
  :config
  ;; (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

;; reformat python buffers
(use-package blacken
  :init
  (setq-default blacken-fast-unsafe t)
  (setq-default blacken-line-length 80)
  )

(use-package python-mode
  :hook
  (python-mode . pyvenv-mode)
  (python-mode . flycheck-mode)
  (python-mode . company-mode)
  (python-mode . blacken-mode)
  (python-mode . yas-minor-mode)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  :config
  )

;; References:
;; https://martibosch.github.io/jupyter-emacs-universe/
;; https://sqrtminusone.xyz/posts/2021-05-01-org-python/
;; https://github.com/emacs-jupyter/jupyter

;; Note: This package needs the following packages installed using
;; brew automake, libtool
(use-package jupyter)
