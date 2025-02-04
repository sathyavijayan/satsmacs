;; General packages
(use-package key-chord
  :init (key-chord-mode 1))

(setq custom-safe-themes t)

(setq satsmacs-save-dir
      (expand-file-name satsmacs/save-place
                        user-emacs-directory))

;; reduce the frequency of garbage collection by making it happen on
;; each 20MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 20000000)

;; warn when opening files bigger than 50MB
(setq large-file-warning-threshold 50000000)

;; expand to full screeen
;; (when (member initial-window-system '(x w32 ns))
;;   (toggle-frame-maximized)
;;   ;;(toggle-frame-fullscreen)
;;   )

;;
;; start daemon server
;;
;; Start server and set directory
(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(server-start)


;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;;
;; Automatically save buffer when losing focus
;;
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;
;; Fixing PATH and env var issues
;;
(use-package exec-path-from-shell
  :init
  (when (memq system-type '(darwin))
    (exec-path-from-shell-initialize)))
;;
;; Set the executable permissions on scripts
;;
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;
;; minibuffer long
;;
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key "\C-co" 'switch-to-minibuffer) ;; Bind to `C-c o'

;;
;; helper
;;
(use-package ivy
  :diminish
  :bind (("s-." . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "<f6>") 'ivy-resume))

;;
;; Commands descriptions for M-x commands
;;
(use-package counsel
  :bind (("M-x"     . counsel-M-x)
         ("C-x b"   . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-x j"   . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus))))


(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;;
;; Dired config
;;
;; Enable Dired to copy between buffers in a split-screen
(setq dired-dwim-target t)
;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies 'always) ; “always” means no asking
(setq dired-recursive-deletes 'top) ; “top” means ask once
(require 'dired-x)
(global-set-key (kbd "C-x C-/")  'dired-jump) ;; jump to dired currentfile



;; use Shift+arrow_keys to move cursor around split panes
;; same but with [Cmd]+[alt]+[->]
(global-set-key [M-s-left]  'windmove-left)
(global-set-key [M-s-right] 'windmove-right)
(global-set-key [M-s-up]    'windmove-up)
(global-set-key [M-s-down]  'windmove-down)
(setq windmove-wrap-around t)


(use-package prodigy
  :init
  (progn
    ;;
    ;; if a .prodigy exists load it
    ;;
    (if (file-exists-p "~/.prodigy.el")
        (load "~/.prodigy.el"))
    ;; set key binding
    (global-set-key (kbd "C-x p") 'prodigy)))

;;
;; Allows to fix a window in place so that it is not closed/removed
;; until you explicitly ask for it.
;;
;; Bindings
;; C-x 9 - Mark as sticky
;; C-u C-x 0 - close sticky window
;;
(load "sticky-windows.el")

;; auto completion
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package bm
  :init
  (setq
   bm-restore-repository-on-load t
   bm-repository-file (expand-file-name "bm-bookmarks" satsmacs-save-dir)
   bm-buffer-persistence t
   bm-restore-repository-on-load t
   bm-cycle-all-buffers t
   bm-in-lifo-order t
   bm-persistent-face 'bm-face)

  :bind
  (("s-1" . 'bm-toggle)
   ("s-2" . 'bm-previous)
   ("s-3" . 'bm-next)
   ("s-5" . 'bm-bookmark-regexp)
   ("s-0" . 'bm-remove-all-current-buffer)
   ("s-)" . 'bm-remove-all-all-buffers))
  :config
  (add-hook 'after-save-hook   #'(lambda nil
                                   (bm-buffer-save-all)
                                   (bm-repository-save)))
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore))

;;
;; ediff - don't start another frame
;;
(require 'ediff)
(setq
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally)


;;
;; ztree install (directory diff)
;;
(use-package ztree)

;;
;; Apparently this is part of Emacs default so no need to install it.
;;
(setq ispell-program-name "aspell"      ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(flyspell-mode +1)

(use-package flycheck
  :diminish flycheck-mode
  :init
  (setq flycheck-temp-prefix (expand-file-name "flycheck" temporary-file-directory))
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-display-errors-function nil
        flycheck-erlang-include-path '("../include")
        flycheck-erlang-library-path '()
        flycheck-check-syntax-automatically '(save)))


;; FIXME - Helm dev has stalled - Haven't made a decision on whether I
;; should move away from it, yet. Im not ready to give up some of the
;; features yet. I will try Ivy, Counsel, Swiper for other features
;; for a bit.
;;
;; https://news.ycombinator.com/item?id=24449883
;; https://github.com/emacs-helm/helm/issues/2386
;;
(use-package helm
  :init (helm-mode 1)
  :bind (("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list)))

(use-package helm-descbinds
  :init (helm-descbinds-mode 1))

(use-package which-key
  :init (which-key-mode))

(key-chord-define-global "FF" 'projectile-find-file)
(key-chord-define-global "PP" 'projectile-switch-project)


;; Ivy posframe to position minibuffer in a better place.
(use-package ivy-posframe
  :init
  (ivy-posframe-mode 1)
  :config
  (setq ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (completion-at-point . ivy-posframe-display-at-point)
          (helm-show-kill-ring . ivy-posframe-display-at-frame-center)
          (t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8))))
