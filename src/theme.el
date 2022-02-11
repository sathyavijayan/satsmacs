(setq satsmacs/default-font "Victor Mono")
(setq satsmacs/default-font-size 160)


(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;; disable visible bell
(setq visible-bell nil)

;; set preferref font
(set-face-attribute 'default nil
        :font   satsmacs/default-font
        :height satsmacs/default-font-size
        :weight 'bold)

;; modeline indicators
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;;
;; better modeline
;;

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

(doom-modeline-def-modeline 'main
  '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
  '(vcs objed-state misc-info persp-name battery grip irc mu4e gnus github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process " "))

;;
;; Load default theme
;;

;; quick switch to dark mode
(defun switch-to-dark-mode ()
  (interactive)
  (message "And miles to go before YOU sleep ! (-_-)zzz")
  (load-theme 'flucui-dark))

(defun switch-to-light-mode ()
  (interactive)
  (message "Rise and shine ! (^_^)/")
  (load-theme 'flucui-light))

(defun setup-flucui-theme ()
  (load-theme 'flucui-light)
  (key-chord-define-global "DD" 'switch-to-dark-mode)
  (key-chord-define-global "LL" 'switch-to-light-mode))

(use-package flucui-themes
  :init (setup-flucui-theme))
