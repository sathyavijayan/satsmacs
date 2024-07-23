(setq satsmacs/default-font "VictorMono NFP")
(setq satsmacs/default-font-size 190)


(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;; disable visible bell
(setq visible-bell nil)


;; modeline indicators
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;;
;; better modeline
;;

;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1)
;;   :custom ((doom-modeline-height 10)))

;; (doom-modeline-def-modeline 'main
;;   '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
;;   '(vcs objed-state misc-info persp-name battery grip irc mu4e gnus github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process " "))

;;
;; Load default theme
;;
;; quick switch to dark mode
(defun switch-to-dark-mode ()
  (interactive)
  (message "And miles to go before YOU sleep ! (-_-)zzz")
  (setup-fonts)
  (load-theme 'flucui-dark))

(defun switch-to-light-mode ()
  (interactive)
  (message "Rise and shine ! (^_^)/")
  (setup-fonts)
  (load-theme 'flucui-light))

;; (use-package nord-theme)

(use-package stimmung-themes
  ;; :straight (stimmung-themes :host github :repo "motform/stimmung-themes") ; if you are a straight shooter
  :demand t
  :ensure t) ; or (stimmung-themes-load-dark)

(defun switch-to-experimental-theme ()
  (interactive)
  (message "What is life without experimentation !")
  (setup-exp-fonts)
  (stimmung-themes-load-light))

(defun setup-flucui-theme ()
  (load-theme 'flucui-light)
  (key-chord-define-global "EE" 'switch-to-experimental-theme)
  (key-chord-define-global "DD" 'switch-to-dark-mode)
  (key-chord-define-global "LL" 'switch-to-light-mode))

(use-package flucui-themes
  :init (setup-flucui-theme))

;; set preferred font
(defun setup-fonts ()
  (set-face-attribute 'default        nil :font satsmacs/default-font :height satsmacs/default-font-size  :weight 'medium)
  (set-face-attribute 'fixed-pitch    nil :font satsmacs/default-font  :height satsmacs/default-font-size  :weight 'medium)
  (set-face-attribute 'variable-pitch nil :font satsmacs/default-font  :height satsmacs/default-font-size  :weight 'normal))

;; set experimental font
(defun setup-exp-fonts ()
  (set-face-attribute 'default        nil :font  satsmacs/default-font  :weight 'medium)
  (set-face-attribute 'fixed-pitch    nil :font satsmacs/default-font   :weight 'medium)
  (set-face-attribute 'variable-pitch nil :font satsmacs/default-font   :weight 'normal))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (setup-fonts)))
  (setup-fonts))

;; ;; -- experimental --
(straight-use-package
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(require 'nano-base-colors)
(require 'nano-colors)
(require 'nano-faces)
;; (require 'nano-theme)
;; (require 'nano-theme-light)
;; (require 'nano-theme-dark)
(require 'nano-modeline)

;; (defun my-nano-faces ()
;;   (let ()
;;     "Derive face attributes for nano-faces using nano-theme values."
;;     (set-face-attribute 'nano-face-default nil
;;                         :foreground nano-color-foreground
;;                         :background nano-color-background
;;                         :family     nano-font-family-monospaced
;;                         :height       (* nano-font-size 10))
;;     (set-face-attribute 'nano-face-critical nil
;;                         :foreground nano-color-foreground
;;                         :background nano-color-critical)
;;     (set-face-attribute 'nano-face-popout nil
;;                         :foreground nano-color-popout)

;;     (set-face-attribute 'nano-face-variable-pitch nil
;;                         :foreground (face-foreground 'nano-face-default)
;;                         :background (face-background 'nano-face-default)
;;                         :family nano-font-family-proportional
;;                         :height (* nano-font-size 10))
;;     (if (display-graphic-p)
;;         (set-face-attribute 'nano-face-strong nil
;;                             :foreground (face-foreground 'nano-face-default)
;;                             :weight 'medium)
;;       (set-face-attribute 'nano-face-strong nil
;;                           :foreground (face-foreground 'nano-face-default)
;;                           :weight 'extra-bold))

;;     (set-face-attribute 'nano-face-salient nil
;;                         :foreground nano-color-salient
;;                         :weight 'medium) ;;--

;;     (set-face-attribute 'nano-face-faded nil
;;                         :foreground nano-color-faded
;;                         :slant 'italic
;;                         :weight 'medium) ;;-

;;     (set-face-attribute 'nano-face-subtle nil
;;                         :background nano-color-subtle)

;;     (set-face-attribute 'nano-face-header-default nil
;;                         :foreground nano-color-foreground
;;                         :background nano-color-subtle
;;                         :box `(:line-width 1
;;                                            :color ,nano-color-background
;;                                            :style nil))

;;     (set-face-attribute 'nano-face-tag-default nil
;;                         :foreground nano-color-foreground
;;                         :background nano-color-background
;;                         :weight 'semi-bold
;;                         :height (if (display-graphic-p)
;;                                     (round
;;                                      (* 0.85 (* 10 nano-font-size)))
;;                                   1)
;;                         :box `(:line-width 1
;;                                            :color ,nano-color-foreground
;;                                            :style nil))

;;     (set-face-attribute 'nano-face-header-strong nil
;;                         :foreground nano-color-strong
;;                         :background nano-color-subtle
;;                         :inherit 'nano-face-strong
;;                         :box `(:line-width 1
;;                                            :color ,nano-color-background
;;                                            :style nil))

;;     (set-face-attribute 'nano-face-tag-strong nil
;;                         :foreground nano-color-strong
;;                         :background nano-color-subtle
;;                         :weight 'semi-bold
;;                         :height (if (display-graphic-p)
;;                                     (round
;;                                      (* 0.85 (* 10 nano-font-size)))
;;                                   1)
;;                         :box `(:line-width 1
;;                                            :color ,nano-color-strong
;;                                            :style nil))

;;     (set-face-attribute 'nano-face-header-salient nil
;;                         :foreground nano-color-background
;;                         :background nano-color-salient
;;                         :box `(:line-width 1
;;                                            :color ,nano-color-background
;;                                            :style nil))

;;     (set-face-attribute 'nano-face-tag-salient nil
;;                         :foreground nano-color-background
;;                         :background nano-color-salient
;;                         :weight 'semi-bold
;;                         :height (if (display-graphic-p)
;;                                     (round
;;                                      (* 0.85 (* 10 nano-font-size)))
;;                                   1)
;;                         :box `(:line-width 1
;;                                            :color ,nano-color-salient
;;                                            :style nil))

;;     (set-face-attribute 'nano-face-header-popout nil
;;                         :foreground nano-color-background
;;                         :background nano-color-popout
;;                         :box `(:line-width 1
;;                                            :color ,nano-color-background
;;                                            :style nil))

;;     (set-face-attribute 'nano-face-tag-popout nil
;;                         :foreground nano-color-background
;;                         :background nano-color-popout
;;                         :weight 'semi-bold
;;                         :height (if (display-graphic-p)
;;                                     (round
;;                                      (* 0.85 (* 10 nano-font-size)))
;;                                   1)
;;                         :box `(:line-width 1
;;                                            :color ,nano-color-popout
;;                                            :style nil))

;;     (set-face-attribute 'nano-face-header-faded nil
;;                         :foreground nano-color-background
;;                         :background nano-color-faded
;;                         :box `(:line-width 1
;;                                            :color ,nano-color-background
;;                                            :style nil))

;;     (set-face-attribute 'nano-face-tag-faded nil
;;                         :foreground nano-color-background
;;                         :background nano-color-faded
;;                         :weight 'semi-bold
;;                         :height (if (display-graphic-p)
;;                                     (round
;;                                      (* 0.85 (* 10 nano-font-size)))
;;                                   1)
;;                         :box `(:line-width 1
;;                                            :color ,nano-color-faded
;;                                            :style nil))

;;     (set-face-attribute 'nano-face-header-subtle nil)

;;     (set-face-attribute 'nano-face-header-critical nil
;;                         :foreground nano-color-background
;;                         :background nano-color-critical
;;                         :box `(:line-width 1
;;                                            :color ,nano-color-background
;;                                            :style nil))
;;     (set-face-attribute 'nano-face-tag-critical nil
;;                         :foreground nano-color-background
;;                         :background nano-color-critical
;;                         :weight 'semi-bold
;;                         :height (if (display-graphic-p)
;;                                     (round
;;                                      (* 0.85 (* 10 nano-font-size)))
;;                                   1)
;;                         :box `(:line-width 1
;;                                            :color ,nano-color-critical
;;                                            :style nil))

;;     (set-face-attribute 'nano-face-header-separator nil
;;                         :inherit 'nano-face-default
;;                         :height 0.1)
;;     (set-face-attribute 'nano-face-header-filler nil
;;                         :inherit 'nano-face-header-default
;;                         :height 0.1)
;;     (set-face-attribute 'nano-face-header-highlight nil
;;                         :inherit 'nano-face-header-faded
;;                         :box nil)))



;; (setq nano-font-family-monospaced satsmacs/default-font)
;; (setq nano-font-family-proportional satsmacs/default-font)
;; ;;(setq nano-font-family-proportional (face-attribute satsmacs/default-font :family))
;; (setq nano-font-size 16)


;; (my-nano-faces)
(setq mode-line-format nil)
(setq-default mode-line-format nil)
(nano-modeline)
;; (nano-theme-set-light)
;; (nano-theme)
