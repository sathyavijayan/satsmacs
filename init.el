;; Sats's emacs config !
;; learning and inspiration from https://github.com/BrunoBonacci/lambdamacs

;;
;; Initialize package sources
(require 'package)
(setq package-user-dir (expand-file-name "packages" user-emacs-directory))
(unless (file-exists-p package-user-dir)
    (make-directory package-user-dir))

;; Always load newest byte code
(setq load-prefer-newer t)

(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")
        ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))


;;
;;  Initialize use-package on non-Linux platforms
;;
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; add use pacakge and set gloabl `:ensure t`
(require 'use-package)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                   ----==| S T R A I G H T . E L |==----                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Sats: initial folder structure is the same as lambdamacs, as it
;; seems to make sense. Will change as I improve this setup.
;; src/
;; modules/
;; config/

(setq satsmacs-src-dir         (expand-file-name "src" user-emacs-directory))
(setq satsmacs-config-dir      (expand-file-name "config" user-emacs-directory))
(setq satsmacs-modules-dir     (expand-file-name "modules" user-emacs-directory))

(add-to-list 'load-path satsmacs-src-dir)
(add-to-list 'load-path satsmacs-config-dir)
(add-to-list 'load-path satsmacs-modules-dir)

(setq custom-file (expand-file-name "emacs-custom.el" satsmacs-config-dir))

(defun load-if (file)
  "load a configuration file if it exists within the conf dir"
  (if (file-exists-p (expand-file-name file satsmacs-config-dir))
      (load-file (expand-file-name file satsmacs-config-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;           ----==| L O A D   C O N F I G U R A T I O N S |==----            ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load    "default-config.el")    ;; Load user preferences
(load-if "custom-config.el")     ;; load custom configuration if present
(load    "utils.el")             ;; utils
(load    "general.el"  )         ;; general settings
(load    "theme.el")             ;; visual aspects
(load    "editor.el")            ;; editor settings
(load    "dev-tools.el")         ;; generic dev tools that apply to
                                 ;; all languages/platforms.
(load    "clojure.el")           ;; Clojure ! Yeah !
(load    "web.el")        ;; web settings
;; (load  "dev.el")            ;; common dev tools
;; (load  "dev-clojure.el")    ;; clojure settings
;; (load  "dev-java.el")       ;; settings for java development
;; (load  "productivity.el")   ;; org-mode and productivity tools
;; (loadx "emacs-custom.el")   ;; load emacs customizations
;; (loadx "post-init.el")      ;; load custom post configuration if present
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
