;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;     ----==| G E N E R A L   D E V E L O P M E N T   T O O L S |==----      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Git and Github packages
;;
(use-package transient)

(use-package magit
  :after transient
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)))
;; manage PRs
;; (use-package forge
;;   :after (magit))

(use-package git-timemachine)

;;(use-package github-review)


(defun magit-open-repo ()
  "open remote repo URL"
  (interactive)
  (let ((url (magit-get "remote" "origin" "url")))
    (progn
      (browse-url (parse-url url))
      (message "opening repo %s" url))))


(add-hook 'magit-mode-hook
          (lambda ()
            (local-set-key (kbd "C-o") 'magit-open-repo)))

;;
;; Browse selected file or lines on github.
;;
(use-package browse-at-remote
  :init
  (global-set-key (kbd "C-c C-w") 'browse-at-remote)
  (global-set-key (kbd "C-c C-S-w")   'browse-at-remote-kill))


;;
;; config projectile
;;
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  ;; -L allows to follow symlinks
  (setq projectile-generic-command "fd . -L -0 --type f --color=never")
  ;; seems the only one working with symlinks
  ;; (setq projectile-indexing-method 'native)
  )


;;
;; Faster searches with the_silver_searcher
;;
(use-package ag)


(use-package helm-ag
  :custom
  (helm-ag-base-command "ag --nocolor --nogroup --ignore-case --ignore-dir target")
  (helm-ag-command-option "") ;;--all-text not working in conjuction with --ignore-dir
  (helm-ag-insert-at-point 'symbol))

;;
;; The following snippet tells Projectile to use Helm-ag for project searches
;; (C-c p s s)
;; The `helm-projectile' version is better as it is fully interactive.
;;

;; taken from: https://github.com/bbatsov/helm-projectile/blob/master/helm-projectile.el
;; Thanks @bbastov
(defun helm-projectile-ag (&optional options)
  "Helm version of `projectile-ag'."
  (interactive (if current-prefix-arg (list (helm-read-string "option: " "" 'helm-ag--extra-options-history))))
  (if (require 'helm-ag nil t)
      (if (projectile-project-p)
          (let* ((grep-find-ignored-files (cl-union (projectile-ignored-files-rel) grep-find-ignored-files))
                 (grep-find-ignored-directories (cl-union (projectile-ignored-directories-rel) grep-find-ignored-directories))
                 (ignored (mapconcat (lambda (i)
                                       (concat "--ignore " i))
                                     (append grep-find-ignored-files grep-find-ignored-directories (cadr (projectile-parse-dirconfig-file)))
                                     " "))
                 (helm-ag-base-command (concat helm-ag-base-command " " ignored " " options))
                 (current-prefix-arg nil))
            (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file))))
        (error "You're not in a project"))
    (when (yes-or-no-p "`helm-ag' is not installed. Install? ")
      (condition-case nil
          (progn
            (package-install 'helm-ag)
            (helm-projectile-ag options))
        (error (error "`helm-ag' is not available.  Is MELPA in your `package-archives'?"))))))


(define-key projectile-mode-map [remap projectile-ag] #'helm-projectile-ag)
(def-projectile-commander-method ?A
                                 "Find ag on project."
                                 (call-interactively 'helm-projectile-ag))

;;
;; Better bullets
;;
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


(setq org-todo-keywords
    '((sequence "TODO(t)" "DOING(d)" "|" "DONE(f)")))

;;
;; Autogenerate html on-save for org-mode
;;
(defun org-autogenerate-html-on-save ()
  (interactive)
  (if (memq 'org-html-export-to-html after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-html-export-to-html t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-html-export-to-html nil t)
    (message "Enabled org html export on save for current buffer...")))

(if (file-exists-p "~/org")
    (setq org-agenda-files (directory-files "~/org" t ".*\.org$")))

(setq org-todo-keywords
    '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")))


(advice-add 'org-deadline       :after (lambda (&rest _) (org-save-all-org-buffers)))
(advice-add 'org-schedule       :after (lambda (&rest _) (org-save-all-org-buffers)))
(advice-add 'org-store-log-note :after (lambda (&rest _) (org-save-all-org-buffers)))
(advice-add 'org-todo           :after (lambda (&rest _) (org-save-all-org-buffers)))

(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)

(setq org-export-coding-system 'utf-8)
(setq org-default-notes-file "~/org/personal.org")
(setq org-directory "~/org")

(setq org-capture-templates
      '(("t" "Generic TODO item (scheduled)"
         entry
         (file org-default-notes-file)
         "* TODO %? \n  SCHEDULED: %^t\n")

        ("T" "Generic TODO item (scheduled)"
         entry
         (file org-default-notes-file)
         "* TODO (%^{SIZE[0-9]|0}) %? %^g\n  SCHEDULED: %^t\n  %i")

        ("C" "Canoo TODO item (scheduled)"
         entry
         (file "~/org/canoo.org")
         "* TODO %? \n  SCHEDULED: %^t\n")

        ("d" "Deadline)"
         entry
         (file org-default-notes-file)
         "* TODO (%^{SIZE[0-9]|0}) %? %^g\n  DEADLINE: %^t\n  %i")

        ("l" "Linked TODO item"
         entry
         (file org-default-notes-file)
         "* TODO (%^{SIZE[0-9]|0}) %? %^g\n  SCHEDULED: %^t\n  %i\n  %a")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                      ----==| M A R K D O W N |==----                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;
;; ability to generate Table of Contents
;;
(use-package markdown-toc)


;;
;; Live preview while editing
;;
(use-package flymd
  :config
  (defun my-flymd-browser-function (url)
    (let ((process-environment (browse-url-process-environment)))
      (apply 'start-process
             (concat "firefox " url)
             nil
             "/usr/bin/open"
             (list "-a" "firefox" url))))
  (setq flymd-browser-open-function 'my-flymd-browser-function))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                     ----==| Y A S N I P P E T |==----                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet                  ; Snippets
  :config
  (setq
   yas-verbosity 1                      ; No need to be so verbose
   yas-wrap-around-region t)

  (add-to-list 'yas-snippet-dirs 'yasnippet-snippets-dir t)
  (add-to-list 'yas-snippet-dirs (expand-file-name "yas" user-emacs-directory) t)

  (yas-reload-all)
  (yas-global-mode))

;;
;; Collection of snippets
;;
(use-package yasnippet-snippets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                    ----==| R E S T C L I E N T |==----                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package restclient)

;;
;; restclient company--auto-completion
;;
(use-package company-restclient
  :config
  (add-to-list 'company-backends 'company-restclient)
  (setq auto-mode-alist
        (append '(("\\.rest\\'" . restclient-mode)) auto-mode-alist)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                       ----==| G R A P H Q L |==----                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package request)
(use-package graphql-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                     ----==| T E R R A F O R M |==----                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package terraform-mode
  :config
  (setq terraform-indent-level 2)
  :bind (:map terraform-mode-map
              ("C-c j" . helm-imenu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                      ----==| G R A P H V I Z |==----                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 2)
  (setq graphviz-dot-view-edit-command nil)
  (setq graphviz-dot-view-command "dot -Tpng %s")
  (setq graphviz-dot-save-before-view t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;          ----==| W E B - S E Q U E N C E - D I A G R A M |==----           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Installing WSD-mode
;;
(use-package wsd-mode
  :init
  (setq wsd-style "roundgreen")
  (setq wsd-style-altern "napkin")
  :bind (:map wsd-mode-map
              ("C-c C-a" . #'wsd-show-diagram-inline-alternative)))

(defun wsd-show-diagram-inline-alternative ()
  (interactive)
  (let*
      ((wsd-style-temp wsd-style))
    (setq wsd-style wsd-style-altern)
    (wsd-show-diagram-inline)
    (setq wsd-style wsd-style-temp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                       ----==| M E R M A I D |==----                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ob-mermaid
  :init
  (setq ob-mermaid-cli-path "/Users/sats/.nvm/versions/node/v17.6.0/bin/mmdc"))

(use-package ob-ipython)

;; FIXME: this is causing issues with emacs 31
;; emacs 31 Shortdoc f function 'f-older-p': bad keyword ':noeval'
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((mermaid . t)
;;    (clojure . t)
;;    (shell .t)
;;    (scheme . t)
;;    ;; Python & Jupyter
;;    (python . t)
;;    (ipython . t)
;;    (jupyter . t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                  ----==| O R G   S E T T I N G S |==----                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-src-fontify-natively t)

;; Hack to support syntax highlighing
;;
;; taken from: https://emacs.stackexchange.com/a/9838
(defun rasmus/org-html-wrap-blocks-in-code (src backend info)
  "Wrap a source block in <pre><code class=\"lang\">.</code></pre>"
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string
     "\\(</pre>\\)" "</code>\n\\1"
     (replace-regexp-in-string "<pre class=\"src src-\\([^\"]*?\\)\">"
                               "<pre class=\"src src-\\1\">\n<code class=\"\\1\">\n" src))))

;; (add-to-list 'org-export-filter-src-block-functions
;;              'rasmus/org-html-wrap-blocks-in-code)
(setq org-export-filter-src-block-functions '(rasmus/org-html-wrap-blocks-in-code))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;              ----==| O R G   H T M L   E X P O R T S |==----               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)
(setq org-export-with-section-numbers nil)
(setq org-html-head "
<link rel=\"stylesheet\" type=\"text/css\" href=\"https://rawcdn.githack.com/BrunoBonacci/org-doc/master/assets/GTD.css\" />
<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.7.2/styles/github.min.css\">

<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.7.2/highlight.min.js\"></script>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.7.2/languages/clojure.min.js\"></script>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.7.2/languages/clojure-repl.min.js\"></script>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.7.2/languages/java.min.js\"></script>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.7.2/languages/bash.min.js\"></script>
<script>hljs.highlightAll();</script>")
(setq org-link-file-path-type "relative")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                    ----==| C A R B O N - N O W |==----                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package carbon-now-sh
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                   ----==| G O O G L E - T H I S |==----                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package google-this
  :init
  (setq google-this-keybind (kbd "s-g"))
  (google-this-mode 1))

;; ------------------------------------------------------------
;; Code boxes
;; ------------------------------------------------------------
(defun -pad-center (str len char)
  (store-substring (make-string len char) (/ (- len (length str)) 2) str))

(defun -trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun comment-box (title)
  (let* ((size 80)
         (norm-title (upcase
                      (-trim-string
                       (replace-regexp-in-string "\\(.\\)" "\\1 " title))))
         (decor-title (concat "----==| " norm-title " |==----")))
    (cl-flet ((str-repeat (size char) (make-string size (string-to-char char))))
      (concat "\n"
              (str-repeat 80 ";") "\n"
              ";;" (str-repeat (- size 4) " ") ";;\n"
              ";;" (-pad-center decor-title (- size 4) ? ) ";;\n"
              ";;" (str-repeat (- size 4) " ") ";;\n"
              (str-repeat 80 ";") "\n"))))


(defun bb-comment-box ()
  "Convert word at point (or selected region) to code box"
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'line)))
         (text   (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (comment-box text)))))

(global-set-key (kbd "s-b") 'bb-comment-box)

;;
;; delete space but one like emacs-live
;;
(defun live-delete-whitespace-except-one ()
  (interactive)
  (just-one-space -1))
(global-set-key (kbd "M-SPC ") 'live-delete-whitespace-except-one)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                   ----==| I N D E N T   B A R S |==----                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq indent-bars-prefer-character t)

(use-package indent-bars
  :hook ((python-mode yaml-mode web-mode) . indent-bars-mode)
  :config
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.3)
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 0.4)
   indent-bars-unspecified-fg-color "white"
   indent-bars-unspecified-bg-color "black")) ; or whichever modes you prefer
