(use-package web-mode
  :ensure flycheck
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.jsx?$" . web-mode)
         ("\\.ts?$" . web-mode)
         ("\\.tsx?$" . web-mode))
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-style-padding 1
        web-mode-script-padding 1
        web-mode-block-padding 0
        web-mode-comment-style 2
        web-mode-indent-style 2
        web-mode-attr-indent-offset 2
        web-mode-attr-value-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-indentless-elements 2
        web-mode-indentation-params '(("lineup-calls" . nil)
                                      ("lineup-args" . nil)))

  :init
  (progn
    (flycheck-add-mode 'javascript-eslint 'web-mode)))

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'web-mode-hook
            #'(lambda ()
                (enable-minor-mode
                 '("\\.jsx?\\'" . prettier-js-mode)
                 )))
  (add-hook 'rjsx-mode-hook 'prettier-js-mode))
