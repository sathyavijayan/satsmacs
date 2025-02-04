;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                         ----==| G P T E L |==----                          ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gptel
  :init
  (setq
   gptel-default-mode 'org-mode
   gptel-expert-commands 't
   gptel-model 'llama3.2:latest
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '(deepseek-r1:latest
                             llama3.2:latest
                             (llava:7b :description "Llava 1.6: Vision capable model"
                                       :capabilities (images)
                                       :mime-types ("image/jpeg" "image/png")))))

  :bind (("C-c RET" . gptel-send)
         ("C-c C-<return>" . gptel-send)))
