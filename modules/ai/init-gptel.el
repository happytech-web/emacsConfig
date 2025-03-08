(require '.env)

(use-package gptel
  :custom
  (gptel-track-media t)
  (gptel-default-mode 'org-mode)
  :config
  (setq gptel-model   'deepseek-chat
	gptel-backend
	(gptel-make-openai "DeepSeek"     ;Any name you want
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key (env/get-deepseek-key)             ;can be a function that returns the key
          :models '(deepseek-chat deepseek-coder deepseek-reasoner)))
  :custom
  (gptel-directives
   '(
     (default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
     (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
     (writing . "You are a large language model and a writing assistant. Respond concisely.")
     (english-teacher . "你是一个英语老师，想尽一切办法让我记住这些单词，即使是稍微无厘头的办法，使用中文回答，可以用英文造句，如果有一词多义尽量把他的多个意思用一个例句串联起来")
     ))
  )


(provide 'init-gptel)
