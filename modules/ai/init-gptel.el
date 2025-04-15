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

  (gptel-make-openai "huoshan" ;Any name you want
    :host "ark.cn-beijing.volces.com"
    :endpoint "/api/v3/chat/completions"
    :stream t
    :key (env/get-huoshan-key)
    :models '(deepseek-v3-241226 deepseek-r1-250120))

  :custom
  (gptel-directives
   '(
     (default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
     (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
     (writing . "You are a large language model and a writing assistant. Respond concisely.")
     (english-teacher . "你是一个英语老师，想尽一切办法让我记住这些单词，即使是稍微无厘头的办法，使用中文回答，可以用英文造句，如果有一词多义尽量把他的多个意思用一个例句串联起来")
     (learning-assistant . "你是一个专业的学习助手，我是你的学生，你需要帮我解答我在学习中的困惑，尽可能详尽，也可以你认为对我有帮助的扩展信息")
     (code-helper . "你是一个专业的程序设计教授，我是你的学生，你需要帮我读懂代码，请一步步来让我能够跟上，也可以给我介绍代码中的优点和编程范式")
     ))
  )


(provide 'init-gptel)
