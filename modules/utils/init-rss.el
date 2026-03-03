(use-package elfeed
  :straight t
  :config
  (setq elfeed-feeds
        '(
          ;; ============
          ;; arXiv 分类
          ;; ============
          ("https://arxiv.org/rss/cs.RO" robotics arxiv)
          ("https://arxiv.org/rss/cs.LG" ml arxiv)
          ("https://arxiv.org/rss/cs.CV" vision arxiv)

          ;; ============
          ;; arXiv 关键词搜索（重点）
          ;; ============
          ("https://arxiv.org/rss/search/?query=%28robot+learning+OR+manipulation+OR+visuomotor+OR+diffusion+policy+OR+world+model+OR+embodied%29&searchtype=all&abstracts=show&order=-announced_date_first&size=50"
           robot-learning query arxiv)

          ;; ============
          ;; Papers with Code
          ;; ============
          ("https://paperswithcode.com/rss/area/robotics" robotics pwc)
          ))
  (setq-default elfeed-search-filter "@1-week-ago +unread ")
  )

(provide 'init-rss)
