# Review Format

## 目标

`0-token filter` 只负责缩小候选集，不负责最终推荐。

最终的：

- 打分
- 分桶
- 是否值得 deep read

都必须由 agent 在阅读标题、摘要、作者、来源线索后填写。

## reviewed JSON 顶层结构

保持这个结构不变：

```json
{
  "date": "YYYY-MM-DD",
  "days": 3,
  "papers": [...]
}
```

允许出现额外顶层元数据，例如：

- `mode`
- `topic`
- `selector_profile`

但不要改变 `papers[].review` 的字段契约。

## 每篇论文必须填写的 review 字段

```json
{
  "title": "...",
  "url": "...",
  "summary": "...",
  "review": {
    "score": 0,
    "bucket": "urgent_follow",
    "reason": "为什么放进这个桶",
    "should_deep_read": true,
    "deep_read": {
      "一句话总结": "",
      "核心贡献": [],
      "要解决的问题": "",
      "现有方法局限": "",
      "本文动机": "",
      "整体框架": "",
      "核心模块": [],
      "关键公式": [],
      "实验结果": "",
      "批判性思考": {
        "优点": [],
        "局限性": [],
        "潜在改进": []
      },
      "相关工作": [],
      "后续阅读": []
    }
  }
}
```

## bucket 含义

- `urgent_follow`
  当前项目强相关，通常应该 deep read
- `direction_core`
  主研究方向强相关，通常值得 deep read
- `classics`
  某方向的经典或里程碑
- `transferable_ideas`
  跨域但可能可迁移
- `skip`
  现在不值得投入更多时间

## review 阶段规则

- review 阶段主要依据摘要、标题、作者、来源线索判断
- 每篇论文只能有一个 bucket
- `reason` 要短，但要具体
- `should_deep_read=true` 只给少量高价值论文
- 如果 `should_deep_read=false`，`deep_read` 可以留空壳
- 如果 `should_deep_read=true`，必须补完整 `deep_read` 结构
