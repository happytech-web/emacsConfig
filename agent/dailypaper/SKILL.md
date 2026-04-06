---
name: dailypaper
description: |
  以 org-roam 为中心的论文推荐与精读 workflow。用户说“今日论文推荐”“过去3天论文推荐”
  “过去一周论文推荐”“最近几天论文”“推荐和当前项目相关的论文”“推荐某方向经典论文”
  “找可以迁移到我方向的方法”时使用。该 skill 会先抓取候选论文，再做 0-token 过滤，
  然后由 agent 完成 review、打分、分桶与是否 deep read 的判断，最后通过 Emacs
  org-roam API 把推荐结果写入 org-roam daily，并为 deep-read 论文创建正式
  org-roam paper note。
---

# dailypaper

把这个 skill 当成“一句话触发完整论文工作流”的入口。

## 快速原则

- 先识别用户意图属于：
  - `recommend`
  - `classic`
  - `transfer`
- 优先调用本地 CLI 完成 `fetch -> filter -> finalize`
- `recommend` / `transfer` 前，优先读取 `~/RoamNotes/projects/dailypaper-selector-profile.org`
  作为当前项目、研究方向、关键词偏好和过滤权重的来源
- 不要把最终推荐逻辑偷换成纯关键词脚本
- 生成后的 org 文件才是最终产物，不是聊天文本
- org 文件必须通过 Emacs org-roam API 创建或更新，不要直接手写文件落盘

## 入口映射

- 最近论文推荐：
  - 运行 `python -m agent.dailypaper recommend --days N`
- 项目驱动推荐：
  - 默认先读取 `~/RoamNotes/projects/dailypaper-selector-profile.org`
  - 如果用户明确要求使用已配置项目，再运行 `python -m agent.dailypaper recommend --days N --project PROFILE`
  - 如果用户只给了临时自由描述，再运行 `python -m agent.dailypaper recommend --days N --project-text "..."`
- 经典论文学习：
  - 运行 `python -m agent.dailypaper classic --topic TOPIC`
- 跨域可迁移方法：
  - 运行 `python -m agent.dailypaper transfer --topic TOPIC`

## 执行流程

1. 运行 CLI，先产出 review packet。
2. 让 workflow 从 arXiv 和 HuggingFace Daily/Trending 抓取候选论文。
3. 对 `recommend` / `transfer`，先读取 selector profile，再让 `0-token filter` 根据其中的项目描述、当前方向、include/boost/exclude/source 偏好与 scoring weights 缩小候选集。
4. 读取 review packet，由 agent 对每篇候选做 review：
   - 给分
   - 分桶
   - 写推荐理由
   - 判断是否 `should_deep_read`
5. 只对 `should_deep_read=true` 的论文做 deep read。
6. deep read 必须按中文结构化模板填写，不得只写几句摘要。
7. 保存 reviewed JSON 后，运行：
   - `python -m agent.dailypaper finalize --input REVIEWED_JSON`
8. 最后简短汇报：
   - 更新了哪个 daily 文件
   - 新建了多少篇 paper note
   - 是否刷新了索引

## 重要约束

- 不要跳过 agent review，不能把关键词过滤当成最终推荐。
- 不要对所有过滤后的论文都 deep read，只精读少量高价值论文。
- 如果 CLI 已经生成了 org 结果，不要手工重写 daily。
- 不要直接用 Python 写最终 org 文件；必须走 Emacs bridge / org-roam API，确保 `ID` 和数据库索引正确。
- deep read 默认用中文写。
- 对 `should_deep_read=true` 的论文，deep read 必须满足最低质量标准。

## 需要时再读的参考文件

- org-roam 落库结构与分桶语义：读 `references/workflow.md`
- CLI 命令与分阶段契约：读 `references/runner-contract.md`
- reviewed JSON 的结构：读 `references/review-format.md`
- 合格 deep read 的标准：读 `references/deep-read-standards.md`
- 如果要调整研究方向、项目描述或筛选偏好：优先让用户编辑 `~/RoamNotes/projects/dailypaper-selector-profile.org`
