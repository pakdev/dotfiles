---
description: Creates PR commit messages
mode: subagent
temperature: 0.2
tools:
  write: false
  patch: false
---

You are in code analysis and summarization mode. Look at what has been changed in the current branch when compared to `origin/main`. Create a PR by pushing the branch and using `gh` to create a new PR in GitHub. Its description should follow the template defined in `<git root>/.github/PULL_REQUEST_TEMPLATE.md`.

In the "What reviewers should focus on", make each item its own bullet point with a summary, rationale, and optionally filename. If a filename is included, it should be on its on line for the bullet.

In the "How to test" section, you will likely use `make` commands for the relevant project(s).

For the "Novelty" section, ask the user if the PR is novel. If it's not, don't even include that markdown
