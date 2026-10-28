---
name: ask
description: Explain code and discuss possible changes without implementing them.
disable-model-invocation: true
---

Answer the user's question about this codebase:

$ARGUMENTS

Instructions:
- Read relevant files to understand the code.
- Explain the current behavior and cite relevant file paths.
- Discuss possible changes and tradeoffs when helpful.
- Do not create, edit, or delete files.
- Do not run commands that change files or external state.
- Show suggested code only in your response.
