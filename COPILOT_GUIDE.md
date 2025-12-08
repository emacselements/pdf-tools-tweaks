# COPILOT_GUIDE.md

## General Project Rules

### Reference the pdf-tools package inside this repo to resolve code issues. Don't guess. The package is kept here: pdf-tools/

### Author Attribution
- All code files must include this in the correct comment style:
  ```
  Author: Raoul Comninos
  ```
- Add to every new file.
- Confirm existing files include it.

### Lisp Code Standards
- Ensure parentheses are balanced.
- Indentation must clearly reflect structure.
- Fix common macro-related paren errors.

### Support Message in README
Every README must include:

If you find this project helpful, consider supporting it!

[Donate via PayPal](https://www.paypal.com/paypalme/revrari)

### Commit Messages
- Keep messages short and factual.
- No AI attribution lines.
- If asked to "commit," generate a suitable brief message.

### Markdown Formatting Notes
Preserve double spaces when required for formatting:

```
Highlighting Commands  
nov-highlights-green g  
nov-highlights-orange h  
```

### README Guidance
- Update `README.md` when behavior or usage changes.
- Keep documentation focused, not verbose.

### Respect Project Boundaries
Do not modify:
- `.emacs.d/`
- Any personal or sensitive files

---

## Research & Accuracy
- If unsure, say so — do not guess.
- Check authoritative sources for conventions and best practices.
- Correct mistakes promptly if discovered.

---

## MELPA Submission Guidelines (for Emacs Packages)

### Required Package Headers
Main `.el` file must include:

```elisp
;;; package-name.el --- Description -*- lexical-binding: t -*-

;; Copyright (C) YEAR Author Name
;; Author: Raoul Comninos
;; Version: X.Y.Z
;; Package-Requires: ((emacs "VERSION") (dependency "VERSION"))
;; Keywords: keywords
;; URL: https://github.com/username/package-name
;; SPDX-License-Identifier: LICENSE-TYPE

;;; Commentary:
;; Short overview (80 char max per line)

;;; Code:

(provide 'package-name)
;;; package-name.el ends here
```

### Naming Standard
- Public symbols: `package-name-…`
- Private symbols: `package-name--…`

### License Requirements
- Root `LICENSE` file required.
- SPDX identifier must match license text.

### Code Quality Checks
Run and fix results from:

```bash
emacs -Q --batch -f batch-byte-compile package-name.el
emacs -Q --batch --eval "(progn (require 'checkdoc) (checkdoc-file \"package-name.el\"))"
```

Fix:
- Docstring length (≤80 chars)
- Use `#'function` for references
- Remove trailing whitespace
- Avoid top-level `setq`

### Documentation Requirements
- README with installation + usage examples
- Commentary block in `.el` file summarizing package

---

## AI Instruction Notes
- This file is for Copilot's awareness inside this repository.
- Follow its conventions when generating or updating code.
