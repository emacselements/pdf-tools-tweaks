# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains Emacs Lisp customizations and fixes for the pdf-tools package. It consists of two main files:
- `pdf-tools-settings-fixes.el`: Core tweaks, bugfixes, and usability improvements for pdf-tools
- `pdf-export-annotations.el`: Export PDF annotations to org-mode format

Author: Raoul Comninos

All files must include author attribution as shown in lines 1-2 of each .el file.

## Code Architecture

### Main Entry Point: pdf-tools-settings-fixes.el

This file is organized into 9 distinct sections (clearly marked with section headers):

1. **Install & Base Settings** - pdf-tools initialization and basic configuration
2. **Keybindings** - Custom key mappings for PDF navigation and annotation
3. **Robust Annotation Editing** - Core fix for "No such annotation" error with retry logic
4. **Save Last Place** - Integration with saveplace-pdf-view package
5. **Fix Invisible Text** - qpdf-based solution for invisible text (3 Tr rendering mode)
6. **Clipboard Timeout** - X selection timeout configuration
7. **Export Annotations** - Loads the pdf-export-annotations.el helper
8. **Custom Annotation Types** - "Mark" (purple highlight) and "Box" (orange squiggly) annotations
   - Defined with `defcustom` for color customization (`pdf-annot-mark-color` and `pdf-annot-box-color`)
   - Bound to `a` (mark) and `b` (box) keys
9. **Trailing Whitespace Trim** - Optional advice for cleaning annotation content
   - Can be disabled by commenting out the advice at line 267

### Key Technical Pattern: Stale Annotation ID Handling

The most important pattern in this codebase is the "retry on stale ID" mechanism used in two places:

- `pdf-annot-edit-contents-commit-with-retry` (line 59)
- `pdf-annot-delete-with-retry` (line 82)

Both functions follow this pattern:
1. Attempt the operation
2. Catch "No such annotation" error
3. Refresh buffer (`revert-buffer`)
4. For delete: re-find annotation by matching edges on the same page
5. Retry the operation

This pattern fixes a common pdf-tools bug where annotation IDs become stale after certain operations.

### Export Module: pdf-export-annotations.el

Two main functions:
- `pdf-export-annotations-to-file` (line 5) - Export all text-type annotations
  - Filters annotations to only 'text' type via `cl-remove-if-not`
  - Interactive usage: `M-x pdf-export-annotations-to-file` or `C-c C-e` in pdf-view-mode
- `pdf-export-annotations-by-type` (line 92) - Export filtered by annotation type
  - Uses `completing-read` for interactive type selection
  - Interactive usage: `M-x pdf-export-annotations-by-type` or `C-c C-t`

Both use a hash table to group annotations by page number before exporting to org-mode format with Unicode icons, quoted text, and links back to the source PDF. Keybindings defined at lines 186-187.

Export format includes:
- Org-mode headers with title, date, filetags, and startup options
- Link to source PDF with clickable file reference
- Page-grouped annotations with icon, type, subject, quoted selected text, and note content

## Development Notes

### Testing Changes

- Load the modified file in Emacs: `M-x load-file RET /path/to/file.el RET`
- Test with a PDF file that has annotations
- For annotation editing/deletion: test the retry logic by editing/deleting annotations multiple times rapidly
- For the invisible text fix: test with PDFs that use "3 Tr" rendering mode (copy-protected or invisible text PDFs)
- For export functionality: test with PDFs containing various annotation types (highlight, note, underline, etc.)

### Emacs Lisp Specifics

- All code uses lexical binding (`lexical-binding: t`)
- Uses `with-eval-after-load` for pdf-annot customizations
- Advice system: `advice-add` with `:override`, `:before`, `:after`, and `:around` combinators
- Error handling: `condition-case` for robust error recovery

### External Dependencies

- pdf-tools package (required)
- saveplace-pdf-view package (required for section 4)
- qpdf system utility (required for invisible text fix)

### File Loading

The main file loads the export module using:
```elisp
(load-file (expand-file-name "pdf-export-annotations.el"
                             (file-name-directory load-file-name)))
```

This assumes both files are in the same directory.

### Keybinding System

The package extensively customizes pdf-view-mode-map (lines 38-54, 260-261):

Navigation:
- `g g` - First page
- `G` - Last page
- `e` - Go to page
- `r` - Revert buffer

Annotation operations:
- `h` - Highlight
- `~` - Squiggly underline
- `u` - Underline
- `s` - Strikeout
- `a` - Mark (custom purple highlight)
- `b` - Box (custom orange squiggly)
- `d` - Delete annotation

Export operations:
- `C-c C-e` - Export all annotations
- `C-c C-t` - Export annotations by type

Note: The `g` key is unbound first (line 41) to allow the `g g` sequence.

### Commented Code

Lines 129-187 in pdf-tools-settings-fixes.el contain commented-out experimental code for:
- Dynamic window height based on visual line wrapping
- Alternative advice configurations

This code is preserved for reference but not currently active.

### Annotation Commit/Delete Implementation Details

The retry logic uses these key components:
- `condition-case` for error catching with specific pattern matching on "No such annotation"
- `revert-buffer` with `t t` arguments to revert without confirmation or asking about unsaved changes
- `sit-for 0.5` to allow buffer refresh to complete before retrying
- For delete operations: searches for annotation by matching `edges` property on the same page after refresh
- `pdf-annot-run-modified-hooks` must be called to properly update UI and state

### Cursor Positioning in Edit Buffer

The function `pdf-annot-edit-contents-position-cursor` (line 123) moves cursor to end when annotation editing starts, but requires proper buffer context. It checks for:
- `pdf-annot-edit-contents-minor-mode` being active
- `pdf-annot-edit-contents--annotation` being set

### Advice System Usage

Active advice in the codebase:
- Line 267: `:before` advice on `pdf-annot-edit-contents-finalize` for trailing whitespace trimming
- Commented lines 178-183 show alternative `:override` and `:around` advice patterns that were tested but not currently active

When modifying advice, always check if old advice needs removal first (see cleanup pattern at line 185-187).
