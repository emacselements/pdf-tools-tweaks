# PDF Tools Tweaks

Custom fixes, enhancements, and usability tweaks for the [pdf-tools](https://github.com/politza/pdf-tools) package in Emacs.

## 📹 Demonstration Videos

Here are some videos demonstrating the features and usage of these tweaks:

- [Emacs PDF-Tools Tips You Probably Don't Know](https://youtu.be/rTtLu4QAT2I)
- [Fix to Longstanding Pdf-Tools Bug](https://youtu.be/e8n31aylkNY)

## 💝 Support & Donations

If you find this project helpful, consider supporting it!

[Donate via PayPal](https://www.paypal.com/paypalme/revrari)

## 📦 Included Files

- **`pdf-tools-settings-fixes.el`**: Main tweaks, bugfixes, and usability improvements for pdf-tools
- **`pdf-export-annotations.el`**: Export all or specific types of PDF annotations to org-mode and Markdown files
- **`pdf-bookmarks.el`**: Simple bookmark system for PDFs with create, navigate, rename, delete, and toggle-back features
- **`pdf-tools/`**: Local copy of pdf-tools package for integration with custom tweaks

## 🚀 Quick Start

### Option 1: Using the included pdf-tools directory

1. Clone this repository to your Emacs configuration directory:
   ```bash
   cd ~/.emacs.d
   git clone https://github.com/emacselements/pdf-tools-tweaks.git
   ```

2. Add the following to your Emacs init file:
   ```elisp
   ;; Load pdf-tools-settings-fixes (includes pdf-tools setup)
   (require 'pdf-tools-settings-fixes)
   
   ;; Load bookmark system (optional but recommended)
   (require 'pdf-bookmarks)
   
   ;; Load annotation export (optional)
   (require 'pdf-export-annotations)
   ```

3. Restart Emacs

### Option 2: Using pdf-tools from ELPA

1. Download the `.el` files to your Emacs configuration directory (e.g., `~/.emacs.d/lisp/`)
2. Install pdf-tools from MELPA: `M-x package-install RET pdf-tools RET`
3. Add to your init file:
   ```elisp
   (require 'pdf-tools-settings-fixes)
   (require 'pdf-bookmarks)
   (require 'pdf-export-annotations)  ; optional
   ```

4. Restart Emacs or reload your configuration

## 🔖 PDF Bookmarks System

A simple, file-based bookmark system for PDF navigation. Bookmarks are stored per-PDF in `~/.emacs.d/pdf-bookmarks/` and sorted by page number.

### ⌨️ Keybindings

| Key | Action | Description |
|-----|--------|-------------|
| `' b` | Create bookmark | Creates a bookmark on the current page with zero-padded page numbers |
| `' g` | Go to bookmark | Shows sorted list of bookmarks, pre-selects current page bookmark if exists |
| `' d` | Delete bookmark | Delete a bookmark, defaults to current page bookmark if available |
| `' r` | Rename bookmark | Rename a bookmark with old name pre-filled |
| `' l` | Toggle back | Toggle between current position and last bookmark location |
| `' m` | Migrate bookmarks | Migrate bookmarks from a renamed PDF file using fuzzy filename matching |

### ✨ Features

- **Smart naming**: Bookmarks display as "Page 003 -- bookmark-name" for proper numerical sorting
- **Context awareness**: When on a bookmarked page, delete/rename operations default to that bookmark
- **Position memory**: Toggle-back saves your position before jumping, allowing easy return
- **Clean interface**: All bookmark operations clear the minibuffer immediately after completion
- **Migration support**: When using bookmarks on a renamed PDF, you'll be prompted to migrate from similar files (30%+ similarity). The original bookmark file is backed up then deleted to prevent conflicts.
- **Standard Emacs keybindings for annotations**:
  - `C-x C-s` - Save annotation without closing (standard save behavior)
  - `C-x 0` - Abort editing and close window (auto-deletes empty annotations)
  - `C-c C-c` - Save annotation and close window
- **Markdown mode for annotation editing**: Annotation buffers use markdown-mode for better editing experience
- **Line break preservation**: Tooltips now preserve line breaks in multi-line annotations

## 📤 Annotation Export System

Export PDF annotations to both org-mode and Markdown formats, grouped by page and type, with icons and links back to the source PDF.

### ⌨️ Keybindings

| Key | Action | Description |
|-----|--------|-------------|
| `C-c C-e` | Export to org-mode | Export all annotations to org-mode format |
| `C-c C-m` | Export to Markdown | Export all annotations to Markdown format |
| `C-c C-t` | Export by type | Export only specific annotation types |

### ✨ Features

- **Multiple formats**: Export to both org-mode and Markdown
- **Type filtering**: Export all annotations or filter by specific types (highlight, note, etc.)
- **Rich output**: Includes headers, page grouping, icons, selected text, and annotation notes
- **Source linking**: Links back to the original PDF location
- **Auto-loading**: Automatically loaded by `pdf-tools-settings-fixes.el` if present

## ⚡ Core Features and Fixes

### 🎯 Keybinding Customization

Enhanced navigation and annotation keys for efficient PDF reading:

#### Navigation & Buffer Operations
| Key | Action | Description |
|-----|--------|-------------|
| `gg` | First page | Jump to the first page |
| `G` | Last page | Jump to the last page |
| `e` | Go to page | Navigate to specific page number |
| `r` | Revert buffer | Refresh the PDF view |

#### Search
| Key | Action | Description |
|-----|--------|-------------|
| `C-s` | Search document | Search across all pages (standard isearch) |
| `C-u C-s` | Search current page | Search only within the current page |

#### Annotation Operations
| Key | Action | Description |
|-----|--------|-------------|
| `d` | Delete annotation | Remove the current annotation |
| `h` | Yellow highlight | Add standard yellow highlight |
| `~` | Squiggly underline | Add squiggly underline annotation |
| `u` | Underline | Add underline annotation |
| `s` | Strikeout | Add strikeout annotation |

#### Custom Annotation Types
| Key | Action | Color | Description |
|-----|--------|-------|-------------|
| `,` | Mark | Purple | Purple highlight for marking important text |
| `a` | Box | Orange | Orange squiggly underline for marking regions |
| `.` | Green highlight | Green | Green highlight for categorizing text |

### 🔧 Robust Annotation Bug Fixes

- **Automatic retry mechanism**: Handles "No such annotation" errors by refreshing buffer and retrying
- **Text cleanup**: Automatically trims trailing whitespace before saving annotation edits
- **Cursor positioning**: Ensures cursor is positioned at the end of annotation text when editing starts
- **Error recovery**: Graceful handling of stale annotation IDs during editing and deletion

### 🔍 Enhanced Search Features

- **Page-restricted search**: Use `C-u C-s` to search only within the current page
- **Automatic restriction reset**: Search restriction is automatically cleared after the search ends
- **Standard isearch integration**: Works seamlessly with Emacs' built-in isearch functionality

### 🎨 PDF Viewing Improvements

- **Discrete page scrolling**: Disables continuous scrolling by default for better page-by-page reading
- **Optimal zoom increments**: Sets zoom to 10% increments for easier scaling
- **Midnight mode ready**: Optional dark mode support (commented out, easy to enable)

### 💾 Memory & State Management

- **Password memory**: Remembers all PDF passwords permanently across sessions
- **Place memory**: Remembers your last position in each PDF (requires `saveplace-pdf-view`)
- **Smart quit**: Prompts to save unsaved changes when quitting PDF buffers

### 🔧 Technical Fixes

- **Clipboard timeout fix**: Increases X selection timeout to avoid clipboard errors with large text selections
- **Invisible text handling**: Makes invisible text (3 Tr) visible for better accessibility

## 📋 Requirements

- **[pdf-tools](https://github.com/politza/pdf-tools)** - The base PDF viewing package for Emacs
- **`saveplace-pdf-view`** - For remembering last position in PDFs
- **`qpdf`** - For the invisible text visibility fix
- **`cl-lib`** - For the bookmark system (usually built-in with Emacs)

Optional components:
- **`pdf-export-annotations.el`** - For annotation export functionality
- **`pdf-bookmarks.el`** - For the bookmark system

## 🔧 Customization

All settings can be customized by modifying the `.el` files:

- **Keybindings**: Modify the `define-key` statements in `pdf-tools-settings-fixes.el`
- **Annotation colors**: Adjust color values in the custom annotation type definitions
- **Zoom increments**: Change `pdf-view-resize-factor` value
- **Bookmark storage**: Modify `pdf-bookmarks-dir` variable in `pdf-bookmarks.el`

## 📄 License

This project is provided as-is, without warranty. You may use, modify, and distribute it freely.

