# PDF Tools Tweaks

Custom fixes, enhancements, and usability tweaks for the [pdf-tools](https://github.com/politza/pdf-tools) package in Emacs, by Raoul Comninos.

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

## 🚀 Quick Start

1. Download all three `.el` files to your Emacs configuration directory (e.g., `~/.emacs.d/lisp/`)
2. Add the following to your Emacs init file:
   
   ```elisp
   ;; Load the main tweaks file (this will auto-load the others)
   (load "/path/to/pdf-tools-settings-fixes.el")
   (require 'pdf-tools-settings-fixes)
   
   ;; Load bookmark system (optional but recommended)
   (load "/path/to/pdf-bookmarks.el")
   (require 'pdf-bookmarks)
   ```
   
   Replace `/path/to/` with the actual path where you saved the files.

3. Restart Emacs or reload your configuration

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

