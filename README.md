## Demonstration Videos

Here are some videos demonstrating the features and usage of these tweaks:

- [Emacs PDF-Tools Tips You Probably Don’t Know](https://youtu.be/rTtLu4QAT2I)
- [Fix to Longstanding Pdf-Tools Bug](https://youtu.be/e8n31aylkNY)

## Support & Donations

If you find this project helpful, consider supporting it!

[Donate via PayPal](https://www.paypal.com/paypalme/revrari)

# pdf-tools-tweaks

Custom fixes, enhancements, and usability tweaks for the [pdf-tools](https://github.com/politza/pdf-tools) package in Emacs, by Raoul Comninos.

---

## Included Files

- **pdf-tools-settings-fixes.el**: Main tweaks, bugfixes, and usability improvements for pdf-tools (see below).
- **pdf-export-annotations.el**: Export all or specific types of PDF annotations to org-mode files from Emacs.

---
## Exporting Annotations (pdf-export-annotations.el)

This file provides commands to export PDF annotations to both org-mode and Markdown formats, grouped by page and type, with icons and links back to the source PDF. It supports exporting all annotations or filtering by type (highlight, note, etc.).

### Features
- Export all annotations to org-mode: `M-x org-pdf-export-annotations-to-file` or `C-c C-e` in `pdf-view-mode`.
- Export all annotations to Markdown: `M-x markdown-pdf-export-annotations-to-file` or `C-c C-m` in `pdf-view-mode`.
- Export only a specific type of annotation: `M-x pdf-export-annotations-by-type` or `C-c C-t`.
- Output includes headers, page grouping, icons, selected text, and annotation notes.

### Usage
1. Ensure `pdf-export-annotations.el` is in the same directory as your other tweaks.
2. It is loaded automatically by `pdf-tools-settings-fixes.el` if present.
3. Use the commands above in any PDF buffer.

---

## Features and Fixes

- **Keybinding Customization:**
   - Remaps navigation and annotation keys for more efficient PDF reading and editing.
      - `gg` → first page, `G` → last page, `e` → goto page, `r` → revert buffer
      - `d` → delete annotation, `h`/`~`/`u`/`s` → highlight, squiggly, underline, strikeout
      - `,` → Mark (purple highlight), `a` → Box (orange squiggly)
      - `b` → create bookmark, `B` → access bookmarks, `M-d` → delete bookmark, `M-n` → rename bookmark
      - `C-c C-e` → export to org-mode, `C-c C-m` → export to Markdown, `C-c C-t` → export by type
      - Removes or disables some default bindings for clarity.

- **Annotation Bug Fixes:**
   - Robust error handling for annotation editing and deletion:
      - Automatically retries saving or deleting annotations if the annotation ID becomes stale ("No such annotation" error), refreshing the buffer and attempting again.
      - Trims trailing whitespace before saving annotation edits.
      - Ensures the cursor is positioned at the end of the annotation text when editing starts.

- **Custom Annotation Types:**
   - Adds new annotation types:
      - **Mark**: Purple highlight for marking text (`,` key).
      - **Box**: Orange squiggly underline for marking regions (`a` key).

- **PDF Viewing Improvements:**
   - Disables continuous scrolling by default.
   - Sets zoom increment to 10% for easier scaling.
   - Optionally enables midnight mode (commented out, easy to enable).

- **Password and Place Memory:**
   - Remembers all PDF passwords permanently.
   - Remembers your last place in each PDF (requires `saveplace-pdf-view`).

- **Clipboard Timeout Fix:**
   - Increases X selection timeout to avoid clipboard errors with large selections.

- **PDF Text Selection Visibility Fix:**
   - Provides a command (`my-fix-pdf-selection`) to patch PDFs that use invisible text ("3 Tr" rendering mode), making hidden or copy-protected text visible and selectable in Emacs. Requires `qpdf`.

- **Smart Quit with Save Prompt:**
   - When quitting a PDF buffer (using `q`, `Q`, or any quit-window command), if there are unsaved changes, you are prompted to save, discard, or cancel. This prevents accidental loss of annotation edits or other changes.
   - Works for all quit methods, including keybindings and window management commands.

- **PDF Bookmarks System:**
   - Create, access, rename, and delete bookmarks for quick navigation within PDFs.
   - Bookmarks are stored locally in `~/.emacs.d/pdf-bookmarks/` as `.bookmarks` files (separate from the PDF).
   - **Create bookmark** (`b`): Save current page with a custom name.
   - **Access bookmark** (`B`): Navigate to saved bookmarks via completion. Defaults to previously visited bookmark for easy toggling.
   - **Delete bookmark** (`M-d`): Remove a bookmark. Defaults to bookmark on current page if one exists.
   - **Rename bookmark** (`M-n`): Rename an existing bookmark. Defaults to bookmark on current page if one exists, skipping selection and going directly to rename prompt.
   - Smart defaults make navigation intuitive: press `B` twice to toggle between two locations.

## Usage
1. Download or copy `pdf-tools-settings-fixes.el` to your Emacs configuration directory (e.g., `~/.emacs.d/lisp/`).
2. Add the following to your Emacs init file:
   
    ```elisp
    (load "/path/to/pdf-tools-settings-fixes.el")
    (require 'pdf-tools-settings-fixes)
    ```
    Replace `/path/to/` with the actual path where you saved the file.

3. Restart Emacs or reload your configuration.

## Requirements
- [pdf-tools](https://github.com/politza/pdf-tools)
- `saveplace-pdf-view` (for place memory)
- `qpdf` (for the invisible text fix)
- `pdf-export-annotations.el` (for annotation export, optional)

## Customization
You can further adjust keybindings, annotation colors, and other settings in the file to suit your workflow.

## License
This file is provided as-is, without warranty. You may use, modify, and distribute it freely.

