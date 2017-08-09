# .emacs.d

My personal Emacs config.

It makes heavy use
of [`use-package`](https://github.com/jwiegley/use-package), downloading
required packages from [elpa](https://elpa.gnu.org/)
and [melpa](https://melpa.org/). Even my own personal packages (which make
no sense being contributed to public package archives) live
in [a package archive](http://blog.davep.org/delpa/).

The current look of my
config
[started life in early 2016 when I killed off my original `~/.emacs` and started using what I think is a cleaner approach via `~/.emacs.d`](http://blog.davep.org/2016/05/26/starting_fresh_with_gnu_emacs.html).
About a year later I made a few more significant changes
to
[make things work even better](http://blog.davep.org/2017/04/01/another_revamp_of_my_emacs_config.html),
with
[another big revamp to switch from using `customize` happening about 3 months later](http://blog.davep.org/2017/07/13/more_revamping_of_my_emacs_config.html).

As with any good development setup, it's an ongoing process and will
continue to evolve. While I obviously am highly unlikely to accept pull
requests for this repo, feedback and ideas for how I might improve it are
always welcome.

## Notable bindings

What follows is a non-comprehensive collection of key-binding details.

### Function keys

| Binding | Function |
| --- | --- |
| <kbd>Meta</kbd>+<kbd>F6</kbd> | Show iBuffer |
| <kbd>F7</kbd> | List personal key bindings |
| <kbd>F11</kbd> | Show current Emacs uptime |
| <kbd>Meta</kbd>+<kbd>F11</kbd> | Show Emacs uptimes |
| <kbd>Ctrl</kbd>+<kbd>F11</kbd> | Show binary clock |
| <kbd>Ctrl</kbd>+<kbd>Meta</kbd>+<kbd>F11</kbd> | Nuke buffers |
| <kbd>F12</kbd> <kbd>h</kbd> | Open eshell |
| <kbd>F12</kbd> <kbd>k</kbd> | Browse the kill-ring |
| <kbd>F12</kbd> <kbd>r</kbd> | Open elisp REPL (ielm) |
| <kbd>F12</kbd> <kbd>s</kbd> | Switch to scratch buffer |
| <kbd>F12</kbd> <kbd>w</kbd> | Open eww (web browser) |
| <kbd>F12</kbd> <kbd>x</kbd> | Open wttrin (as in "view wX") |

### Moving around

| Binding | Function |
| --- | --- |
| <kbd>home</kbd> | Context-aware "home" |
| <kbd>end</kbd> | Context-aware "end" |
| <kbd>Super</kbd>+<kbd>left</kbd> | Context-aware "home" |
| <kbd>Super</kbd>+<kbd>right</kbd> | Context-aware "end" |
| <kbd>Meta</kbd>+<kbd>left</kbd> | Backward sexp |
| <kbd>Meta</kbd>+<kbd>right</kbd> | Forward sexp |
| <kbd>Super</kbd>+<kbd>up</kbd> | Backward page |
| <kbd>Super</kbd>+<kbd>down</kbd> | Forward page |

### Packages

| Binding | Function |
| --- | --- |
| <kbd>F12</kbd> <kbd>p</kbd> <kbd>l</kbd> | List packages |
| <kbd>F12</kbd> <kbd>p</kbd> <kbd>r</kbd> | Refresh package lists |
| <kbd>F12</kbd> <kbd>p</kbd> <kbd>p</kbd> | Lint the current buffer as a package |
| <kbd>F12</kbd> <kbd>p</kbd> <kbd>u</kbd> | Upload current buffer as a package, to delpa |

### [boxquote.el](https://github.com/davep/boxquote.el)

| Binding | Function |
| --- | --- |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>i</kbd> | `boxquote-insert-file` |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>Meta</kbd>+<kbd>w</kbd> | `boxquote-kill-ring-save` |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>y</kbd> | `boxquote-yank` |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>b</kbd> | `boxquote-region` |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>t</kbd> | `boxquote-title` |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>h</kbd> <kbd>f</kbd> | `boxquote-describe-function` |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>h</kbd> <kbd>v</kbd> | `boxquote-describe-variable` |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>h</kbd> <kbd>k</kbd> | `boxquote-describe-key` |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>h</kbd> <kbd>w</kbd> | `boxquote-where-is` |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>!</kbd> | `boxquote-shell-command` |

### [cheat-sh.el](https://github.com/davep/cheat-sh.el)

| Binding | Function |
| --- | --- |
| <kbd>f12</kbd> <kbd>/</kbd> <kbd>/</kbd> | Search cheat-sh, using region if there is one |
| <kbd>f12</kbd> <kbd>/</kbd> <kbd>l</kbd> | List sheets for a given item |
| <kbd>f12</kbd> <kbd>/</kbd> <kbd>?</kbd> | Show cheat-sh help |
| <kbd>f12</kbd> <kbd>/</kbd> <kbd>s</kbd> | Search cheat-sh |
| <kbd>f12</kbd> <kbd>/</kbd> <kbd>t</kbd> | Search a specific topic on cheat-sh |

### [insert.el](https://github.com/davep/insert.el)

| Binding | Function |
| --- | --- |
| <kbd>F12</kbd> <kbd>i</kbd> <kbd>a</kbd> | Insert an autoload cookie for current `defun` |
| <kbd>F12</kbd> <kbd>i</kbd> <kbd>f</kbd> | Insert a filename |
| <kbd>F12</kbd> <kbd>i</kbd> <kbd>m</kbd> | Insert a melpa badge for a package |
| <kbd>F12</kbd> <kbd>i</kbd> <kbd>s</kbd> | Insert Emacs sexp link quotes around current symbol |
| <kbd>F12</kbd> <kbd>i</kbd> <kbd>y</kbd> | Insert markdown code for a YouTube video preview |
| <kbd>F12</kbd> <kbd>i</kbd> <kbd>;</kbd> | Insert my own style of "break here" comment |

### [winsplit.el](https://github.com/davep/winsplit.el)

| Binding | Function |
| --- | --- |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>right</kbd> | Open window to the right of the current window |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>left</kbd> | Open window to the left of the current window |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>up</kbd> | Open window above current window |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>down</kbd> | Open window below current window |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>Ctrl</kbd>+<kbd>right</kbd> | Open window to the right of the current window and prompt for a file |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>Ctrl</kbd>+<kbd>left</kbd> | Open window to the left of the current window and prompt for a file |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>Ctrl</kbd>+<kbd>up</kbd> | Open window above current window and prompt for a file |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>Ctrl</kbd>+<kbd>down</kbd> | Open window below current window and prompt for a file |
