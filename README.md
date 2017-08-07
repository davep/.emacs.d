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
| <kbd>M-F6</kbd> | Show iBuffer |
| <kbd>F7</kbd> | List personal key bindings |
| <kbd>F11</kbd> | Show current Emacs uptime |
| <kbd>M-F11</kbd> | Show Emacs uptimes |
| <kbd>C-F11</kbd> | Show binary clock |
| <kbd>C-M-F11</kbd> | Nuke buffers |
| <kbd>F12</kbd> <kbd>h</kbd> | Open eshell |
| <kbd>F12</kbd> <kbd>r</kbd> | Open elisp REPL (ielm) |
| <kbd>F12</kbd> <kbd>s</kbd> | Switch to scratch buffer |
| <kbd>F12</kbd> <kbd>w</kbd> | Open eww (web browser) |

### [insert.el](https://github.com/davep/insert.el)

| Binding | Function |
| --- | --- |
| <kbd>C-c</kbd> <kbd>i</kbd> <kbd>a</kbd> | Insert an autoload cookie for current `defun` |
| <kbd>C-c</kbd> <kbd>i</kbd> <kbd>f</kbd> | Insert a filename |
| <kbd>C-c</kbd> <kbd>i</kbd> <kbd>m</kbd> | Insert a melpa badge for a package |
| <kbd>C-c</kbd> <kbd>i</kbd> <kbd>s</kbd> | Insert Emacs sexp link quotes around current symbol |
| <kbd>C-c</kbd> <kbd>i</kbd> <kbd>y</kbd> | Insert markdown code for a YouTube video preview |
| <kbd>C-c</kbd> <kbd>i</kbd> <kbd>;</kbd> | Insert my own style of "break here" comment |
