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

### General/misc keys

| Binding                                        | Function                                                                                   |
| ---                                            | ---                                                                                        |
| <kbd>Meta</kbd>+<kbd>s</kbd>                   | [Switch to `*scratch*`](https://github.com/davep/itch.el)                                  |
| <kbd>F6</kbd>                                  | Switch to next buffer                                                                      |
| <kbd>Ctrl</kbd>+<kbd>F6</kbd>                  | Process list                                                                               |
| <kbd>Meta</kbd>+<kbd>F6</kbd>                  | Show iBuffer                                                                               |
| <kbd>F11</kbd>                                 | [Show current Emacs uptime](https://github.com/davep/uptimes.el)                           |
| <kbd>F12</kbd> <kbd>F11</kbd>                  | [Show Emacs uptimes](https://github.com/davep/uptimes.el)                                  |
| <kbd>Ctrl</kbd>+<kbd>Meta</kbd>+<kbd>F11</kbd> | [Nuke buffers](https://github.com/davep/nuke-buffers.el)                                   |
| <kbd>F12</kbd> <kbd>F12</kbd>                  | List personal key bindings                                                                 |
| <kbd>F12</kbd> <kbd>e</kbd>                    | [Open `eg`](https://github.com/davep/eg.el)                                                |
| <kbd>F12</kbd> <kbd>h</kbd>                    | Open `eshell`                                                                              |
| <kbd>C</kbd>+<kbd>x</kbd> <kbd>y</kbd>         | Browse the kill-ring                                                                       |
| <kbd>F12</kbd> <kbd>r</kbd>                    | Open elisp REPL (`ielm`)                                                                   |
| <kbd>F12</kbd> <kbd>s</kbd>                    | [Toggle through camelCase, snake-case, etc.](https://github.com/akicho8/string-inflection) |
| <kbd>F12</kbd> <kbd>w</kbd>                    | Open `eww` (web browser)                                                                   |
| <kbd>F12</kbd> <kbd>x</kbd>                    | Open `wttrin` (as in "view wX")                                                            |
| <kbd>F12</kbd> <kbd>o</kbd> <kbd>a</kba>       | Org agenda view                                                                            |
| <kbd>F12</kbd> <kbd>o</kbd> <kbd>t</kba>       | Org todo view                                                                              |
| <kbd>F12</kbd> <kbd>q</kbd>                    | [requote](https://github.com/davep/requote.el)                                             |
| <kbd>F8</kbd>                                  | [neotree](https://github.com/jaypei/emacs-neotree)                                         |
| <kbd>F12</kbd> <kbd>i</kbd> <kbd>e</kbd>       | [Add an end-of-file marker](https://github.com/davep/end-it.el)                            |
| <kbd>F12</kbd> <kbd>?</kbd> <kbd>w</kbd>       | [woman](https://www.gnu.org/software/emacs/manual/html_mono/woman.html)                    |

### [Finding things](https://github.com/abo-abo/swiper)

| Binding                                                  | Function                     |
| ---                                                      | ---                          |
| <kbd>F12</kbd> <kbd>f</kbd> <kbd>g</kbd>                 | Find files with `ripgrep`    |
| <kbd>F12</kbd> <kbd>f</kbd> <kbd>l</kbd>                 | Find files with `locate`     |
| <kbd>F12</kbd> <kbd>f</kbd> <kbd>r</kbd>                 | Open a recent file           |
| <kbd>F12</kbd> <kbd>f</kbd> <kbd>Meta</kbd>+<kbd>x</kbd> | Find a recently-used command |
| <kbd>F12</kbd> <kbd>f</kbd> <kbd>c</kbd> <kbd>w</kbd>    | Find a web colour            |
| <kbd>F12</kbd> <kbd>f</kbd> <kbd>c</kbd> <kbd>e</kbd>    | Find an Emacs colour         |

### Moving around

| Binding                                  | Function                                                   |
| ---                                      | ---                                                        |
| <kbd>Meta</kbd>+<kbd>g</kbd>             | Goto line                                                  |
| <kbd>Super</kbd>+<kbd>s</kbd>            | Forward search symbol at point                             |
| <kbd>Home</kbd>                          | [Context-aware "home"](https://github.com/davep/moving.el) |
| <kbd>End</kbd>                           | [Context-aware "end"](https://github.com/davep/moving.el)  |
| <kbd>Super</kbd>+<kbd>Left</kbd>         | [Context-aware "home"](https://github.com/davep/moving.el) |
| <kbd>Super</kbd>+<kbd>Right</kbd>        | [Context-aware "end"](https://github.com/davep/moving.el)  |
| <kbd>Meta</kbd>+<kbd>Left</kbd>          | Backward sexp                                              |
| <kbd>Meta</kbd>+<kbd>Right</kbd>         | Forward sexp                                               |
| <kbd>Super</kbd>+<kbd>Up</kbd>           | Backward page                                              |
| <kbd>Super</kbd>+<kbd>Down</kbd>         | Forward page                                               |
| <kbd>F12</kbd> <kbd>m</kbd> <kbd>b</kbd> | Create bookmark here                                       |
| <kbd>F12</kbd> <kbd>m</kbd> <kbd>l</kbd> | View bookmarks                                             |
| <kbd>F12</kbd> <kbd>m</kbd> <kbd>g</kbd> | Go to bookmark                                             |

### Packages

| Binding                                  | Function                                                                     |
| ---                                      | ---                                                                          |
| <kbd>F12</kbd> <kbd>p</kbd> <kbd>l</kbd> | List packages                                                                |
| <kbd>F12</kbd> <kbd>p</kbd> <kbd>r</kbd> | Refresh package lists                                                        |
| <kbd>F12</kbd> <kbd>p</kbd> <kbd>t</kbd> | Lint the current buffer as a package                                         |
| <kbd>F12</kbd> <kbd>p</kbd> <kbd>u</kbd> | Upload current buffer as a package, to [delpa](http://blog.davep.org/delpa/) |

### Magit

| Binding                                               | Function                  |
| ---                                                   | ---                       |
| <kbd>F12</kbd> <kbd>g</kbd> <kbd>s</kbd>              | Repo status               |
| <kbd>F12</kbd> <kbd>g</kbd> <kbd>l</kbd> <kbd>a</kbd> | View log for whole repo   |
| <kbd>F12</kbd> <kbd>g</kbd> <kbd>l</kbd> <kbd>f</kbd> | View log for current file |
| <kbd>F12</kbd> <kbd>g</kbd> <kbd>i</kbd>              | View issues (via Forge)   |

### [boxquote.el](https://github.com/davep/boxquote.el)

| Binding                                                  | Function                     |
| ---                                                      | ---                          |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>i</kbd>                 | `boxquote-insert-file`       |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>Meta</kbd>+<kbd>w</kbd> | `boxquote-kill-ring-save`    |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>y</kbd>                 | `boxquote-yank`              |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>b</kbd>                 | `boxquote-region`            |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>t</kbd>                 | `boxquote-title`             |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>h</kbd> <kbd>f</kbd>    | `boxquote-describe-function` |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>h</kbd> <kbd>v</kbd>    | `boxquote-describe-variable` |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>h</kbd> <kbd>k</kbd>    | `boxquote-describe-key`      |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>h</kbd> <kbd>w</kbd>    | `boxquote-where-is`          |
| <kbd>F12</kbd> <kbd>b</kbd> <kbd>!</kbd>                 | `boxquote-shell-command`     |

### [cheat-sh.el](https://github.com/davep/cheat-sh.el)

| Binding                                  | Function                                      |
| ---                                      | ---                                           |
| <kbd>F12</kbd> <kbd>/</kbd> <kbd>/</kbd> | Search cheat-sh, using region if there is one |
| <kbd>F12</kbd> <kbd>/</kbd> <kbd>l</kbd> | List sheets for a given item                  |
| <kbd>F12</kbd> <kbd>/</kbd> <kbd>?</kbd> | Show cheat-sh help                            |
| <kbd>F12</kbd> <kbd>/</kbd> <kbd>s</kbd> | Search cheat-sh                               |
| <kbd>F12</kbd> <kbd>/</kbd> <kbd>t</kbd> | Search a specific topic on cheat-sh           |

### [insert.el](https://github.com/davep/insert.el)

| Binding                                  | Function                                                 |
| ---                                      | ---                                                      |
| <kbd>F12</kbd> <kbd>i</kbd> <kbd>a</kbd> | Insert an autoload cookie for current `defun`            |
| <kbd>F12</kbd> <kbd>i</kbd> <kbd>f</kbd> | Insert a filename                                        |
| <kbd>F12</kbd> <kbd>i</kbd> <kbd>m</kbd> | Insert a [melpa](https://melpa.org/) badge for a package |
| <kbd>F12</kbd> <kbd>i</kbd> <kbd>s</kbd> | Insert Emacs sexp link quotes around current symbol      |
| <kbd>F12</kbd> <kbd>i</kbd> <kbd>y</kbd> | Insert markdown code for a YouTube video preview         |
| <kbd>F12</kbd> <kbd>i</kbd> <kbd>;</kbd> | Insert my own style of "break here" comment              |

### [winsplit.el](https://github.com/davep/winsplit.el)

| Binding                                                       | Function                                                             |
| ---                                                           | ---                                                                  |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>Right</kbd>                 | Open window to the right of the current window                       |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>Left</kbd>                  | Open window to the left of the current window                        |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>Up</kbd>                    | Open window above current window                                     |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>Down</kbd>                  | Open window below current window                                     |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>Ctrl</kbd>+<kbd>Right</kbd> | Open window to the right of the current window and prompt for a file |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>Ctrl</kbd>+<kbd>Left</kbd>  | Open window to the left of the current window and prompt for a file  |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>Ctrl</kbd>+<kbd>Up</kbd>    | Open window above current window and prompt for a file               |
| <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>Ctrl</kbd>+<kbd>Down</kbd>  | Open window below current window and prompt for a file               |

## Licence

It hardly seems worth having a licence on something that's intended for my
own use. However, on the off chance that someone wants to derive from this:

> This program is free software: you can redistribute it and/or modify it
> under the terms of the GNU General Public License as published by the Free
> Software Foundation, either version 3 of the License, or (at your option)
> any later version.
>
> This program is distributed in the hope that it will be useful, but
> WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
> or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
> for more details.
>
> You should have received a copy of the GNU General Public License along
> with this program. If not, see <http://www.gnu.org/licenses/>.
