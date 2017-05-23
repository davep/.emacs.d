# .emacs.d

My personal emacs config.

It's based quite heavily around the use of `customize` in preference to
setting values "by hand" in `init.el` and friends, and also around the use
of package archive-installed packages in preference to maintaining a local
library of code (archives currently used are [elpa](https://elpa.gnu.org/)
and [melpa](https://melpa.org/)). Even my own personal packages which make
no sense being contributed to public package archives live
in [a personal package archive](http://blog.davep.org/delpa/).

It heavily uses [`use-package`](https://github.com/jwiegley/use-package).

The current look of my
config
[started life in early 2016 when I killed off my original `~/.emacs` and started using what I think is a saner approach via `~/.emacs.d`](http://blog.davep.org/2016/05/26/starting_fresh_with_gnu_emacs.html).
About a year later I made a few more significant changes
to
[make things work even better](http://blog.davep.org/2017/04/01/another_revamp_of_my_emacs_config.html).

As with any good development setup, it's an ongoing process and will
continue to evolve. While I obviously am highly unlikely to accept pull
requests for this repo, feedback and ideas for how I might improve it are
always welcome.
