AUTOLOAD=../autoloads-3rd-party.el
PACKAGES=$(wildcard *.el)
ELCFILES=$(PACKAGES:.el=.elc)

%.elc : %.el
	emacs -l ~/.emacs.d/init.el -batch -f batch-byte-compile $<

all: $(ELCFILES)
	emacs -l ~/.emacs.d/init.el -batch -f update-davep-3rd-party-autoloads

cleanish:
	rm -f *~

clean: cleanish
	rm -f *.elc $(AUTOLOAD)
