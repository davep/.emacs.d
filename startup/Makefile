PACKAGES=$(wildcard *.el)
ELCFILES=$(PACKAGES:.el=.elc)

%.elc : %.el
	emacs -l ~/.emacs.d/init.el -batch -f batch-byte-compile $<

all: $(ELCFILES)

cleanish:
	rm -f *~

clean: cleanish
	rm -f *.elc
