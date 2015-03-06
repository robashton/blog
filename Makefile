default: all

MARKDOWN := $(wildcard in/*.txt)
INPUTHTML := $(patsubst in/%,out/%, $(INPUTS))


out/%.txt: input/pages/*/%.html site/entries
	cp $< $@

site/entries:
	mkdir -p site/entries

site/entries/%.html:

