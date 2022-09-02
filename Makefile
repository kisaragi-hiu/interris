.cask: Cask
	cask install

ELC := $(patsubst %.el,%.elc,$(wildcard *.el))

$(ELC): .cask $(wildcard *.el)
	cask build

compile: $(ELC)

test: $(ELC)
	cask exec buttercup tests/

.PHONY: test compile
