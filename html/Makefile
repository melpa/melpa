SHELL  := /bin/bash

.PHONY: all clean
all: index.html

index.md: index.erb
	erb index.erb > index.md

index.html: index.md
	pandoc --template=template.html --css=style.css -s --mathml -t html --smart index.md > index.html

clean:
	-rm -v index.md index.html
