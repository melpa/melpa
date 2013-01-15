SHELL  := /bin/bash

.PHONY: all clean index.md updates.rss
all: index.html updates.rss

index.md: index.erb
	erb index.erb > index.md

index.html: index.md
	pandoc --template=template.html --css=style.css -s --mathml -t html --smart index.md > index.html

updates.rss: updates.rss.erb
	erb updates.rss.erb > updates.rss

clean:
	-rm -v index.md index.html updates.rss
