SHELL  := /bin/bash

.PHONY: all clean updates.rss index.html
all: index.html updates.rss

index.html: index.html.erb
	erb index.html.erb > index.html

updates.rss: updates.rss.erb
	erb updates.rss.erb > updates.rss

clean:
	-rm -v index.html updates.rss
