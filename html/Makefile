SHELL  := /bin/bash

.PHONY: all clean updates.rss index.html
all: index.html updates.rss

index.html: index.html.erb
	erb index.html.erb > index.html.tmp && mv index.html.tmp index.html

updates.rss: updates.rss.erb
	erb updates.rss.erb > updates.rss.tmp && mv updates.rss.tmp updates.rss

clean:
	-rm -v index.html updates.rss
