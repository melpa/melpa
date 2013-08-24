SHELL  := /bin/bash

.PHONY: all clean updates.rss
all: updates.rss

updates.rss: updates.rss.erb
	erb updates.rss.erb > updates.rss.tmp && mv updates.rss.tmp updates.rss

clean:
	-rm -v updates.rss
