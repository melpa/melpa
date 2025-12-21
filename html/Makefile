SHELL  := bash

.PHONY: all clean updates.rss social.org
all: updates.rss social.org

updates.rss: updates.rss.erb
	erb updates.rss.erb > updates.rss.tmp && mv -f updates.rss.tmp updates.rss

social.org: social.org.erb
	erb social.org.erb > social.org.tmp && mv -f social.org.tmp social.org

livejson:
	for f in recipes archive download_counts build-status; \
	do curl https://melpa.org/$$f.json > $$f.json; done

clean:
	-rm -v updates.rss social.org

.dummy: livejson
