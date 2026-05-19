SHELL  := bash

ifdef V
  Q=
else
  Q=@
endif

.PHONY: all clean updates.rss
all: updates.rss

updates.rss: updates.rss.erb
	$(Q)erb updates.rss.erb > updates.rss.tmp && mv -f updates.rss.tmp updates.rss

livejson:
	$(Q)for f in recipes archive download_counts build-status; \
	do curl https://melpa.org/$$f.json > $$f.json; done

clean:
	$(Q)rm -v updates.rss

.dummy: livejson
