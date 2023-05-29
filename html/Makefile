SHELL  := bash
dyn_json := recipes.json archive.json download_counts.json build-status.json

all: updates.rss packages.json total_downloads.json downloads.json $(dyn_json)

updates.rss: updates.rss.erb packages.json
	@echo "$(htmldir) • Rendering $@ ..."
	@erb updates.rss.erb > updates.rss.tmp && mv updates.rss.tmp updates.rss

packages/%.json: packages.json

packages.json: archive.json downloads.json generate-packages-json.sh
	./generate-packages-json.sh < $< > $@.tmp && mv $@.tmp $@

total_downloads.json: download_counts.json
	jq '{ total: add }' $< > $@.tmp && mv $@.tmp $@

downloads.json: download_counts.json ./generate-download-percentile.sh
	./generate-download-percentile.sh < $< > $@.tmp && mv $@.tmp $@

$(dyn_json):
	curl -sS https://melpa.org/$@ > $@

livejson: $(dyn_json)

.PHONY: livejson
