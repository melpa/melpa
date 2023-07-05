SHELL  := bash
dyn_json := recipes.json archive.json download_counts.json build-status.json
pages  := $(notdir $(wildcard templates/pages/*.html))
packages_json := $(wildcard packages/*.json)
packages_html := $(packages_json:.json=.html)
htmldir := $(notdir $(CURDIR))

all: updates.rss pages packages

updates.rss: templates/updates.rss.erb packages.json
	@echo "$(htmldir) • Rendering $@ ..."
	@./erubi templates/updates.rss.erb > updates.rss.tmp && mv updates.rss.tmp updates.rss

# Build static pages with content in templates/pages
%.html: templates/pages/%.html templates/layout.html.erb
	@echo "$(htmldir) • Rendering $@ ..."
	@./erubi content_template=$< templates/layout.html.erb > $@.tmp && mv $@.tmp $@

# Homepage needs to be updated when the JSON files change
index.html: templates/pages/index.html templates/layout.html.erb total_downloads.json packages.json
	@echo "$(htmldir) • Rendering $@ ..."
	@./erubi content_template=$< templates/layout.html.erb > $@.tmp && mv $@.tmp $@

# One HTML file for every package with a JSON file
packages/%.html: packages/%.json templates/package.html.erb templates/layout.html.erb
	@echo "$(htmldir) • Rendering $@ ..."
	@./erubi pname=$* content_template=templates/package.html.erb templates/layout.html.erb > $@.tmp && mv $@.tmp $@

packages/%.json: packages.json

packages.json: archive.json downloads.json generate-packages-json.sh
	@echo "$(htmldir) • Generating $@ ..."
	@./generate-packages-json.sh < $< > $@.tmp && mv $@.tmp $@

total_downloads.json: download_counts.json
	@echo "$(htmldir) • Generating $@ ..."
	@jq '{ total: add }' $< > $@.tmp && mv $@.tmp $@

downloads.json: download_counts.json ./generate-download-percentile.sh
	@echo "$(htmldir) • Generating $@ ..."
	@./generate-download-percentile.sh < $< > $@.tmp && mv $@.tmp $@

pages: $(pages)
packages: $(packages_html)

$(dyn_json):
	curl -sS https://melpa.org/$@ > $@

livejson: $(dyn_json)

clean:
	-rm -v *.tmp
	-rm -v $(packages_html)

.dummy: livejson
.PHONY: all clean packages livejson
