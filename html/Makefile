SHELL  := bash
dyn_json := recipes.json archive.json download_counts.json build-status.json
pages  := $(notdir $(wildcard templates/pages/*.html))
packages_json := $(wildcard packages/*.json)
packages_html := $(packages_json:.json=.html)
htmldir := $(notdir $(CURDIR))

define generate_downloads_percentile
mlr --json --infer-none --jlistwrap put -q ' \
	@records[NR] = $$*; \
	end { \
		for (k, v in @records) { \
			@records[k]["percentile"] = 100 * int(k)/NR; \
		} \
		emit @records; \
	}'
endef

define prep_packages_json
jq 'to_entries | map(.value + { name: .key }) | map({ \
  name, \
  type, \
  version: .ver, \
  extension: (if .type == "single" then "el" else "tar" end), \
  description: .desc|gsub("\\[source:.*$$"; ""), \
  props, \
  deps: (.deps // {})|map_values(join(".")), \
 })'
endef

define generate_packages_json
	mlr --json \
		join -f downloads.json -j name --ur then \
		join -f recipes_map.json -j name --ur then put --ojsonl -q ' \
begin { \
  @rdeps = {}; \
  @records = []; \
} \
func calculateSourceURL(name, recipe, commit) { \
  if (recipe.fetcher == "github") { \
    if (recipe.repo =~ "/") { \
      ref = commit ?? recipe.branch; \
      return format("https://github.com/{}{}", recipe.repo, is_not_empty(ref) ? "/tree/" . ref : ""); \
    } else { \
      return "https://gist.github.com/" . recipe.repo; \
    } \
  } elif (recipe.fetcher == "gitlab") { \
    ref = commit ?? recipe.branch; \
    return format("https://gitlab.com/{}{}", recipe.repo, is_not_empty(ref) ? "/tree/" . ref : ""); \
  } elif (recipe.fetcher == "bitbucket") { \
      base = "https://bitbucket.com/" . recipe.repo; \
      if (commit) { return base . "/src/" . commit; } \
      if (recipe.branch) { return base . "/branch/" . recipe.branch; } \
      return base; \
  } elif (recipe.fetcher == "wiki") { \
    return "http://www.emacswiki.org/emacs/" + name + ".el"; \
  } elif (is_not_empty(recipe.url)) { \
    funct urlMatch = func(re, prefix) { \
      if (recipe.url =~ re) { \
         return prefix . "\1"; \
      } \
    }; \
    return (urlMatch("(bitbucket\.org/[^/]+/[^/?]+)", "https://") ??? \
            urlMatch("(gitorious\.org/[^/]+/[^.]+)", "https://") ??? \
            urlMatch("(gitlab\.com/[^/]+/[^.]+)", "https://") ??? \
            urlMatch("^lp:(.*)", "https://launchpad.net/") ??? \
            urlMatch("^(https?://code\.google\.com/p/[^/]+/)", "") ??? \
            urlMatch("^(https?://[^.]+\.googlecode\.com/)", "") ??? \
            urlMatch("^https://git\.code\.sf\.net/p/([^/]+)", "https://sourceforge.net/p/") ??? \
            urlMatch("^(https?://git\..*)", "")); \
  } \
} \
$$build_time = sec2gmt(strptime(string($$version[1]) . fmtnum($$version[2], "%04d"), "%Y%m%d%H%M")); \
if (is_error($$build_time)) { \
   $$build_time = sec2gmt(systime()); \
} \
$$version = joinv($$version, "."); \
$$source = calculateSourceURL($$name, $$recipe, $$props.commit); \
if (is_error($$source)) { \
  $$source = null; \
} \
for (dep, _ in $$deps) { \
    if (!haskey(@rdeps, dep)) { \
       @rdeps[dep] = []; \
    } \
    @rdeps[dep][length(@rdeps[dep]) + 1] = $$name; \
} \
@records[NR] = $$*; \
end { \
    for (r in @records) { \
        if (haskey(@rdeps, r.name)) { \
           r.rdeps = @rdeps[r.name]; \
        } \
        emit r; \
        emit > "packages/" . (r.name) . ".json", r; \
    } \
}'
endef

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

packages_prepped.json: archive.json
	@echo "$(htmldir) • Generating $@ ..."
	@< $< $(prep_packages_json) > $@

recipes_map.json: recipes.json
	@echo "$(htmldir) • Generating $@ ..."
	@jq 'to_entries | map({ name: .key, recipe: .value })' < $< > $@

packages.json: packages_prepped.json downloads.json recipes_map.json
	@echo "$(htmldir) • Generating $@ ..."
	@< $< $(generate_packages_json) > $@.tmp && mv $@.tmp $@

total_downloads.json: download_counts.json
	@echo "$(htmldir) • Generating $@ ..."
	@jq '{ total: add }' $< > $@.tmp && mv $@.tmp $@

downloads_sorted.json: download_counts.json
	@echo "$(htmldir) • Generating $@ ..."
	@jq 'to_entries | map({ name: .key, downloads: .value|tonumber }) | sort_by(.downloads)' < $< > $@

downloads.json: downloads_sorted.json
	@echo "$(htmldir) • Generating $@ ..."
	$(generate_downloads_percentile) < $< > $@.tmp && mv $@.tmp $@

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

.INTERMEDIATE: downloads_sorted.json packages_prepped.json recipes_map.json
