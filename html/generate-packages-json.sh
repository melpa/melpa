#!/usr/bin/env bash
jq 'to_entries | map(.value + { name: .key }) | map({
  name,
  type,
  version: .ver,
  extension: (if .type == "single" then "el" else "tar" end),
  description: .desc|gsub("\\[source:.*$"; ""),
  props,
  deps: (.deps // {})|map_values(join(".")),
 })' |
	mlr --json --no-jlistwrap \
		join -f downloads.json -j name --ur then \
		join -f <(jq 'to_entries | map({ name: .key, recipe: .value })' recipes.json) -j name --ur then put -q '
begin {
  @rdeps = {};
  @records = [];
}
func calculateSourceURL(name, recipe, commit) {
  if (recipe.fetcher == "github") {
    if (recipe.repo =~ "/") {
      ref = commit ?? recipe.branch;
      return format("https://github.com/{}{}", recipe.repo, is_not_empty(ref) ? "/tree/" . ref : "");
    } else {
      return "https://gist.github.com/" . recipe.repo;
    }
  } elif (recipe.fetcher == "gitlab") {
    ref = commit ?? recipe.branch;
    return format("https://gitlab.com/{}{}", recipe.repo, is_not_empty(ref) ? "/tree/" . ref : "");
  } elif (recipe.fetcher == "bitbucket") {
      base = "https://bitbucket.com/" . recipe.repo;
      if (commit) { return base . "/src/" . commit; }
      if (recipe.branch) { return base . "/branch/" . recipe.branch; }
      return base;
  } elif (recipe.fetcher == "wiki") {
    return "http://www.emacswiki.org/emacs/" + name + ".el";
  } elif (is_not_empty(recipe.url)) {
    funct urlMatch = func(re, prefix) {
      if (recipe.url =~ re) {
         return prefix . "\1";
      }
    };
    return (urlMatch("(bitbucket\.org/[^/]+/[^/?]+)", "https://") ???
            urlMatch("(gitorious\.org/[^/]+/[^.]+)", "https://") ???
            urlMatch("(gitlab\.com/[^/]+/[^.]+)", "https://") ???
            urlMatch("^lp:(.*)", "https://launchpad.net/") ???
            urlMatch("^(https?://code\.google\.com/p/[^/]+/)", "") ???
            urlMatch("^(https?://[^.]+\.googlecode\.com/)", "") ???
            urlMatch("^https://git\.code\.sf\.net/p/([^/]+)", "https://sourceforge.net/p/") ???
            urlMatch("^(https?://git\..*)", ""));
  }
}
$build_time = sec2gmt(strptime(string($version[1]) . fmtnum($version[2], "%04d"), "%Y%m%d%H%M"));
if (is_error($build_time)) {
   $build_time = sec2gmt(systime());
}
$version = joinv($version, ".");
$source = calculateSourceURL($name, $recipe, $props.commit);
if (is_error($source)) {
  $source = null;
}
for (dep, _ in $deps) {
    if (!haskey(@rdeps, dep)) {
       @rdeps[dep] = [];
    }
    @rdeps[dep][length(@rdeps[dep]) + 1] = $name;
}
@records[NR] = $*;
end {
    for (r in @records) {
        if (haskey(@rdeps, r.name)) {
           r.rdeps = @rdeps[r.name];
        }
        emit r;
        emit > "packages/" . (r.name) . ".json", r;
    }
}
' |
	mlr --json --jlistwrap cat
