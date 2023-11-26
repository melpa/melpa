# TODO:
#  - rss
#  - opensearch
#  - unify static files
#  - toggle stable vs. non-stable
#  - build timestamps
#  - detect old-style URLs and redirect (in JS)
#  - page titles
#  - entity-quote package URLs containing +
#  - case-insensitive search
#  - gist recipes
from flask import Flask
from flask import render_template
from flask import request
import json
import os
import datetime
from collections import namedtuple
import re
import itertools

##############################################################################
# Data models
##############################################################################

JSON_DIR="../../html"

BuildStatus = namedtuple("BuildStatus", ['started_at', 'completed_at', 'next_at', 'duration'])

def calculate_source_url(recipe, commit):
      fetcher = recipe["fetcher"]
      url = recipe.get("url")
      repo = recipe.get("repo")
      branch = recipe.get("branch")
      match fetcher:
          case 'github':
              ref = commit or branch
              extra = ref and f"/tree/{ref}" or ""
              return f"https://github.com/{repo}/{extra}"
          case "gitlab":
              ref = commit or branch
              extra = ref and f"/tree/{ref}" or ""
              return f"https://gitlab.com/{repo}/{extra}"
          case "sourcehut":
              ref = commit or branch
              extra = ref and f"/tree/{ref}" or ""
              return f"https://git.sr.ht/~{repo}/{extra}"
          case "bitbucket":
              extra = ""
              if commit: extra = "/src/" + commit
              elif branch: extra = "/branch/" + branch
              return f"https://bitbucket.com/{repo}{extra}"
          case _:
              def url_match(pat, rep=''):
                  new_url = re.sub(pat, rep, url)
                  if new_url != url: return new_url
                  else: return None
              return (url_match(r'(bitbucket\.org\/[^\/]+\/[^\/\?]+)', "https://") or
                      url_match(r'(gitorious\.org\/[^\/]+\/[^.]+)', "https://") or
                      url_match(r'(gitlab\.com\/[^\/]+\/[^.]+)', "https://") or
                      url_match(r'^lp:(.*)', "https://launchpad.net/") or
                      url_match(r'^(https?:\/\/code\.google\.com\/p\/[^\/]+\/)') or
                      url_match(r'^(https?:\/\/[^.]+\.googlecode\.com\/)') or
                      url_match(r'^https:\/\/git\.code\.sf\.net\/p\/([^\/]+)', "https://sourceforge.net/p/") or
                      url_match(r'^(https?:\/\/git\..*)'));


class PackageDescriptor:
    def __init__(self, name, entry, recipe, download_counts):
        self.name = name
        self.description = entry["desc"]
        self.version = ".".join(str(v) for v in entry["ver"])
        self.old_names = recipe.get("old-names", [])
        self.downloads =  sum(download_counts.get(p, 0) for p in (self.old_names + [name]))
        self.fetcher = recipe["fetcher"]
        self.recipe_url = f"https://github.com/melpa/melpa/blob/master/recipes/{name}"
        self.download_url = f"/packages/{name}-{self.version}." + (entry["type"] == 'single' and "el" or "tar")
        self.commit = entry.get("commit")
        self.dependencies = entry["deps"]
        self.home_url = entry["deps"]
        self._search_extra = recipe.get("repo")
        self.home_url = recipe.get("url")
        self.source_url = calculate_source_url(recipe, self.commit)
        self.readme_url = f"/packages/{name}-readme.txt"
        self.badge_url = f"/packages/{name}-badge.svg"
        self.log_url = f"/packages/{name}.log"

    def search_text(self):
        return " ".join(x for x in [self.name, self.description, self.version, self._search_extra] if x)

def maybe_timestamp(t):
    if t is not None: return datetime.datetime.fromtimestamp(t)

def maybe_duration(t):
    if t is not None: return datetime.timedelta(seconds=t)

class PackageData:
    def __init__(self):
        # Load raw data
        with open(os.path.join(JSON_DIR, "recipes.json"), 'r') as f:
            _recipes = json.load(f)
        with open(os.path.join(JSON_DIR, "archive.json"), 'r') as f:
            _archive = json.load(f)
        with open(os.path.join(JSON_DIR, "download_counts.json"), 'r') as f:
            _download_counts = json.load(f)
        with open(os.path.join(JSON_DIR, "build-status.json"), 'r') as f:
            _build_status = json.load(f)

        # Assemble package info
        self.packages = sorted([
            PackageDescriptor(name, entry=entry,
                              recipe=_recipes[name],
                              download_counts=_download_counts)
            for (name, entry) in _archive.items()
        ], key=lambda p: p.name) # Pre-sort by the default case

        self.total_downloads = sum(_download_counts.values())
        self.last_build = BuildStatus(started_at=maybe_timestamp(_build_status["started"]),
                                      completed_at=maybe_timestamp(_build_status["completed"]),
                                      next_at=maybe_timestamp(_build_status["next"]),
                                      duration=maybe_duration(_build_status["duration"]))
    def is_stale(self):
        return False

package_data = None
def load_package_data():
    global package_data
    if package_data is None or package_data.is_stale():
        package_data = PackageData()
    return package_data


##############################################################################
# Flask app
##############################################################################

app = Flask(__name__)

@app.route("/")
def index():
    data = load_package_data()
    match_count=100,
    q, =request.args.get('q', ''),
    sort, =request.args.get('sort', 'package'),
    asc, =request.args.get('asc', 'true'),

    packages = data.packages
    search_terms = [t.strip() for t in q.lower().split(' ')]
    if search_terms:
        packages = [
            p for p in data.packages if packages.
        ]

    return render_template('index.html',
                           total_packages=len(data.packages),
                           last_build=data.last_build,
                           match_count=match_count,
                           packages = packages,
                           q=q,
                           sort=sort,
                           asc=asc,
                           downloads=data.total_downloads)

@app.route("/getting-started")
def getting_started():
    return render_template('getting-started.html')
