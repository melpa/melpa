# TODO:
#  - rss
#  - opensearch
#  - unify static files
#  - toggle stable vs. non-stable
#  - build timestamps
#  - page titles
#  - entity-quote package URLs containing +
#  - case-insensitive search
#  - gist recipes
#  - escaping chars in description etc.

from fastapi import FastAPI, Request, HTTPException
from fastapi.responses import HTMLResponse, RedirectResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from starlette.datastructures import URL
import json
import os
import datetime
from collections import namedtuple
import re
from itertools import groupby
from math import ceil

##############################################################################
# Data models
##############################################################################

HTML_DIR="../../html"

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
              return f"https://github.com/{repo}{extra}"
          case "gitlab":
              ref = commit or branch
              extra = ref and f"/tree/{ref}" or ""
              return f"https://gitlab.com/{repo}{extra}"
          case "sourcehut":
              ref = commit or branch
              extra = ref and f"/tree/{ref}" or ""
              return f"https://git.sr.ht/~{repo}{extra}"
          case "bitbucket":
              extra = ""
              if commit: extra = "/src/" + commit
              elif branch: extra = "/branch/" + branch
              return f"https://bitbucket.com/{repo}{extra}"
          case _:
              if not url: return None
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

Dependency = namedtuple("Dependency", ['name', 'version'])

def version_string(parts):
    return ".".join(str(v) for v in parts)

class PackageDescriptor:
    def __init__(self, name, entry, recipe, download_counts):
        self.name = name
        self.description = entry["desc"]
        self.version_parts = entry["ver"]
        self.version = version_string(self.version_parts)
        self.old_names = recipe.get("old-names", [])
        self.downloads =  sum(download_counts.get(p, 0) for p in (self.old_names + [name]))
        self.fetcher = recipe["fetcher"]
        self.recipe_url = f"https://github.com/melpa/melpa/blob/master/recipes/{name}"
        self.download_url = f"/packages/{name}-{self.version}." + (entry["type"] == 'single' and "el" or "tar")
        props = entry.get("props", {})
        self.commit = props.get("commit")
        self.dependencies = [Dependency(name, version_string(ver)) for (name, ver) in (entry["deps"] or {}).items()]
        self._search_extra = recipe.get("repo")
        self.source_url = calculate_source_url(recipe, self.commit)
        self.home_url = props.get("url", self.source_url)
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
        with open(os.path.join(HTML_DIR, "recipes.json"), 'r') as f:
            _recipes = json.load(f)
        with open(os.path.join(HTML_DIR, "archive.json"), 'r') as f:
            _archive = json.load(f)
        with open(os.path.join(HTML_DIR, "download_counts.json"), 'r') as f:
            _download_counts = json.load(f)
        with open(os.path.join(HTML_DIR, "build-status.json"), 'r') as f:
            _build_status = json.load(f)

        # Assemble package info
        self.packages_by_name = {name: PackageDescriptor(name, entry=entry,
                              recipe=_recipes[name],
                              download_counts=_download_counts)
            for (name, entry) in _archive.items()}

        self.packages = sorted(self.packages_by_name.values(),
                               key=lambda p: p.name) # Pre-sort by the default case

        # Pre-calculate download percentiles
        by_downloads = sorted(self.packages, key=lambda p: p.downloads)
        percentile = 0
        self.download_percentiles = {}
        for (_, cohort) in groupby(by_downloads, lambda p: p.downloads):
            cohort = list(cohort)
            self.download_percentiles.update({ p.name: percentile / len(by_downloads) for p in cohort })
            percentile += len(cohort) * 100

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
# Pagination
##############################################################################
class Paginator:
    def __init__(self, items, page_size, page):
        items = list(items)
        self.page_size = page_size
        self.first_page = 1
        self.last_page = ceil(len(items) / page_size)
        self.page_numbers = range(max(page - 5, 1), min(page + 5, self.last_page))

        if page < self.first_page or page > self.last_page:
            raise IndexError("No such page")
        start = (page - 1) * page_size
        self.page_items = items[start:start+self.page_size]
        self.page_number = page

##############################################################################
# Flask app
##############################################################################

app = FastAPI()

app.mount("/static", StaticFiles(directory="static"), name="static")
app.mount("/packages", StaticFiles(directory=os.path.join(HTML_DIR, "packages")), name="packages")
templates = Jinja2Templates(directory="templates")
templates.env.globals['URL'] = URL

@app.get("/", response_class=HTMLResponse)
async def index(request: Request, q='', sort='package', asc: bool = True, page: int = 1):
    data = load_package_data()
    page = int(page)

    packages = data.packages
    search_terms = [t.strip() for t in q.lower().split(' ')]
    if search_terms:
        packages = [
            p for p in data.packages if all(p.search_text().find(term) >= 0 for term in search_terms)
        ]
    match_count = len(packages)

    match sort:
        case 'version': packages = sorted(packages, key=lambda p: p.version_parts, reverse=not asc)
        case 'fetcher': packages = sorted(packages, key=lambda p: p.fetcher, reverse=not asc)
        case 'downloads': packages = sorted(packages, key=lambda p: p.downloads, reverse=not asc)
        case 'package':
            if not asc: packages = reversed(packages)

    try:
        pages = Paginator(packages, 100, page)
    except IndexError:
        raise HTTPException(status_code=404, detail="Page not found")

    return templates.TemplateResponse("index.html", {
        "request": request,
        "total_packages": len(data.packages),
        "last_build": data.last_build,
        "match_count": match_count,
        "pages": pages,
        "q": q,
        "sort": sort,
        "asc": asc,
        "downloads": data.total_downloads
    })

@app.get("/package/{name}", response_class=HTMLResponse)
async def package(name, request: Request):
    data = load_package_data()
    package = data.packages_by_name.get(name)
    if not package:
        package = next(p for p in data.packages if name in p.old_names)
        if package: return RedirectResponse(url=f"/package/{package.name}")
        raise HTTPException(status_code=404, detail="Package not found")

    readme_text = ''
    readme_path = os.path.join(HTML_DIR, f"packages/{package.name}-readme.txt")
    if os.path.isfile(readme_path):
        with open(readme_path, 'rt') as f: readme_text = f.read()

    needed_by = [p for p in data.packages if any([d.name == package.name for d in p.dependencies])]

    return templates.TemplateResponse('package.html', {
        "request": request,
        "package": package,
        "readme_text": readme_text,
        "downloads_percentile": data.download_percentiles[package.name],
        "packages_by_name": data.packages_by_name,
        "needed_by": needed_by
    })


@app.get("/getting-started", response_class=HTMLResponse)
async def getting_started(request: Request):
    return templates.TemplateResponse('getting-started.html', { "request": request})
