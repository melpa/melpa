/* global window */
(function(m, document, _, moment, Cookies){
  "use strict";

  // TODO Link to other MELPA in header, e.g. from MELPA to MELPA Stable
  // TODO Show compatible emacs versions for any package
  // TODO Google Analytics
  // TODO D3 visualisation for deps
  // TODO Voting / starring
  // TODO Add header links from MELPA to MELPA Stable and vice-versa

  //////////////////////////////////////////////////////////////////////////////
  // Helpers
  //////////////////////////////////////////////////////////////////////////////

  function intersperse(seq, sep) {
    var res = seq.slice(0,1);
    for(var i=1; i < seq.length; ++i) {
      res.push(sep);
      res.push(seq[i]);
    }
    return res;
  }

  function addPropSetHook(prop, setter) {
    return function(val) {
      if (arguments.length === 0)
        return prop();
      var ret = prop(val);
      setter(val);
      return ret;
    };
  }

  //////////////////////////////////////////////////////////////////////////////
  // Models
  //////////////////////////////////////////////////////////////////////////////

  var melpa = {};
  melpa.rootURL = window.location.protocol + "//" + window.location.host;

  melpa.Package = function(data) {
    ["name", "description", "version", "dependencies", "source", "commit",
     "downloads", "fetcher", "recipeURL", "packageURL", "sourceURL", "oldNames"].map(function(p) {
      this[p] = data[p];
    }.bind(this));
    this._searchText = _([data.name, data.description, data.version].concat(data.searchExtra || []))
      .compact().valueOf().join(' ').toLowerCase();
    this.readmeURL = "/packages/" + data.name + "-readme.txt";
    this.badgeURL = "/packages/" + data.name + "-badge.svg";
    this.matchesTerm = function(term) {
      return _.every(term.split(' ').map(function(part) {
        return this._searchText.indexOf(part) !== -1;
      }.bind(this)));
    };
  };

  melpa.PackageList = function(packages) {
    this.packages = packages;
    this.totalDownloads = m.prop(packages.reduce(function (total, p) { return total + (p.downloads || 0); }, 0));
    this.totalPackages = m.prop(packages.length);
    var savedSearches = {};
    function preFilteredPackages(term) {
      var prefixes = _(savedSearches).keys().filter(function(k) { return term.indexOf(k) === 0; }).sortBy('length').valueOf().reverse();
      return prefixes.length > 0 ? savedSearches[prefixes[0]] : packages;
    }
    this.matchingPackages = function(terms) {
      var t = terms.trim().toLowerCase();
      var matching = savedSearches[t];
      if (!matching) {
        matching = savedSearches[t] = preFilteredPackages(t).filter(function(p) { return p.matchesTerm(t); });
      }
      return matching;
    };
    var packagesByName = packages.reduce(function(packagesByName, p) {
      packagesByName[p.name] = p;
      if(p.oldNames) {
        _(p.oldNames).each(function(n) { packagesByName[n] = p; });
      }
      return packagesByName;
    }, {});
    this.packageWithName = function(name) {
      return packagesByName[name];
    };

    var downloadCounts = _.pluck(packages, 'downloads');
    this.downloadsPercentileForPackage = function(p) {
      return downloadCounts.filter(function(d) { return d < p.downloads; }).length * 100.0 / downloadCounts.length;
    };

    this.dependenciesOnPackageName = function(packageName) {
      return packages.filter(function(p) {
        return _.findWhere(p.dependencies, {name: packageName});
      });
    };
  };

  //////////////////////////////////////////////////////////////////////////////
  // Gather remote info about packages
  //////////////////////////////////////////////////////////////////////////////

  melpa.packageList = m.sync([
    m.request({method: 'GET', url: "/recipes.json"}),
    m.request({method: 'GET', url: "/archive.json"}),
    m.request({method: 'GET', url: "/download_counts.json"})
  ]).then(function (info) {
    var recipes = info[0], archive = info[1], downloads = info[2];

    var calculateSourceURL = function(name, recipe, commit) {
      var base, ref;
      if (recipe.fetcher == "github") {
        if (recipe.repo.indexOf("/") != -1) {
          ref = commit || recipe.branch;
          return "https://github.com/" + recipe.repo +
            (ref ? "/tree/" + ref : "");
        } else {
          return "https://gist.github.com/" + recipe.repo;
        }
      } else if (recipe.fetcher == "gitlab") {
        base = "https://gitlab.com/" + recipe.repo;
        ref = commit || recipe.branch;
        return "https://gitlab.com/" + recipe.repo +
          (ref ? "/tree/" + ref : "");
      } else if (recipe.fetcher == "bitbucket") {
        base = "https://bitbucket.com/" + recipe.repo;
        if (commit) return base + "/src/" + commit;
        if (recipe.branch) return base + "/branch/" + recipe.branch;
        return base;
      } else if (recipe.fetcher == "wiki") {
        return "http://www.emacswiki.org/emacs/" + name + ".el";
      } else if (recipe.url) {
        var urlMatch = function(re, prefix) {
          var m = recipe.url.match(re);
          return m !== null ? (prefix || '') + m[0] : null;
        };
        return (urlMatch(/(bitbucket\.org\/[^\/]+\/[^\/\?]+)/, "https://") ||
                urlMatch(/(gitorious\.org\/[^\/]+\/[^.]+)/, "https://") ||
                urlMatch(/(gitlab\.com\/[^\/]+\/[^.]+)/, "https://") ||
                urlMatch(/^lp:(.*)/, "https://launchpad.net/") ||
                urlMatch(/^(https?:\/\/code\.google\.com\/p\/[^\/]+\/)/) ||
                urlMatch(/^(https?:\/\/[^.]+\.googlecode\.com\/)/));
      }
      return null;
    };

    var listed = _.intersection(_.keys(archive), _.keys(recipes));
    return new melpa.PackageList(_(listed).reduce(function(pkgs, name) {
      var built = archive[name];
      var props = built.props || {};
      var recipe = recipes[name];
      var version = built.ver.join(".");
      var deps = _.map(built.deps || [], function (ver, name) {
        return {name: name, version: ver.join('.')};
      });
      var oldNames = recipe['old-names'] || [];
      var commit = props.commit;

      pkgs.push(new melpa.Package({
        name: name,
        version: version,
        commit: commit,
        dependencies: deps,
        description: built.desc.replace(/\s*\[((?:source: )?\w+)\]$/, ""),
        source: recipe.fetcher,
        downloads: oldNames.concat(name).reduce(function(sum, n) { return sum + (downloads[n] || 0); }, 0),
        fetcher: recipe.fetcher,
        recipeURL: "https://github.com/melpa/melpa/blob/master/recipes/" + name,
        packageURL: "packages/" + name + "-" + version + "." + (built.type == "single" ? "el" : "tar"),
        sourceURL: calculateSourceURL(name, recipe, commit),
        oldNames: oldNames,
        searchExtra: [recipe.repo]
      }));
      return pkgs;
    }, []));
  });

  //////////////////////////////////////////////////////////////////////////////
  // View helpers
  //////////////////////////////////////////////////////////////////////////////

  function glyphicon(name) {
    return m("span.glyphicon.glyphicon-" + name);
  }

  function packageLink(pkg, contents) {
    return m("a", {href: "/" + encodeURIComponent(pkg.name), config: m.route},
             contents || pkg.name);
  }

  function packagePath(pkg) {
    if (m.route.mode !== "hash") throw "FIXME: unsupported route mode";
    return "/#/" + encodeURIComponent(pkg.name);
  }

  //////////////////////////////////////////////////////////////////////////////
  // Pagination
  //////////////////////////////////////////////////////////////////////////////

  melpa.paginator = {};
  melpa.paginator.controller = function(getItemList) {
    this.pageLength = m.prop(50);
    this.windowSize = m.prop(7);
    this.pageNumber = m.prop(1);
    this.paginatedItems = function() {
      if (this.pageNumber() !== null) {
        return getItemList().slice(this.pageLength() * (this.pageNumber() - 1),
                                  this.pageLength() * this.pageNumber());
      } else {
        return getItemList();
      }
    };
    this.maxPage = function() {
      return Math.floor(getItemList().length / this.pageLength());
    };
    this.prevPages = function() {
      return _.last(_.range(1, this.pageNumber()),
                    Math.floor((this.windowSize() - 1) / 2));
    };
    this.nextPages = function() {
      return _.first(_.range(this.pageNumber() + 1, 1 + this.maxPage()),
                     this.windowSize() - 1 - this.prevPages().length);
    };
  };

  melpa.paginator.view = function(ctrl) {
    var prevPage = _.last(ctrl.prevPages());
    var nextPage = _.first(ctrl.nextPages());
    var pageLinkAttrs = function(n) {
      return n ? { onclick: function(){ ctrl.pageNumber(n); } } : {};
    };
    var pageLink = function(n) {
      return m("li", m("a", pageLinkAttrs(n), m("span", n)));
    };
    return m("nav",
             m("ul.pagination", [
               m("li", { class: (prevPage ? "" : "disabled") },
                 m("a", pageLinkAttrs(prevPage), [
                   m("span", {"aria-hidden": "true"}, m.trust("&laquo;")),
                   m("span.sr-only", "Previous")
                 ])),
               ctrl.prevPages().map(pageLink),
               m("li.active", m("a", m("span", [ctrl.pageNumber(), " ", m("span.sr-only", "(current)")]))),
               ctrl.nextPages().map(pageLink),
               m("li", { class: (nextPage ? "" : "disabled") },
                 m("a", pageLinkAttrs(nextPage), [
                   m("span", {"aria-hidden": "true"}, m.trust("&raquo;")),
                   m("span.sr-only", "Next")
                 ]))
             ]));
  };

  //////////////////////////////////////////////////////////////////////////////
  // Package list
  //////////////////////////////////////////////////////////////////////////////

  melpa.packagelist = {};
  melpa.packagelist.controller = function() {
    var defaultQueryParams = {q: '', sort: 'name', asc: true, page: 1};
    var queryParams = {
      q: m.route.param('q') || defaultQueryParams.q,
      sort: m.route.param('sort') || defaultQueryParams.sort,
      asc: m.route.param('asc') ? (m.route.param('asc') == 'true') :  defaultQueryParams.asc
    };
    var resetPagination = function() { this.paginatorCtrl.pageNumber(1); }.bind(this);
    var updateRoute = function() {
      queryParams = {
        q: this.filterTerms(),
        sort: this.sortBy(),
        asc: this.sortAscending()
      };
      var parts = [];
      for (var k in queryParams) {
        if (queryParams[k] !== defaultQueryParams[k]) {
          parts.push(k + "=" + encodeURIComponent(queryParams[k]));
        }
      }
      history.replaceState({},"", "/#/" + (parts.length > 0 ? "?" + parts.join("&") : ""));
    }.bind(this);

    this.filterTerms = addPropSetHook(addPropSetHook(m.prop(queryParams.q), resetPagination), updateRoute);
    this.sortBy = addPropSetHook(m.prop(queryParams.sort), updateRoute);
    this.sortAscending = addPropSetHook(m.prop(queryParams.asc), updateRoute);
    this.packageList = melpa.packageList;
    this.matchingPackages = function() {
      return this.packageList().matchingPackages(this.filterTerms());
    };
    this.sortedPackages = function() {
      var pkgs = _.sortBy(this.matchingPackages(), this.sortBy());
      if (!this.sortAscending())
        pkgs = pkgs.reverse();
      return pkgs;
    }.bind(this);
    this.toggleSort = function(field) {
      if (this.sortBy() == field) {
        this.sortAscending(!this.sortAscending());
      } else {
        this.sortAscending(true);
        this.sortBy(field);
      }
      resetPagination();
    };
    this.wantPagination = function() {
      return !Cookies.get("nopagination");
    };
    this.togglePagination = function() {
      if (this.wantPagination()) {
        Cookies.set("nopagination", "1");
      } else {
        Cookies.expire("nopagination");
      }
    };
    this.paginatorCtrl = new melpa.paginator.controller(this.sortedPackages);
  };

  melpa.packagelist.view = function(ctrl) {
    var sortToggler = function(field) {
      return function() { return ctrl.toggleSort(field); };
    };
    var sortIndicator = function(field) {
      return glyphicon((field != ctrl.sortBy()) ? "minus" : (ctrl.sortAscending() ? "chevron-down" : "chevron-up"));
    };
    return m("section#packages", [
      m("h2", [
        "Current List of ",
        ctrl.packageList().totalPackages().toLocaleString(),
        " Packages ",
        m("small", [
          ctrl.packageList().totalDownloads().toLocaleString(),
          " downloads to date"
        ])
      ]),
      m("p", [
        m("input.form-control[type=search]", {
          placeholder: "Enter filter terms", autofocus: true,
          value: ctrl.filterTerms(), onkeyup: m.withAttr("value", ctrl.filterTerms)
        }),
        " ",
        m("span.help-block", [ctrl.matchingPackages().length, " matching package(s)"])
      ]),
      m("table#package-list.table.table-bordered.table-responsive.table-hover", [
        m("thead", [
          m("tr", [
            m("th.sortable", {onclick: sortToggler("name")}, ["Package", sortIndicator("name")]),
            m("th.sortable", {onclick: sortToggler("description")}, ["Description", sortIndicator("description")]),
            m("th.sortable", {onclick: sortToggler("version")}, ["Version", sortIndicator("version")]),
            m("th", "Recipe"),
            m("th.sortable", {onclick: sortToggler("fetcher")}, ["Source", sortIndicator("fetcher")]),
            m("th.sortable", {onclick: sortToggler("downloads")}, ["DLs", sortIndicator("downloads")]),
          ])
        ]),
        m("tbody",
          (ctrl.wantPagination() ? ctrl.paginatorCtrl.paginatedItems() : ctrl.sortedPackages()).map(function(p) {
            return m("tr", { key: p.name }, [
              m("td", packageLink(p)),
              m("td", packageLink(p, p.description)),
              m("td.version", m("a", {href: p.packageURL}, [p.version, " ", glyphicon('download')])),
              m("td.recipe",
                m("a", {href: p.recipeURL}, glyphicon('cutlery'))),
              m("td.source",
                p.sourceURL ? m("a", {href: p.sourceURL}, p.source) : p.source),
              m("td", [p.downloads.toLocaleString()])
            ]);
          }))
      ]),
      (ctrl.wantPagination() ? melpa.paginator.view(ctrl.paginatorCtrl) : null),
      m("small",
        m("a", {onclick: ctrl.togglePagination.bind(ctrl)},
          (ctrl.wantPagination() ? "Disable pagination (may slow down display)" : "Enable pagination")
         ))
    ]);
  };

  //////////////////////////////////////////////////////////////////////////////
  // Package details
  //////////////////////////////////////////////////////////////////////////////

  melpa.packagedetails = {};
  melpa.packagedetails.controller = function() {
    var ctrl = {
      packageName: m.route.param("package"),
      package: m.prop(),
      readme: m.prop('No description available.'),
      neededBy: m.prop([]),
      downloadsPercentile: m.prop(0),
      archivename: new melpa.archivename.controller()
    };
    ctrl.title = ctrl.packageName;
    melpa.packageList.then(function(packageList) {
      var p = packageList.packageWithName(ctrl.packageName);
      if (!p) return;
      ctrl.package(p);
      ctrl.downloadsPercentile(packageList.downloadsPercentileForPackage(p));
      ctrl.neededBy(_.sortBy(packageList.dependenciesOnPackageName(ctrl.packageName), 'name'));
      ctrl.packageWithName = packageList.packageWithName;
      m.request({method: "GET",
                 url: p.readmeURL,
                 deserialize: _.identity
                }).then(ctrl.readme);
    });
    return ctrl;
  };

  melpa.packagedetails.view = function(ctrl) {
    var pkg = ctrl.package();
    if (!pkg) return m("h1", ["Package not found: ", ctrl.packageName]);
    this.depLink = function(dep) {
      var depPkg = ctrl.packageWithName(dep.name);
      var label = dep.name + " " + dep.version;
      return depPkg ? packageLink(depPkg, label) : label;
    };
    this.reverseDepLink = function(dep) {
      var depPkg = ctrl.packageWithName(dep.name);
      return depPkg ? packageLink(depPkg, dep.name) : dep.name;
    };
    var badgeURL = melpa.rootURL + pkg.badgeURL;
    var fullURL = melpa.rootURL + packagePath(pkg);

    return m("section", [
      m("h1", [pkg.name, " ", m("small", pkg.version)]),
      m("p.lead", pkg.description),
      m("p", [
        m("a.btn.btn-default", {href: pkg.recipeURL}, [glyphicon('cutlery'), " Recipe"]), ' ',
        m("a.btn.btn-default", {href: pkg.packageURL}, [glyphicon('download'), " Download"]), ' ',
        (pkg.sourceURL ? m("a.btn.btn-default", {href: pkg.sourceURL}, [glyphicon('home'), " Homepage"]) : '')
      ]),
      m("section", [
        m(".well", [
          m("dl.dl-horizontal", [
            m("dt", "Downloads"),
            m("dd", [
              pkg.downloads.toLocaleString(),
              m("span.muted", " (all versions)"),
              ", percentile: ",
              ctrl.downloadsPercentile().toFixed(2)
            ]),
            m("dt", "Source"),
            m("dd", [
              pkg.sourceURL ? m("a", {href: pkg.sourceURL}, pkg.source) : pkg.source,
              pkg.commit ? m("span.muted", " (commit " + pkg.commit.substring(0,6) + ")") : []
            ]),
            m("dt", "Dependencies"),
            m("dd", intersperse(_.sortBy(pkg.dependencies, 'name').map(this.depLink), " / ")),
            m("dt", "Needed by"),
            m("dd", intersperse(ctrl.neededBy().map(this.reverseDepLink), " / ")),
            pkg.oldNames.length > 0 ? [
              m("dt", "Renamed from:"),
              m("dd", intersperse(pkg.oldNames, ', '))
            ] : []
          ])
        ])
      ]),
      m("section", [
        m("h4", "Description"),
        m("pre", ctrl.readme())
      ]),
      m("section",
        m("h4", "Badge code"),
        m(".well", [
          m("dl", [
            m("dt", "Preview"),
            m("dd", packageLink(pkg, m("img", {alt: ctrl.archivename.archiveName(), src: melpa.rootURL + pkg.badgeURL})))
          ]),
          m("dl", [
            m("dt", "HTML"),
            m("dd", m("pre", '<a href="' + fullURL + '"><img alt="' + ctrl.archivename.archiveName() + '" src="' + badgeURL + '"/></a>')),
            m("dt", "Markdown"),
            m("dd", m("pre", "[![" + ctrl.archivename.archiveName() + "](" + badgeURL +  ")](" + fullURL + ")")),
            m("dt", "Org"),
            m("dd", m("pre", '[[' + fullURL + '][file:' + badgeURL + ']]'))
          ])
        ]))
    ]);
  };


  //////////////////////////////////////////////////////////////////////////////
  // Showing last build time
  //////////////////////////////////////////////////////////////////////////////

  melpa.buildstatus = {};
  melpa.buildstatus.controller = function() {
    this.started = m.prop();
    this.completed = m.prop();
    this.next = m.prop();
    this.duration = m.prop();
    this.running = function() { return !this.completed(); }.bind(this);

    m.request({method: 'GET', url: "/build-status.json", background: true})
      .then(function(status){
        m.startComputation();
        this.started(maybeDate(status.started));
        this.completed(maybeDate(status.completed));
        this.next(maybeDate(status.next));
        this.duration(status.duration);
        m.endComputation();
      }.bind(this));
    function maybeDate(v) { return v ? new Date(v * 1000) : null; }
  };
  melpa.buildstatus.view = function(ctrl) {
    function reltime(t) {
      return t ? moment(t).fromNow() : "unknown";
    }
    function duration() {
      return ctrl.duration() ? moment.duration(ctrl.duration(), 'seconds').humanize() : "unknown";
    }
    if (ctrl.running()) {
      return m(".alert.alert-warning", [
        m("strong", "Current build started: "),
        m("span", [reltime(ctrl.started())]),
        m("span", [", last took ", duration()])
      ]);
    } else {
      return m(".alert.alert-success", [
        m("strong", "Next build: "),
        m("span", [reltime(ctrl.next())]),
        m("span", [", last ended ", reltime(ctrl.completed()),
                   " and took ", duration()])
      ]);
    }
  };


  //////////////////////////////////////////////////////////////////////////////
  // Changing the appearance of the MELPA Stable page
  //////////////////////////////////////////////////////////////////////////////

    melpa.stable = m.prop(window.location.host === 'stable.melpa.org' ||
                          window.location.host === 'stable-test.melpa.org');
  melpa.archivename = {};
  melpa.archivename.controller = function() {
    this.archiveName = function() {
      return melpa.stable() ? "MELPA Stable" : "MELPA";
    };
  };
  melpa.archivename.view = function(ctrl) {
    return m("span", ctrl.archiveName());
  };

  document.addEventListener("DOMContentLoaded", function() {
    document.title = (new melpa.archivename.controller()).archiveName();
    _.each(document.getElementsByClassName('archive-name'), function (e) {
      // jshint unused: false
      m.mount(e, melpa.archivename);
    });
    if (melpa.stable()) {
      document.getElementsByTagName("html")[0].className += " stable";
    }
  });

  //////////////////////////////////////////////////////////////////////////////
  // Static pages
  //////////////////////////////////////////////////////////////////////////////

  melpa.staticpage = function(partialPath, title) {
    return {
      controller: function() {
        this.content = m.prop('');
        this.title = title;
        m.request({method: "GET", url: partialPath,
                   deserialize: _.identity
                  }).then(this.content);
      },
      view: function(ctrl) {
        return m("div", [m.trust(ctrl.content())]);
      }
    };
  };


  //////////////////////////////////////////////////////////////////////////////
  // Front page
  //////////////////////////////////////////////////////////////////////////////

  melpa.frontpage = {};
  melpa.frontpage.controller = function() {
    this.packagelist = new melpa.packagelist.controller();
    this.buildstatus = new melpa.buildstatus.controller();
    this.archivename = new melpa.archivename.controller();
  };
  melpa.frontpage.view = function(ctrl) {
    return m("div", [
      m("section.page-header", [
        m("h1", [
          melpa.archivename.view(ctrl.archivename),
          m("small", " (Milkypostman’s Emacs Lisp Package Archive)")
        ])
      ]),
      m(".row", [
        m(".col-md-8", [
          m("section.jumbotron", [
            m("ul", [
              "<strong>Up-to-date packages built on our servers from upstream source</strong>",
              "<strong>Installable in any Emacs with 'package.el'</strong> - no local version-control tools needed",
              "<strong>Curated</strong> - no obsolete, renamed, forked or randomly hacked packages",
              "<strong>Comprehensive</strong> - more packages than any other archive",
              "<strong>Automatic updates</strong> - new commits result in new packages",
              "<strong>Extensible</strong> - contribute recipes via github, and we'll build the packages"
            ].map(function(content) { return m("li", m.trust(content)); }))
          ])
        ]),
        m(".col-md-4", [
          melpa.buildstatus.view(ctrl.buildstatus),
          m.trust('<a class="twitter-timeline" data-dnt="true" data-related="milkypostman,sanityinc" href="https://twitter.com/melpa_emacs" data-widget-id="311867756586864640">Tweets by @melpa_emacs</a>'),
          m('script', {src: "//platform.twitter.com/widgets.js", type: "text/javascript"})
        ])
      ]),
      melpa.packagelist.view(ctrl.packagelist)
    ]);
  };


  //////////////////////////////////////////////////////////////////////////////
  // Titled pages
  //////////////////////////////////////////////////////////////////////////////

  melpa.currentPageTitle = m.prop();

  melpa.titledPage = function(module) {
    return {
      controller: function() {
        var ctrl = new (Function.prototype.bind.apply(module.controller, arguments));
        var t = ctrl.title;
        melpa.currentPageTitle(typeof t === "function" ? t() : t);
        return ctrl;
      },
      view: module.view
    };
  };

  melpa.titleComponent = {
    controller: function() {
      this.archivename = new melpa.archivename.controller();
    },
    view: function(ctrl) {
      return _.compact([melpa.currentPageTitle(), ctrl.archivename.archiveName()]).join(" - ");
    }
  };

  var titleElem = document.querySelector("title");
  titleElem.textContent = "";
  m.module(titleElem, melpa.titleComponent);

  //////////////////////////////////////////////////////////////////////////////
  // Routing
  //////////////////////////////////////////////////////////////////////////////
  melpa.gettingstarted = melpa.staticpage("/partials/getting-started.html", "Getting Started");

  m.route.mode = "hash";
  m.route(document.getElementById("content"), "/", {
    "/": melpa.titledPage(melpa.frontpage),
    "/getting-started": melpa.titledPage(melpa.gettingstarted),
    "/:package": melpa.titledPage(melpa.packagedetails)
  });
})(window.m, window.document, window._, window.moment, window.Cookies);
