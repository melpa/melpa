/* global window */
(function(m, document, _, moment, jQuery){
  "use strict";

  // TODO Disqus
  // TODO Show compatible emacs versions for any package
  // TODO Google Analytics http://stackoverflow.com/questions/10713708/tracking-google-analytics-page-views-with-angular-js
  // TODO D3 visualisation for deps
  // TODO Fix json encoding of versions
  // TODO Link to specific github branch
  // TODO Show recent github events on package pages where applicable
  // TODO Voting / starring

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

  //////////////////////////////////////////////////////////////////////////////
  // Models
  //////////////////////////////////////////////////////////////////////////////

  var melpa = {};

  melpa.Package = function(data) {
    ["name", "description", "version", "dependencies", "source",
     "downloads", "fetcher", "recipeURL", "packageURL", "sourceURL", "oldNames"].map(function(p) {
      this[p] = data[p];
    }.bind(this));
    this._searchText = _([data.name, data.description, data.source, data.sourceURL])
      .compact().valueOf().join(' ').toLowerCase();
    this.readmeURL = "/packages/" + data.name + "-readme.txt";
    this.matchesTerm = function(term) {
      return this._searchText.indexOf(term) != -1;
    };
  };

  melpa.PackageList = function(packages) {
    this.packages = packages;
    this.totalDownloads = m.prop(_.reduce(_.map(packages, function(p) { return p.downloads || 0; }),
                                          function (a, b) { return b === undefined ? a : a + b; }, 0));
    this.totalPackages = m.prop(packages.length);
    var savedSearches = {};
    function preFilteredPackages(term) {
      var prefixes = _.sortBy(_.filter(_.keys(savedSearches),
                                       function(k) { return term.indexOf(k) === 0; }),
                              'length').reverse();
      return prefixes.length > 0 ? savedSearches[prefixes[0]] : null;
    }
    this.filteredPackages = function(terms, sortBy, sortAscending) {
      var t = terms.trim().toLowerCase();
      var packages = savedSearches[t];
      if (!packages) {
        var preFiltered = preFilteredPackages(t);
        packages = _.filter(preFilteredPackages(t) || this.packages,
                            function(p) { return p.matchesTerm(t); });
        if (preFiltered) packages.sortKey = preFiltered.sortKey;
      }
      var sortKey = sortBy + "-" + sortAscending;
      if (packages.sortKey === sortKey) return packages;
      if (packages.sortKey === sortBy + "-" + !sortAscending) {
        packages = packages.reverse();
      } else {
        var matched = _.sortBy(packages, function(p) { return p[sortBy]; });
        packages = savedSearches[t] = sortAscending ? matched : matched.reverse();
      }
      packages.sortKey = sortKey;
      return packages;
    };
    var packagesByName = {};
    _.each(packages, function(p) {
      packagesByName[p.name] = p;
      if(p.oldNames) {
        _.each(p.oldNames, function(n) { packagesByName[n] = p; });
      }
    });
    this.packageWithName = function(name) {
      return packagesByName[name];
    };

    var downloadCounts = _.pluck(packages, 'downloads');
    this.downloadsPercentileForPackage = function(p) {
      return _.filter(downloadCounts, function(d) { return d < p.downloads; }).length * 100.0 / downloadCounts.length;
    };

    this.dependenciesOnPackageName = function(packageName) {
      return (_.filter(packages, function(p) {
        return _.findWhere(p.dependencies, {name: packageName});
      }));
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

    var calculateSourceURL = function(name, recipe) {
      if (recipe.fetcher == "github") {
        return (/\//.test(recipe.repo) ? "https://github.com/" : "https://gist.github.com/") + recipe.repo;
      } else if (recipe.fetcher == "wiki" && !recipe.files) {
        return "http://www.emacswiki.org/emacs/" + name + ".el";
      } else if (recipe.url) {
        var urlMatch = function(re, prefix) {
          var m = recipe.url.match(re);
          return m !== null ? prefix + m[0] : null;
        };
        return (urlMatch(/(bitbucket\.org\/[^\/]+\/[^\/\?]+)/, "https://") ||
                urlMatch(/(gitorious\.org\/[^\/]+\/[^.]+)/, "https://") ||
                urlMatch(/\Alp:(.*)/, "https://launchpad.net/") ||
                urlMatch(/\A(https?:\/\/code\.google\.com\/p\/[^\/]+\/)/) ||
                urlMatch(/\A(https?:\/\/[^.]+\.googlecode\.com\/)/));
      }
      return null;
    };

    var listed = _.intersection(_.keys(archive), _.keys(recipes));
    return new melpa.PackageList(_(listed).reduce(function(pkgs, name) {
      var built = archive[name];
      var recipe = recipes[name];
      var version = built.ver.join(".");
      var deps = _.map(built.deps || [], function (ver, name) {
        return {name: name, version: ver.join('.')};
      });
      var oldNames = recipe['old-names'] || [];

      pkgs.push(new melpa.Package({
        name: name,
        version: version,
        dependencies: deps,
        description: built.desc.replace(/\s*\[((?:source: )?\w+)\]$/, ""),
        source: recipe.fetcher,
        downloads: _.reduce(oldNames.concat(name), function(sum, n) { return sum + (downloads[n] || 0); }, 0),
        fetcher: recipe.fetcher,
        recipeURL: "https://github.com/milkypostman/melpa/blob/master/recipes/" + name,
        packageURL: "packages/" + name + "-" + version + "." + (built.type == "single" ? "el" : "tar"),
        sourceURL: calculateSourceURL(name, recipe),
        oldNames: oldNames
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

  //////////////////////////////////////////////////////////////////////////////
  // Package list
  //////////////////////////////////////////////////////////////////////////////

  melpa.packagelist = {};
  melpa.packagelist.controller = function() {
    this.filterTerms = m.prop(m.route.param('q') || '');
    this.sortBy = m.prop("name");
    this.sortAscending = m.prop(true);
    this.packageList = m.prop();
    melpa.packageList.then(this.packageList);
    this.filteredPackages = function() {
      return this.packageList().filteredPackages(this.filterTerms(), this.sortBy(), this.sortAscending());
    };
    this.toggleSort = function(field) {
      if (this.sortBy() == field) {
        this.sortAscending(!this.sortAscending());
      } else {
        this.sortAscending(true);
        this.sortBy(field);
      }
    };
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
        ctrl.packageList().totalPackages(),
        " Packages ",
        m("small", [
          ctrl.packageList().totalDownloads(),
          " downloads to date"
        ])
      ]),
      m("p", [
        m("input.form-control", {type: "search", placeholder: "Enter filter terms", autofocus: true,
                                 value: ctrl.filterTerms(), onkeyup: m.withAttr("value", ctrl.filterTerms)}),
        " ",
        m("span.help-block", ["Showing ", ctrl.filteredPackages().length, " matching package(s)"])
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
          ctrl.filteredPackages().map(function(p) {
            return m("tr", [
              m("td", [
                m("a", {href: "/" + p.name, config: m.route}, [
                  p.name
                ])
              ]),
              m("td", [
                m("a", {href: "/" + p.name, config: m.route}, [
                  p.description
                ])
              ]),
              m("td.version", [
                m("a", {href: p.packageURL}, [
                  p.version,
                  " ",
                  glyphicon('download')
                ])
              ]),
              m("td.recipe", [
                m("a", {href: p.recipeURL}, [
                  glyphicon('cutlery')
                ])
              ]),
              m("td.source", [
                p.sourceURL ? m("a", {href: p.sourceURL}, [p.source]) : p.source
              ]),
              m("td", [p.downloads])
            ]);
          }))
      ])
    ]);
  };

  //////////////////////////////////////////////////////////////////////////////
  // Package details
  //////////////////////////////////////////////////////////////////////////////

  melpa.packagedetails = {};
  melpa.packagedetails.controller = function() {
    var packageName = m.route.param("package");
    this.package = m.prop();
    this.readme = m.prop('No description available.');
    this.neededBy = m.prop([]);
    this.downloadsPercentile = m.prop(0);

    melpa.packageList.then(function(packageList) {
      var p = packageList.packageWithName(packageName);
      if (!p) return;
      this.package(p);
      this.downloadsPercentile(packageList.downloadsPercentileForPackage(p));
      this.neededBy(packageList.dependenciesOnPackageName(packageName));
      this.packageWithName = packageList.packageWithName;
      m.request({method: "GET",
                 url: p.readmeURL,
                 deserialize: function(v){return v;}
                }).then(this.readme);
    }.bind(this));
  };

  melpa.packagedetails.view = function(ctrl) {
    var pkg = ctrl.package();
    if (!pkg) return m("h1", "Package not found");
    this.depLink = function(dep) {
      var depPkg = ctrl.packageWithName(dep.name);
      var label = dep.name + " " + dep.version;
      return depPkg ? m("a", {href: "/" + dep.name, config: m.route}, label) : label;
    };
    this.packageLink = function(pkg) {
      return m("a", {href: "/" + pkg.name, config: m.route}, pkg.name);
    };
    return m("section", [
      m("h1", [
        pkg.name,
        " ",
        m("small", pkg.version)
      ]),
      m("p.lead", pkg.description),
      m("p", [
        m("a.btn.btn-default", {href: pkg.recipeURL}, [glyphicon('cutlery'), " Recipe"]), ' ',
        m("a.btn.btn-default", {href: pkg.packageURL}, [glyphicon('download'), " Download"]), ' ',
        (pkg.sourceURL ? m("a.btn.btn-default", {href: pkg.sourceURL}, [glyphicon('home'), " Homepage"]) : '')
      ]),
      m("section", [
        m(".well", [
          m("dl.dl-horizontal", _.flatten([
            m("dt", "Downloads"),
            m("dd", [
              pkg.downloads,
              m("span.muted", " (all versions)"),
              ", percentile: ",
              ctrl.downloadsPercentile().toFixed(2)
            ]),
            m("dt", "Source"),
            m("dd", [
              pkg.sourceURL ? m("a", {href: pkg.sourceURL}, pkg.source) : m("span", pkg.source)
            ]),
            m("dt", "Dependencies"),
            m("dd", intersperse(pkg.dependencies.map(this.depLink), " / ")),
            m("dt", "Needed by"),
            m("dd", intersperse(ctrl.neededBy().map(this.packageLink), " / ")),
            pkg.oldNames.length > 0 ? _.flatten([
              m("dt", "Renamed from:"),
              pkg.oldNames
              // m("dt", "Old name needed by:"),
              // m("dd", "TODO")
            ]) : []
          ]))
        ])
      ]),
      m("section", [
        m("h4", "Description"),
        m("pre", ctrl.readme())
      ])
    ]);
  };


  //////////////////////////////////////////////////////////////////////////////
  // Showing last build time
  //////////////////////////////////////////////////////////////////////////////

  melpa.buildstatus = {};
  melpa.buildstatus.controller = function() {
    this.buildCompletionTime = m.request({method: 'GET', url: "/build-status.json"})
      .then(function(status){
        return new Date(status.completed * 1000);
    });
  };
  melpa.buildstatus.view = function(ctrl) {
    return m(".alert.alert-success", [
      m("strong", "Last build ended: "),
      m("span", [moment(ctrl.buildCompletionTime()).fromNow()])
    ]);
  };


  //////////////////////////////////////////////////////////////////////////////
  // Changing the appearance of the MELPA Stable page
  //////////////////////////////////////////////////////////////////////////////

  melpa.stable = m.prop(window.location.host === 'melpa-stable.milkbox.net');
  melpa.archivename = {};
  melpa.archivename.controller = function() {
    this.archiveName = function() {
      return melpa.stable() ? "MELPA Stable" : "MELPA";
    };
  };
  melpa.archivename.view = function(ctrl) {
    return m("span", ctrl.archiveName());
  };

  jQuery(window).load(function() {
    document.title = (new melpa.archivename.controller()).archiveName();
    jQuery(".archive-name").empty().each(function(i, e) {
      m.module(e, melpa.archivename);
    });
    if (melpa.stable()) {
      jQuery("html").addClass("stable");
    }
  });

  //////////////////////////////////////////////////////////////////////////////
  // Static pages
  //////////////////////////////////////////////////////////////////////////////

  melpa.staticpage = function(partialPath) {
    this.controller = function() {
    this.content = m.prop('');
      m.request({method: "GET", url: partialPath,
                 deserialize: function(v){return v;}
                }).then(this.content);
    };
    this.view = function(ctrl) {
      return m("div", [m.trust(ctrl.content())]);
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
              m("li", m.trust("<strong>Up-to-date packages built on our servers from upstream source</strong>")),
              m("li", m.trust("<strong>Installable in any recent Emacs using 'package.el'</strong> - no need to install svn/cvs/hg/bzr/git/darcs/fossil etc.")),
              m("li", m.trust("<strong>Curated</strong> - no obsolete, renamed, forked or randomly hacked packages")),
              m("li", m.trust("<strong>Comprehensive</strong> - more packages than any other archive")),
              m("li", m.trust("<strong>Automatic updates</strong> - new commits result in new packages")),
              m("li", m.trust("<strong>Extensible</strong> - contribute recipes via github, and we'll build the packages"))
            ])
          ])
        ]),
        m(".col-md-4", [
          melpa.buildstatus.view(ctrl.buildstatus),
          m.trust('<a class="twitter-timeline" data-dnt="true" data-related="milkypostman,sanityinc" href="https://twitter.com/melpa_emacs" data-widget-id="311867756586864640">Tweets by @melpa_emacs</a>')
        ])
      ]),
      melpa.packagelist.view(ctrl.packagelist)
    ]);
  };


  //////////////////////////////////////////////////////////////////////////////
  // Routing
  //////////////////////////////////////////////////////////////////////////////
  melpa.gettingstarted = new melpa.staticpage("/partials/getting-started.html");

  m.route.mode = "hash";
  m.route(document.getElementById("content"), "/", {
    "/": melpa.frontpage,
    "/getting-started": melpa.gettingstarted,
    "/:package": melpa.packagedetails
  });


  //////////////////////////////////////////////////////////////////////////////
  // Lazily initialise twitter widgets as they appear
  //////////////////////////////////////////////////////////////////////////////
  window.setInterval(function() {
    if (window.twttr && window.twttr.widgets) window.twttr.widgets.load();
  }, 100);

})(window.m, window.document, window._, window.moment, window.jQuery);
