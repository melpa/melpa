# MELPA

[![Build Status](https://github.com/melpa/melpa/actions/workflows/ci.yml/badge.svg)](https://github.com/melpa/melpa/actions/workflows/ci.yml)

MELPA is a growing collection of `package.el`-compatible Emacs Lisp
packages built automatically on our server from the upstream source
code using simple recipes. (Think of it as a server-side version of
[`el-get`], or even [Homebrew].)

Packages are updated at intervals throughout the day.

To browse available packages, check out the
[archive index page][MELPA].

Adding packages is as simple as submitting a new recipe as a pull
request; read on for details.

## Table of Contents

* [Usage](#usage)
* [Contributing](#contributing)
* [Recipe Format](#recipe-format)
* [Build Scripts](#build-scripts)
* [API](#api)
* [Configuration](#configuration)
* [Mirrors](#mirrors)
* [About](#about)

## Usage

To use the MELPA repository, you'll need an Emacs with `package.el`,
i.e., Emacs 24.1 or greater. To test TLS support you can visit a HTTPS
URL, for example with `M-x eww RET https://wikipedia.org RET`.

Enable installation of packages from MELPA by adding an entry to
`package-archives` after `(require 'package)` and before the call to
`package-initialize` in your `init.el` or `.emacs` file:

```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.
;; See `package-archive-priorities` and `package-pinned-packages`.
;; Most users will not need or want to do this.
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
```

Then just use `M-x package-list-packages` to browse and install
packages from MELPA and elsewhere.

Note that you'll need to run `M-x package-refresh-contents` or `M-x
package-list-packages` to ensure that Emacs has fetched the MELPA
package list before you can install packages with `M-x
package-install` or similar.

### MELPA Stable

Packages in MELPA are built directly from the latest package source
code in the upstream repositories, but we also build and publish
packages corresponding to the latest tagged code in those
repositories, where version tags exist. These packages are published
in a separate package archive called [MELPA Stable]. Most users should
prefer MELPA over MELPA Stable.

Some notes:

* If you leave the original MELPA server in your `package-archives`
  then by default you will get the *development* versions of packages
  and not the stable ones, because the development versions are higher.

* If your Emacs has the variables `package-pinned-packages` (available
  in 24.4 and later) and/or `package-archive-priorities`, you can
  customize or modify those variables as needed.

* You can use the [`package-filter`] package which we provide.

* You will probably want to remove all packages and then reinstall
  them. Any packages you already have installed from MELPA will never
  get "updated" to the stable version because of the way version
  numbering is handled.

Note that the MELPA maintainers do not use MELPA Stable themselves,
and do not particularly recommend its use.

## Contributing

See the [CONTRIBUTING.org] document.

## Recipe Format

Packages are specified by files in the `recipes` directory. You can
contribute a new package by adding a new file under `recipes` using
the following form (`[...]` denotes optional or conditional values),

```elisp
(<package-name>
 :fetcher [git|github|gitlab|codeberg|sourcehut|hg]
 [:url "<repo url>"]
 [:repo "user-name/repo-name"]
 [:commit "commit"]
 [:branch "branch"]
 [:version-regexp "<regexp>"]
 [:files ("<file1>" ...)]
 [:old-names (<old-name> ...)])
```

* `package-name` a lisp symbol that has the same name as the package
  being specified.

* `:fetcher` specifies the type of repository the package is being
  maintained in.

  Melpa supports the Git and Mercurial version control systems and
  provides generic fetcher types for them: `git` and `hg`. When you
  use one of these fetchers, you must specify the `:url` property.

  Melpa also provides dedicated fetchers for certain Git forges (aka
  "Git repository hosting platforms"), which should always be
  preferred over the generic `git` fetcher. When using a dedicated
  fetcher, you must specify `:repo`, not `:url`. Currently these
  Git forge fetchers exist: [`github`], [`gitlab`], [`codeberg`] and
  [`sourcehut`].

  There are no dedicated fetchers for Mercurial. When a forge
  supports both Git and Mercurial, then the respective fetcher can
  only be used for Git repositories.  For Mercurial repositories
  always use the `hg` fetcher.

* `:url` specifies the URL of the version control repository. It is
  required for the generic `git` and `hg` fetchers and is invalid for
  forge-specific fetchers.

* `:repo` specifies the repository used by forge-specific fetchers
  and is of the form `user-name/repo-name`. It is required for
  forge-specific fetchers and is invalid for the generic fetchers.

  Note that user names in Sourcehut URLs are prefixed with `~`, that
  has to be omitted in the value of this property.

* `:commit` specifies the commit of the Git repository to checkout.
  The value will be passed to `git reset` in a repo where `upstream` is
  the original repository. Can therefore be either a SHA, if pointing
  at a specific commit, or a full ref prefixed with "origin/". Only
  used by the `git`-based fetchers.

* `:branch` specifies the branch of the Git repository to use. This is
  like `:commit`, but it adds the "origin/" prefix automatically. This
  must be specified when using a branch other than the default branch.

* `:version-regexp` is a regular expression for extracting a
  version-string from the repository tags. The default matches typical
  version tags such as `1.0`, `R16` or `v4.3.5`, so you should not
  override it unless necessary. For an unusual tag like "OTP-18.1.5",
  we might add `:version-regexp "[^0-9]*\\(.*\\)"` to strip the "OTP-"
  prefix. The captured portion of the regexp must be parseable by
  Emacs' `version-to-list` function.

* `:files` optional property specifying the Emacs Lisp libraries and
  info files used to build the package. Please do not override this if
  the default value (below) is adequate, which it should usually be:

  ```elisp
  '("*.el" "lisp/*.el"
    "dir" "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
    (:exclude
     ".dir-locals.el" "lisp/.dir-locals.el"
     "test.el" "tests.el" "*-test.el" "*-tests.el"
     "lisp/test.el" "lisp/tests.el" "lisp/*-test.el" "lisp/*-tests.el"))
  ```

  Note that you should place Emacs Lisp libraries in the root of the
  repository or in the `lisp/` directory. Test files should be placed
  in the `test/` directory and they should not provide a feature.
  Likewise `NAME-pkg.el` isn't a library, so you might want to place
  it in the root directory, even when libraries reside in `lisp/`.

  Please do not track any third-party libraries and test utilities in
  your repository. If you absolutely must do it, then place these
  files in a directory dedicated to that purpose, alongside a file
  named `.nosearch`. The latter prevents various tools from adding the
  containing directory to the `load-path` or from otherwise getting
  confused.

  The elements of the `:files` list are glob-expanded to make a list
  of paths that will be copied into the root of the new package. This
  means a file like `lisp/foo.el` would become `foo.el` in the new
  package. To specify a destination subdirectory, use a list element
  of the form `(TARGET-DIR SOURCE-PATH ...)`.

  To exclude certain paths, use `(:exclude SOURCE-PATH ...)`.  There
  should only be one element that begins with `:exclude` and it should
  be the last element, though that is not enforced at this time.

  If your package requires some additional files, but is otherwise
  fine with the defaults, use the special element `:defaults` as the
  first element of the `:files` list.  This causes the default value
  shown above to be prepended to the specified file list. For example
  `:files (:defaults "snippets")` would cause the `snippets` subdir
  to be copied in addition to the defaults.

  **Warning:** Elements of `:files` are (no longer) processed in order
  because we feed these globs to `git log` or `hg log` to determine the
  last commit that touched a relevant file. These commands unfortunately
  process all exclude globs after all include globs. Therefore it is not
  possible to override the `:exclude` element that appears in `:defaults`
  in a later element of `:files`. This means that a package whose name
  ends with `-test` cannot use `:defaults`. Likewise if the name of a
  *library* (as opposed to a file implementing tests) ends with `-test.el`,
  then `:defaults` cannot be used.

  **Warning:** Once the appropriate commit has been determined
  `file-expand-wildcards` is used to determine the files matched by
  each glob. Unfortunately (unlike in a shell) a glob that begins with
  `*` may also match filenames that begin with `.`, so you might have
  to add exclude globs to prevent those from being included. `:defaults`
  takes care to exclude `.dir-locals.el`; if you don't use `:defaults`,
  then you might have to exclude that explicitly.

* `:old-names` specifies former names of the package, if any. The
  value is a list of symbols.

### Example: Single File Repository

`smex` is a repository that contains two files:

* `README.markdown`
* `smex.el`

Since there is only one `.el` file, this package only needs the
`:fetcher` and `:repo` specified,

```elisp
(smex :fetcher github :repo "nonsequitur/smex")
```

### Example: Multiple Packages in one Repository

Assume we have a repository containing three libraries `mypackage.el`,
`helm-mypackage.el`, and `persp-mypackage.el`. The latter two
libraries are optional and users who don't want to use the packages
`helm` and/or `perspective` should not be forced to install them just
so they can install `mypackage`. These libraries should therefore be
distributed as separate packages.

The three packages have to be declared in three separate files
`recipes/mypackage`, `recipes/helm-mypackage`, and
`recipes/persp-mypackage`:

```elisp
(mypackage
 :fetcher github
 :repo "someuser/mypackage"
 :files ("mypackage.el"))
```

```elisp
(helm-mypackage
 :fetcher github
 :repo "someuser/mypackage"
 :files ("helm-mypackage.el"))
```

```elisp
(persp-mypackage
 :fetcher github
 :repo "someuser/mypackage"
 :files ("persp-mypackage.el"))
```

### Example: Multiple Files in Multiple Directories

There are special cases where creation of the package comes from many
different sub-directories in the repository and the destination
sub-directories need to be explicitly set.

Consider the `flymake-perlcritic` recipe,

```elisp
(flymake-perlcritic
 :fetcher github
 :repo "illusori/emacs-flymake-perlcritic"
 :files ("*.el" ("bin" "bin/flymake_perlcritic")))
```

which will result in a package structure of,

```
flymake-perlcritic-YYYYMMDD
|-- bin
|   `-- flymake_perlcritic
|-- flymake-perlcritic-pkg.el
`-- flymake-perlcritic.el
```

Notice that specifying an entry in `:files` that is a list takes the
first element to be the destination directory.

But a better solution, given that we probably want to copy the
*entire* `snippets` directory to the root of the package, we could
just specify that directory. Consider the `pony-mode` recipe,

```elisp
(pony-mode
 :fetcher github
 :repo "davidmiller/pony-mode"
 :files ("src/*.el" "snippets"))
```

which generates the package,

```
pony-mode-YYYYMMDD
|-- pony-mode-pkg.el
|-- pony-mode.el
|-- pony-tpl.el
`-- snippets
    |-- html-mode
    |   |-- bl
    |   |-- ex
    |   |-- for
    |   |-- if
    |   |-- loa
    |   |-- sup
    |   |-- testc
    |   `-- {{
    `-- python-mode
        |-- auth-view
        |-- bn
        |-- model
        |-- modelform
        |-- render-to
        |-- testc
        `-- view
```

## Build Scripts

Building MELPA is all based around using the `Makefile` included in
the root repository directory. Described below are the actions that
accepted by the `Makefile`.

* `all` — build all packages under the `recipes/` directory and
  compiles the `index.html` file for the [MELPA] website.

* `recipes/<NAME>` — build individual recipe `<NAME>`. Built packages
  are put in the `packages/` folder with version corresponding to the
  date of the latest commit that modified at least one of the files
  specified by the recipe; given according to the `%Y%m%d` format.

* `json` — build all JSON files.

* `archive.json` — construct the `archive.json` file that will
  contain a JSON object of all compiled packages.

* `recipes.json` — construct the `recipes.json` file containing a
  JSON object of all packages available for building.

* `clean` — clean everything.

* `html` — build `index.html`.

* `clean-working` — remove all repositories that have been checked
  out to the `working/` directory.

* `clean-packages` — remove all compiled packages from the `packages`
  directory.

* `clean-json` — remove all JSON files.

Note that these scripts require an Emacs with `package.el` installed,
such as Emacs 24. If you have an older version of Emacs, you can get a
suitable `package.el` [here][package-old].

## API

All repository code is contained in the file
`package-build/package-build.el`. That code is maintained in a
[separate repository][`package-build`]: the version in the MELPA
repository is imported using `git subtree`.

### Functions

* `(package-build-all)` — build packages for all recipes in the
  directory specified by `package-build-recipes-dir`.

* `(package-build-archive NAME)` — interactive Emacs Lisp function
  to build a single archive. NAME is a symbol for the package to
  be built. Packages are staged in the directory specified by
  `package-build-working-dir` and built packages are placed in the
  directory specified by `package-build-archive-dir`. Packages are
  versioned based on the most recent commit date to package files
  based on commits to upstream package repository. For multi-file
  packages, the file `<NAME>-pkg.el` is automatically generated and
  contains *description*, *version*, and *requires* information
  determined by searching `<NAME>-pkg.el`, `<NAME>.el`, and
  `<NAME>-pkg.el.in`, if they exist in the repository.

### Variables

* `package-build-working-dir` — Staging area containing package
  repositories and package directories being built.

* `package-build-archive-dir` — Location to store `archive-contents`
  and any built packages.

* `package-build-recipes-dir` — Directory containing MELPA compatible
  recipes. See [Recipe Format](#recipe-format) section for more
  details.

## Configuration

Packages end up in the `packages/` directory by default.
This can be configured using the `package-build-archive-dir` variable.

Repositories are checked out to the `working/` directory by default.
This can be configured using the `package-build-working-dir` variable.

## Mirrors

Official mirrors are available (with many thanks to mirrorservice.org)
so that if melpa.org is down, packages can still be installed. The
following are the HTTP/HTTPS URLs to use in `package-archives` for
MELPA and MELPA Stable respectively:

* http://www.mirrorservice.org/sites/melpa.org/packages/
* https://www.mirrorservice.org/sites/melpa.org/packages/
* http://www.mirrorservice.org/sites/stable.melpa.org/packages/
* https://www.mirrorservice.org/sites/stable.melpa.org/packages/

Only the packages are mirrored, not the web site front-end itself.

_We are NOT responsible for the contents of any UNOFFICIAL mirror of
our packages._

Use `rsync` to get started with your own mirror:

```
rsync -avz --delete rsync://melpa.org/packages/ snapshots/
rsync -avz --delete rsync://melpa.org/packages-stable/ releases/
```

## About

*MELPA* is *Milkypostman's ELPA* or *Milkypostman's Experimental Lisp
Package Archive* if you're not into the whole brevity thing.

[CONTRIBUTING.org]: CONTRIBUTING.org
[MELPA]:            https://melpa.org/
[MELPA Stable]:     https://stable.melpa.org/
[`package-build`]:  https://github.com/melpa/package-build/

[`el-get`]:         https://github.com/dimitri/el-get/
[Homebrew]:         https://brew.sh/
[`git`]:            https://git-scm.com/
[`github`]:         https://github.com/
[`gitlab`]:         https://gitlab.com/
[`codeberg`]:       https://codeberg.org/
[`sourcehut`]:      https://git.sr.ht/
[`hg`]:             https://www.mercurial-scm.org/
[`package-filter`]: https://github.com/milkypostman/package-filter/
[package-old]:      https://git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob_plain;hb=ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09;f=lisp/emacs-lisp/package.el
