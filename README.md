# MELPA

[![Build Status](https://github.com/melpa/melpa/workflows/CI/badge.svg)](https://github.com/melpa/melpa/actions)

MELPA is a growing collection of `package.el`-compatible Emacs Lisp
packages built automatically on our server from the upstream source
code using simple recipes. (Think of it as a server-side version of
[el-get](https://github.com/dimitri/el-get), or even
[Homebrew](https://github.com/Homebrew/homebrew).)

Packages are updated at intervals throughout the day.

To browse available packages, check out the
[archive index page](https://melpa.org/).

Adding packages is as simple as submitting a new recipe as a pull request;
read on for details.

## Table of Contents

* [Usage](#usage)
* [Contributing](#contributing)
* [Recipe Format](#recipe-format)
* [Build Scripts](#build-scripts)
* [API](#api)
* [Mirrors](#mirrors)
* [About](#about)


## Usage

To use the MELPA repository, you'll need an Emacs with `package.el`,
ie. Emacs 24.1 or greater. To test TLS support you can visit a HTTPS
URL, for example with `M-x eww RET https://wikipedia.org RET`.

Enable installation of packages from MELPA by adding an entry to
`package-archives` after `(require 'package)` and before the call to
`package-initialize` in your `init.el` or `.emacs` file:

```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
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
in a separate package archive called [MELPA
Stable](https://stable.melpa.org). Most users should prefer MELPA over
MELPA Stable.

Some notes:

* If you leave the original MELPA server in your `package-archives`
  then by default you will get the *development* versions of packages
  and not the stable ones, because the development versions are higher.

* If your Emacs has the variables `package-pinned-packages` (available
  in 24.4 and later) and/or `package-archive-priorities`, you can
  customize or modify those variables as needed.

* You can use the
  [package-filter.el](https://github.com/milkypostman/package-filter)
  package which we provide.

* You will probably want to remove all packages and then reinstall
  them. Any packages you already have installed from MELPA will never
  get "updated" to the stable version because of the way version
  numbering is handled.

Note that the MELPA maintainers do not use MELPA Stable themselves,
and do not particularly recommend its use.

## Contributing

See the [CONTRIBUTING.org](CONTRIBUTING.org) document.

## Recipe Format

Packages are specified by files in the `recipes` directory.  You can
contribute a new package by adding a new file under `recipes` using
the following form (`[...]` denotes optional or conditional values),

```elisp
(<package-name>
 :fetcher [git|github|gitlab|hg]
 [:url "<repo url>"]
 [:repo "github-or-gitlab-user/repo-name"]
 [:commit "commit"]
 [:branch "branch"]
 [:version-regexp "<regexp>"]
 [:files ("<file1>" ...)]
 [:old-names (<old-name> ...)])
```

- `package-name`
a lisp symbol that has the same name as the package being specified.

- `:fetcher` specifies the type of repository that `:url` or `:repo`
  points to.  MELPA supports [`git`][git], [`github`][github],
  [`gitlab`][gitlab], and [`hg`][hg] (Mercurial).

- `:url`
specifies the URL of the version control repository. *required for
the `git`, and `hg` fetchers.*

- `:repo` specifies the github or gitlab repository and is of the form
  `user/repo-name`. *required for the `github` and `gitlab` fetchers*.

- `:commit`
specifies the commit of the git repo to checkout. The value
will be passed to `git reset` in a repo where `upstream` is the
original repository. Can therefore be either a SHA, if pointing at a
specific commit, or a full ref prefixed with "origin/". Only used by
the `git`-based fetchers.

- `:branch`
specifies the branch of the git repo to use. This is like `:commit`, but
it adds the "origin/" prefix automatically. This must be specified when
using a branch other than the default branch.

- `:version-regexp` is a regular expression for extracting a
  version-string from the repository tags.  The default matches
  typical version tags such as `1.0`, `R16` or `v4.3.5`, so you should
  not override it unless necessary.  For an unusual tag like
  "OTP-18.1.5", we might add `:version-regexp "[^0-9]*\\(.*\\)"` to
  strip the "OTP-" prefix.  The captured portion of the regexp must be
  parseable by Emacs' `version-to-list` function.

- `:files` optional property specifying the elisp and info files used to build the
package. Please do not override this if the default value (below) is adequate, which
it should usually be:

        ("*.el" "*.el.in" "dir"
         "*.info" "*.texi" "*.texinfo"
         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"))

    Note that elisp in subdirectories is never included by default, so
you might find it convenient to keep your package's elisp in the root
of your repository, and separate auxiliary files such as tests into
subdirectories to keep packaging simple.

    The elements of the `:files` list are glob-expanded and processed
    from left to right to make a list of paths that will be copied
    into the root of the new package, as if by using `cp -R [SRCPATHS]
    DEST`. This means a directory like "foo/bar" would become "bar" in
    the new package. To specify a destination subdirectory, use a list
    element of the form `(SUBDIR SRCPATH ...)`. Likewise, to filter
    out paths expanded earlier in the list, use `(:exclude SRCPATH
    ...)`.

    If your package requires some additional files, but is
otherwise fine with the defaults, it's recommended to use the special
element `:defaults` as the very first element of the `:files` list,
which causes the default value shown above to be prepended to the
specified file list. For example `:files (:defaults "snippets")` would
cause the "snippets" subdir to be copied in addition to the defaults.


[git]: http://git-scm.com/
[github]: https://github.com/
[gitlab]: https://gitlab.com/
[hg]: https://www.mercurial-scm.org/

- `:old-names` specifies former names of the package, if any.  The value is
  a list of symbols.


### Example: Single File Repository

[smex](https://github.com/nonsequitur/smex) is a repository that
contains two files:

* `README.markdown`
* `smex.el`

Since there is only one `.el` file, this package only needs the `:url`
and `:fetcher` specified,

```elisp
(smex :repo "nonsequitur/smex" :fetcher github)
```

### Example: Multiple Packages in one Repository

Assume we have a repository containing three libraries `mypackage.el`,
`helm-mypackage.el`, and `persp-mypackage.el`.  The latter two
libraries are optional and users who don't want to use the packages
`helm` and/or `perspective` should not be forced to install them just
so they can install `mypackage`.  These libraries should therefore be
distributed as separate packages.

The three packages have to be declared in three separate files
`recipes/mypackage`, `recipes/helm-mypackage`, and
`recipes/persp-mypackage`:

```elisp
(mypackage :repo "someuser/mypackage"
           :fetcher github
           :files ("mypackage.el"))
```

```elisp
(helm-mypackage :repo "someuser/mypackage"
                :fetcher github
                :files ("helm-mypackage.el"))
```

```elisp
(persp-mypackage :repo "someuser/mypackage"
                 :fetcher github
                 :files ("persp-mypackage.el"))
```

### Example: Multiple Files in Multiple Directories

There are special cases where creation of the package comes from many
different sub-directories in the repository and the destination
sub-directories need to be explicitly set.

Consider the `flymake-perlcritic` recipe,

```elisp
(flymake-perlcritic :repo "illusori/emacs-flymake-perlcritic"
                    :fetcher github
                    :files ("*.el" ("bin" "bin/flymake_perlcritic")))
```

which will result in a package structure of,

```
flymake-perlcritic-YYYMMDD
|-- bin
|   `-- flymake_perlcritic
|-- flymake-perlcritic-pkg.el
`-- flymake-perlcritic.el
```

Notice that specifying an entry in `:files` that is a list takes the
first element to be the destination directory.  These can be embedded
further, such as the following---hypothetical---entry for `:files`,

```elisp
("*.el" ("snippets"
         ("html-mode" "snippets/html-mode/*")
         ("python-mode" "snippets/python-mode/*")))
```

which would result in a package with `*.el` in something like,

```
package-YYYYMMDD
|-- snippets
|   |-- html-mode
|   |   |-- div
|   |   `-- html
|   `-- python-mode
|       |-- for
|       `-- main
`-- package.el
```

But a better solution, given that we probably want to copy the
*entire* `snippets` directory to the root of the package, we could
just specify that directory.  Consider the `pony-mode` recipe,

```elisp
(pony-mode
 :repo "davidmiller/pony-mode"
 :fetcher github
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

* `all` -- Builds all packages under the `recipes/` directory and compiles the `index.html` file for the [melpa] website.

* `recipes/<NAME>` -- Build individual recipe `<NAME>`. Built packages
are put in the `packages/` folder with version corresponding to the
date of the latest commit that modified at least one of the files
specified by the recipe; given according to the `%Y%m%d` format.

* `json` -- build all JSON files.

* `archive.json` -- construct the `archive.json` file that will contain a JSON object of all compiled packages.

* `recipes.json` -- construct the `recipes.json` file containing a JSON object of all packages available for building.

* `clean` -- clean everything.

* `html` -- build `index.html`.

* `clean-working` -- remove all repositories that have been checked out to the `working/` directory.

* `clean-packages` -- remove all compiled packages from the `packages` directory.

* `clean-json` -- remove all JSON files.

 Note that these scripts require an Emacs with `package.el` installed,
 such as Emacs 24. If you have an older version of Emacs, you can get a
 suitable `package.el` [here](https://git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob_plain;hb=ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09;f=lisp/emacs-lisp/package.el).

[melpa]: https://melpa.org


## API

All repository code is contained in the file
`package-build/package-build.el`.  That code is maintained in a
[separate repository](https://github.com/melpa/package-build): the version
in the MELPA repository is imported using `git subtree`.

### Functions

- `(package-build-all)` : build packages for all recipes in the
directory specified by `package-build-recipes-dir`.


- `(package-build-archive NAME)` : interactive elisp function to build
a single archive. NAME is a symbol for the package to be built.
Packages are staged in the directory specified by
`package-build-working-dir` and built packages are placed in the
directory specified by `package-build-archive-dir`. Packages are
versioned based on the most recent commit date to package files based
on commits to upstream package repository. For multi-file packages,
the file `<NAME>-pkg.el` is automatically generated and contains
*description*, *version*, and *requires* information determined by
searching `<NAME>-pkg.el`, `<NAME>.el`, and `<NAME>-pkg.el.in`, if
they exist in the repository.

### Variables

- `package-build-working-dir` : Staging area containing package
repositories and package directories being built.

- `package-build-archive-dir` : Location to store `archive-contents` and
any built packages.

- `package-build-recipes-dir` : Directory containing MELPA compatible
recipes.  See [Recipe Format](#recipe-format) section for more details.


## Configuration

Packages end up in the `packages/` directory by default.
This can be configured using the `package-build-archive-dir` variable.

Repositories are checked out to the `working/` directory by default.
This can be configured using the `package-build-working-dir` variable.

## Mirrors

Official mirrors are available (with many thanks to mirrorservice.org)
so that if melpa.org is down, packages can still be installed.  The
following are the HTTP/HTTPS URLs to use in `package-archives` for
MELPA and MELPA Stable respectively:

* [http://www.mirrorservice.org/sites/melpa.org/packages/](http://www.mirrorservice.org/sites/melpa.org/packages/)
* [https://www.mirrorservice.org/sites/melpa.org/packages/](https://www.mirrorservice.org/sites/melpa.org/packages/)
* [http://www.mirrorservice.org/sites/stable.melpa.org/packages/](http://www.mirrorservice.org/sites/stable.melpa.org/packages/)
* [https://www.mirrorservice.org/sites/stable.melpa.org/packages/](https://www.mirrorservice.org/sites/stable.melpa.org/packages/)

Only the packages are mirrored, not the web site front-end itself.

_We are NOT responsible for the contents of any UNOFFICIAL mirror of
our packages._

## About

*MELPA* is *Milkypostman's ELPA* or *Milkypostman's Experimental Lisp
 Package Archive* if you're not into the whole brevity thing.
