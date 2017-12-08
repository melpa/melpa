# MELPA

[![Build Status](https://travis-ci.org/melpa/melpa.png?branch=master)](https://travis-ci.org/melpa/melpa)

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

To use the MELPA repository, you'll need an Emacs with
`package.el`. Emacs 24 has `package.el` bundled with it, and there's
also a
[version you can use with Emacs 23](http://repo.or.cz/w/emacs.git/blob_plain/ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09:/lisp/emacs-lisp/package.el).

Enable installation of packages from MELPA by adding an entry to
`package-archives` after `(require 'package)` and before the call to
`package-initialize` in your `init.el` or `.emacs` file:

```lisp
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
```

Then just use `M-x package-list-packages` to browse and install
packages from MELPA and elsewhere.

**Note:** Packages from the default “bleeding-edge” repository will
always have higher versions than those from other archives like
Marmalade, so if you decide you need non-MELPA versions of specific
packages for some reason, extra configuration will be required:

* If your Emacs has the variable `package-pinned-packages` (available
  in 24.4 and later), you can customize or modify that variable as
  needed.

* You can use the
  [package-filter.el](https://github.com/milkypostman/package-filter)
  package which we provide.

* You can use MELPA Stable.

### MELPA Stable

By default, MELPA provides the very latest revisions of packages.  If
you prefer to only receive updates for tagged releases, use
[MELPA Stable](https://stable.melpa.org) instead:

```lisp
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
```

*Versions for packages on the original MELPA server are based on the date of the last commit and will likely be higher than any version on the stable server.* Keep the following things in mind,

* If you leave the original MELPA server in your `package-archives`
  then by default you will get the *development* versions of packages
  and not the stable ones.

* You will probably want to remove all packages and then reinstall
  them. Any packages you already have installed from MELPA will never
  get "updated" to the stable version because of the way version
  numbering is handled.

Note that the MELPA maintainers do not use MELPA Stable themselves,
and do not particularly recommend its use.

## Contributing

See the [CONTRIBUTING.md](CONTRIBUTING.md) document.

## Recipe Format

Packages are specified by files in the `recipes` directory.  You can
contribute a new package by adding a new file under `recipes` using
the following form (`[...]` denotes optional or conditional values),

```lisp
(<package-name>
 :fetcher [git|github|gitlab|bitbucket|hg|wiki]
 [:url "<repo url>"]
 [:repo "github-gitlab-or-bitbucket-user/repo-name"]
 [:version-regexp "<regexp>"]
 [:files ("<file1>" ...)])
```

- `package-name`
a lisp symbol that has the same name as the package being specified.

- `:fetcher` specifies the type of repository that `:url` or `:repo`
  points to.  Melpa supports [`git`][git], [`github`][github],
  [`gitlab`][gitlab], [`hg`][hg] (Mercurial), and [`wiki`][emacswiki]
  (for packages from Emacswiki.org).

  - The `bitbucket` fetcher derives from `hg`, so you have to use
    `git` for Git repositories hosted on Bitbucket.

  - The `wiki` fetcher gets the latest version from
    `http://www.emacswiki.org/emacs/download/<NAME>.el` where `NAME`
    is the package name.  Note that the `:url` property is not needed
    for the `wiki` fetcher unless the name of the package file on the
    EmacsWiki differs from the package name being built.  This fetcher
    is deprecated due to security concerns.  Please distribute your
    packages using a `git` or `hg` repository.

  - Bazaar, CVS, Darcs, Fossil and Subversion used to be supported,
    until only a dozen packages remained on Melpa that used one of
    these five version control systems.  These packages are now
    imported from the Emacsorphanage, which features semi-regularly
    updated Git mirrors of the upstream non-Git repositories.  (The
    latest stable `package-build.el` release actually still supports
    these vcs.)

- `:url`
specifies the URL of the version control repository. *required for
the `git`, and `hg` fetchers.*

- `:repo` specifies the github/gitlab/bitbucket repository and is of the form
`user/repo-name`. *required for the `github`, `gitlab`, and `bitbucket` fetchers*.

- `:commit`
specifies the commit of the git repo to checkout. The value
will be passed to `git reset` in a repo where `upstream` is the
original repository. Can therefore be either a SHA, if pointing at a
specific commit, or a full ref prefixed with "origin/". Only used by
the `git`-based fetchers.

- `:branch`
specifies the branch of the git repo to use. This is like `:commit`, but
it adds the "origin/" prefix automatically.

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

    This option is necessary when there are multiple packages in the
repository and thus the package should only be built from a subset of
`.el` files. For example, elisp test files should not normally be
packaged. *Any file specified at any path in the repository is copied
to the root of the package.* More complex options are available,
submit an [Issue](https://github.com/melpa/melpa/issues) if the
specified package requires more complex file specification.

    If the the package merely requires some additional files, for example for
bundling external dependencies, but is otherwise fine with the defaults, it's
recommended to use `:defaults` as the very first element of this list, which
causes the default value shown above to be prepended to the specified file list.

    Note that elisp in subdirectories is never included by default, so
you might find it convenient to separate auxiliiary files such as tests into
subdirectories to keep packaging simple.

[git]: http://git-scm.com/
[github]: https://github.com/
[gitlab]: https://gitlab.com/
[bitbucket]: https://bitbucket.org/
[hg]: https://www.mercurial-scm.org/
[emacswiki]: http://www.emacswiki.org/


### Example: Single File Repository

[smex](https://github.com/nonsequitur/smex) is a repository that
contains two files:

* `README.markdown`
* `smex.el`

Since there is only one `.el` file, this package only needs the `:url`
and `:fetcher` specified,

```lisp
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

```lisp
(mypackage :repo "someuser/mypackage"
            :fetcher github
            :files ("mypackage.el"))
```

```lisp
(helm-mypackage :repo "someuser/mypackage"
                 :fetcher github
                 :files ("helm-mypackage.el"))
```

```lisp
(persp-mypackage :repo "someuser/mypackage"
                  :fetcher github
                  :files ("persp-mypackage.el"))
```

### Example: Multiple Files in Multiple Directories

There are special cases where creation of the package comes from many
different sub-directories in the repository and the destination
sub-directories need to be explicitly set.

Consider the `flymake-perlcritic` recipe,

```lisp
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

```lisp
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

```lisp
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
 suitable `package.el` [here](http://bit.ly/pkg-el23).

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
