# MELPA 

[![Build Status](https://travis-ci.org/milkypostman/melpa.png?branch=master)](https://travis-ci.org/milkypostman/melpa)

MELPA is a growing collection of `package.el`-compatible Emacs Lisp
packages built automatically on our server from the upstream source
code using simple recipes. (Think of it as a server-side version of
[el-get](https://github.com/dimitri/el-get), or even
[homebrew](https://github.com/mxcl/homebrew).)

Packages are updated at intervals throughout the day.

To browse available packages, check out the
[archive index page](http://melpa.org/).

Adding packages is as simple as submitting a pull request; read on for
details.

## Table of Contents

* [Usage](#usage)
* [Contributing](#contributing-new-recipes)
* [Recipe Format](#recipe-format)
* [Build Scripts](#build-scripts)
* [API](#api)
* [About](#about)
* [Stable Packages](#stable-packages)


## Usage

To use the MELPA repository, you'll need an Emacs with
`package.el`. Emacs 24 has `package.el` bundled with it, and there's
also a
[version you can use with Emacs 23](http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el).

Enable installation of packages from MELPA by adding an entry to
`package-archives` after `(require 'package)` and before the call to
`package-initialize` in your `init.el` or `.emacs` file:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
```

Then just use `M-x package-list-packages` to browse and install
packages from MELPA and elsewhere.

Note that MELPA packages will always have higher versions than those
from other archives like Marmalade, so if you decide you need
non-MELPA versions of specific packages for some reason, extra
configuration will be required:

If your Emacs has the variable `package-pinned-packages`, you can
customize or modify that variable as needed. Otherwise, use the
separate
[package-filter.el](https://github.com/milkypostman/package-filter)
package which we provide.


## Contributing New Recipes

New recipe submissions should adhere to the following guidelines,

* One pull request per recipe. You can create multiple branches and
  create a pull request for each branch.

* Upstream source must be stored in an authoritative
  [SCM](http://en.wikipedia.org/wiki/Software_configuration_management)
  repository. Emacswiki recipes are discouraged but can be accepted.

* Packages should be built from the *official* package repository.
  Forks of the official repository will not be accepted except in
  *extreme* circumstances.

* The package name should match the name of the feature provided.  See
  the `package` function for more information.

* Packages should adhere to the `package.el` format as specified by
  `(info "(elisp) Packaging")`. More information on this format is
  provided by the
  [marmalade package manual](https://web.archive.org/web/20111120220609/http://marmalade-repo.org/doc-files/package.5.html).

* Recipes should try to minimize the size of the resulting package by
  specifying only files relevant to the package. See the
  [Recipe Format](#recipe-format) section for more information on
  specifying package files.



### Expediting Recipe Reviews

Because we care about the quality of packages that are part of MELPA
we review all submissions. The following steps can help us with this
process and expedite the recipe review process,

* Include the following information in the pull request:

    * A brief summary of what the package does.

    * A direct link to the package repository.

    * Your association with the package (e.g., are you the maintainer?
      have you contributed? do you just like the package a lot?).

    * Relevant communications with the upstream package maintainer (e.g.,
      `package.el` compatibility changes that you have submitted).

* Test that the package builds properly via `make recipes/<recipe>`,
  or pressing `C-c C-c` in the recipe buffer.

* Test that the package installs properly via `package-install-file`,
  or entering "yes" when prompted after pressing `C-c C-c` in the
  recipe buffer.

* If you are *not* the original author or maintainer of the package you
  are submitting, please consider notifying the author *prior* to submitting
  and make reasonable effort to include them in the pull request process.


### Testing

Let `<NAME>` denote the name of the recipe to submit.

1. Fork the MELPA repository.
2. Add your new file under the directory specified by
`package-build-recipes-dir` (default: `recipes/` directory where
`package-build` was loaded). If you prefer, the interactive command
`package-build-create-recipe` in `package-build.el` will guide you
through this process.

3. Confirm your package builds properly by running

        make recipes/<NAME>

  (Be sure that the `emacs` on your path is at least version 23, or
  set `$EMACS_COMMAND` to the location of a suitable binary.)

  Alternatively, open the recipe in Emacs and press `C-c C-c` in the
  recipe buffer: this will also prompt you to install the
  freshly-built package.

4. Install the file you built by running `package-install-file` from
within Emacs and specifying the newly built package in the directory
specified by `package-build-archive-dir` (default: `packages/`
directory where `package-build` was loaded).

You can optionally run a sandboxed Emacs in which locally-built
packages will be available for installation along with those already
in MELPA:

```
EMACS=/path/to/emacs make sandbox
```

then `M-x package-list-packages`, install and test as
appropriate. This is a useful way to discover missing dependencies!

### Submitting

After verifying the entry works properly please open a pull request on
Github. Consider the [hub](https://github.com/defunkt/hub)
command-line utility by [defunkt](http://chriswanstrath.com/) which
helps simplify this process.


## Recipe Format

Packages are specified by files in the `recipes` directory.  You can
contribute a new package by adding a new file under `recipes` using
the following form (`[...]` denotes optional or conditional values),

```lisp
(<package-name>
 :fetcher [git|github|bzr|hg|darcs|fossil|svn|cvs|wiki]
 [:url "<repo url>"]
 [:repo "github-user/repo-name"]
 [:module "cvs-module"]
 [:files ("<file1>" ...)])
```

- `package-name`
a lisp symbol that has the same name as the package being specified.

- `:fetcher` (one of `git, github, bzr, hg, darcs, fossil, svn, cvs, wiki`)
specifies the type of repository that `:url` points to. Right now
package-build supports [git][git], [github][github],
[bazaar (bzr)][bzr], [mercurial (hg)][hg], [subversion (svn)][svn],
[cvs][cvs], [darcs][darcs], [fossil][fossil], and [Emacs Wiki (wiki)][emacswiki] as
possible mechanisms for checking out the repository.

    *package-build* uses
the corresponding application to update files before building the
package. In the case of the `github`
fetcher, use `:repo` instead of `:url`; the git URL will then be
deduced.

    The Emacs Wiki fetcher gets the latest version of the package
from `http://www.emacswiki.org/emacs/download/<NAME>.el` where `NAME`
is the package name. Note that the `:url` property is not needed for
the `wiki` engine unless the name of the package file on the EmacsWiki
differs from the package name being built.

- `:url`
specifies the URL of the version control repository. *required for
the `git`, `bzr`, `hg`, `darcs`, `fossil`, `svn` and `cvs` fetchers.*

- `:repo` specifies the github repository and is of the form
`github-user/repo-name`. *required for the `github` fetcher*.

- `:commit`
specifies the commit of the git repo to checkout. The value
will be passed to `git reset` in a repo where `upstream` is the
original repository. Can therefore be either a sha, if pointing at a
specific commit, or a full ref prefixed with "origin/". Only used by
the `git` and `github` fetchers.

- `:branch`
specifies the branch of the git repo to use. This is like `:commit`, but
it adds the "origin/" prefix automatically.

- `:module`
specifies the module of a CVS repository to check out.  Defaults to to
`package-name`.  Only used with `:fetcher cvs`, and otherwise ignored.

- `:files` optional property specifying the elisp and info files used to build the
package. Automatically populated by matching all `.el`, `.info` and `dir` files in the
root of the repository and the `doc` directory. Excludes all files in the root directory 
ending in `test.el` or `tests.el`. See the default value below,

        ("*.el" "*.el.in" "dir"
         "*.info" "*.texi" "*.texinfo"
         "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
         (:exclude ".dir-locals.el" "tests.el" "*-test.el" "*-tests.el"))

    This option is necessary when there are multiple packages in the
repository and thus the package should only be built from a subset of
`.el` files. For example, elisp test files should not normally be
packaged. *Any file specified at any path in the repository is copied
to the root of the package.* More complex options are available,
submit an [Issue](https://github.com/milkypostman/melpa/issues) if the
specified package requires more complex file specification.

[git]: http://git-scm.com/
[github]: https://github.com/
[bzr]: http://bazaar.canonical.com/en/
[hg]: http://mercurial.selenic.com/
[svn]: http://subversion.apache.org/
[cvs]: http://www.nongnu.org/cvs/
[darcs]: http://darcs.net/
[fossil]: http://www.fossil-scm.org/
[emacswiki]: http://www.emacswiki.org/


### Example: Single File Repository

[ido-ubiquitous](https://github.com/DarwinAwardWinner/ido-ubiquitous) is a repository that contains two files:

* `README.md`
* `ido-ubiquitous.el`

Since there is only one `.el` file, this package only needs the `:url` and `:fetcher` specified,

```lisp
(ido-ubiquitous
 :url "https://github.com/DarwinAwardWinner/ido-ubiquitous.git"
 :fetcher git)
```

### Example: Multiple Packages in one Repository

The
[emacs-starter-kit](https://github.com/technomancy/emacs-starter-kit)
contains the *starter-kit* package along with extra packages in the
`modules` directory; *starter-kit-bindings*, *starter-kit-lisp*, etc.

```lisp
(starter-kit
 :url "https://github.com/technomancy/emacs-starter-kit.git"
 :fetcher git)
(starter-kit-bindings
 :url "https://github.com/technomancy/emacs-starter-kit.git"
 :fetcher git
 :files ("modules/starter-kit-bindings.el"))
```

Notice that `:files` is not specified for `starter-kit` since
package-build will automatically add all `.el` files in the root
directory of the repository.  The `starter-kit-bindings` repository is
contained in the `modules/` subdirectory and thus needs the packages
files specified explicitly.


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
newest HEAD revision available; given according to the `%Y%m%d`
format.

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

[melpa]: http://melpa.org


## API

All repository code is contained in the `package-build.el`.

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

## About

*MELPA* is *Milkypostman's ELPA* or *Milkypostman's Experimental Lisp
 Package Archive* if you're not into the whole brevity thing.

## Stable Packages

MELPA now includes a mechanism to build *stable* versions of packages
given that the repositories meet the following criteria,

1. Hosted using *git* or *hg*.
2. Tag names are version strings compatible parseable by the `version-to-list`
   function, optionally prefixed with `v`, `v.` or `v-`.

To use the stable versions of packages you should use the stable server
in your `package-archives` list.

```lisp
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
```

An online list of available packages can be found at 
[http://stable.melpa.org](http://stable.melpa.org).

### Stable Version Generation

To have a stable version generated for your package simply tag the repository
using a naming compatible with `version-to-list`, optionally prefixed with `v`,
`v.` or `v-`. The repo state of this tag will be used to generate the stable
package.

### Notes

*Versions for packages on the original MELPA server are based on the date of the last commit and will likely be higher than any version on the stable server.* Keep the following things in mind,

* If you leave the original MELPA server in your `package-archives`
  then by default you will get the *development* versions of packages
  and not the stable ones.

* You will probably want to remove all packages and then reinstall
  them. Any packages you already have installed from MELPA will never
  get "updated" to the stable version because of the way version
  numbering is handled.



  

