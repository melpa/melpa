# MELPA

MELPA is a growing collection of `package.el`-compatible Emacs Lisp
packages built automatically on our server from the upstream source
code using simple recipes. (Think of it as a server-side version of
[el-get](https://github.com/dimitri/el-get), or even
[homebrew](https://github.com/mxcl/homebrew).)

Packages are updated hourly.

If you just want to browse and install packages, check out the
[archive index page](http://melpa.milkbox.net/) for instructions.

Adding packages is as simple as submitting a pull request; read on for
details.

## Table of Contents

* [Usage](#usage)
* [Contributing](#contributing-new-packages)
* [Package Format](#package-format)
* [Server Scripts](#server-scripts)
* [API](#api)
* [MELPA Package](#melpa-package)
* [About](#about)


## Usage

To use the MELPA repository, add it to `package-archives` before the
call to `package-initialize` in your `init.el` file.

    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.milkbox.net/packages/") t)
    
Since `package.el` doesn't allow locking packages to certain version,
we also provide a package `melpa.el` which contains code to allow
restricting packages to specific repositories.  This allows someone to
blacklist packages that come from a specific repository, or blacklist
all packages from a repository and only whitelist certain packages.

See the [MELPA Package](#melpa-package) section below or
[Installing](http://melpa.milkbox.net/#installing) section on the
MELPA homepage.


## Contributing New Packages

For submitting new packages we ask you following the following
guidelines,

* Upstream source must be stored in an authoritative
  [SCM](http://en.wikipedia.org/wiki/Software_configuration_management)
  repository or on the Emacswiki.
  
<!-- * Package must be actively developed and not otherwise included in a -->
<!--   different ELPA archive. Packages that are better suited for -->
<!--   [marmalade](http://marmalade-repo.org/) -->
  
* Submit one pull request per recipe.  You can create multiple
  branches and create a pull request for each branch.

* Recipes should try to minimize the size of the resulting package by
  specifying only files relevant to the package. See the
  [Package Format](#package-format) section for more information on
  specifying package files.
  
* The package name should match the name of the feature provided.  See
  the `package` function for more information.
  
* Packages should adhere to the `package.el` format as specified by
  `(info "(elisp) Packaging")`. More information on this format is
  provided by the
  [marmalade package manual](http://marmalade-repo.org/doc-files/package.5.html).
  
  

### Testing

Let `<NAME>` denote the name of the recipe to submit.

1. Fork the MELPA repository.
2. Add your new file under the directory specified by
`package-build-recipes-dir` (default: `recipes/` directory where
`package-build` was loaded).
3. Confirm your package build properly by running

        make recipes/<NAME>
       
4. Install the file you built by running `package-install-file` from
within Emacs and specifying the newly built package in the directory
specified by `package-build-archive-dir` (default: `packages/`
directory where `package-build` was loaded).

### Submitting

After verifying the entry works properly please open a pull request on
Github. Consider the [hub](https://github.com/defunkt/hub)
command-line utility by [defunkt](http://chriswanstrath.com/) which
helps simplify this process.


## Package Format

Packages are specified by files in the `recipes` directory.  You can
contribute a new package by adding a new file under `recipes` using
the following form,

```elisp
(<package-name>
 :fetcher [git|github|bzr|hg|darcs|svn|cvs|wiki]
 [:url "<repo url>"]
 [:repo "github-user/repo-name"]
 [:module "cvs-module"]
 [:files ("<file1>", ...)])
```

- `package-name`
a lisp symbol that has the same name as the package being specified.

- `:url`
specifies the URL of the version control repository. *required for
the `git`, `bzr`, `hg`, `darcs`, `svn` and `cvs` fetchers*

- `:module`
specifies the module of a CVS repository to check out.  Defaults to to
`package-name`.  Only used with `:fetcher cvs`, and otherwise ignored.

- `:fetcher`
specifies the type of repository that `:url` points to.  Right now
package-build supports [git][git], [github][github],
[bazaar (bzr)][bzr], [mercurial (hg)][hg],
[subversion (svn)][svn], [cvs][cvs] [darcs][darcs], and
[Emacs Wiki (wiki)][emacswiki] as possible mechanisms for checking out
the repository.  With the exception of the Emacs Wiki fetcher,
package-build uses the corresponding application to update files
before building the package.  The Emacs Wiki fetcher gets the latest
version of the package from
`http://www.emacswiki.org/emacs/download/<NAME>.el` where `NAME` is
the package name.  Note that the `:url` property is not needed for the
`wiki` engine unless the name of the package file on the EmacsWiki
differs from the package name being built. In the case of the `github`
fetcher, use `:repo` instead of `:url`; the git URL will then be
deduced.

- `:files`
optional property specifying the explicit files used to build the
package.  Automatically populated by matching all `.el` files in the
root of the repository.  This is necessary when there are multiple
`.el` files in the repository but the package should only be built
from a subset.  *Any file in any path in the repository is copied to
the root of the package*

[git]: http://git-scm.com/
[github]: https://github.com/
[bzr]: http://bazaar.canonical.com/en/
[hg]: http://mercurial.selenic.com/
[svn]: http://subversion.apache.org/
[cvs]: http://www.nongnu.org/cvs/
[darcs]: http://darcs.net/
[emacswiki]: http://www.emacswiki.org/


### Single File Repository

[ido-ubiquitous](https://github.com/DarwinAwardWinner/ido-ubiquitous) is a repository that contains two files:

* `README.md`
* `ido-ubiquitous.el`

Since there is only one `.el` file, this package only needs the `:url` and `:fetcher` specified,

```elisp
(ido-ubiquitous
 :url "https://github.com/DarwinAwardWinner/ido-ubiquitous.git"
 :fetcher git)
``` 

### Multiple Packages in one Repository

The
[emacs-starter-kit](https://github.com/technomancy/emacs-starter-kit)
contains the *starter-kit* package along with extra packages in the
`modules` directory; *starter-kit-bindings*, *starter-kit-lisp*, etc.

```elisp
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


### Multiple Files in Multiple Directories

There are special cases when we need 
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
    
[melpa]: http://melpa.milkbox.net


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
recipes.  See [Package Format](#package-format) section for more details.


## Configuration

Packages end up in the `packages/` directory by default.
This can be configured using the `package-build-archive-dir` variable.

Repositories are checked out to the `working/` directory by default.
This can be configured using the `package-build-working-dir` variable.

## MELPA Package

The `melpa.el` package---available in MELPA--allows creating a
whitelist or blacklist of packages for a specific repository.  This
allows for disabling all packages from a specific repository and only
enabling certain packages, or simply blacklist a certain subset of packages.

### Configuring

By default there are two variables that can be customized to specify
which packages will be enabled (whitelist packages only) or excluded
(blacklist of packages)


- `package-archive-enable-alist` : Optional Alist of enabled packages
    used by `package-filter`. The format is (ARCHIVE . PACKAGE ...),
    where ARCHIVE is a string matching an archive name in
    `package-archives`, PACKAGE is a symbol of a package in ARCHIVE to
    enable. If no ARCHIVE exists in the alist, all packages are
    enabled.

    If no ARCHIVE exists in the alist, all packages are enabled.

<!-- extra padding??? -->

- `package-archive-exclude-alist` : Alist of packages excluded by
    `package-filter`. The format is (ARCHIVE . PACKAGE ...), where
    ARCHIVE is a string matching an archive name in
    `package-archives`, PACKAGE is a symbol of a package in that
    archive to exclude. Any specified package is excluded regardless
    of the value of `package-archive-enable-alist`
    

    If a particular ARCHIVE has an entry in
`package-archive-enable-alist` then only packages



### Manual Installation

You can install the package manually by pasting this into yoru `*scratch*` buffer and evaluating it.

    (progn
      (switch-to-buffer
       (url-retrieve-synchronously
        "https://raw.github.com/milkypostman/melpa/master/melpa.el"))
      (package-install-from-buffer  (package-buffer-info) 'single))





## About

*MELPA* is *Milkypostman's ELPA* or *Milkypostman's Experimental Lisp
 Package Archive* if you're not into the whole brevity thing.

