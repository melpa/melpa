# MELPA

MELPA is a growing collection of `package.el`-compatible Emacs Lisp
packages built automatically on our server from the upstream source
code using simple recipes. (Think of it as a server-side version of
[el-get](https://github.com/dimitri/el-get), or even
[homebrew](https://github.com/mxcl/homebrew).)

Packages are updated when changes are made to the MELPA repository,
or at least daily.

If you just want to browse and install packages, check out the
[archive index page](http://melpa.milkbox.net/) for instructions.

Adding packages is as simple as submitting a pull request; read on for
details.

### About the name

*MELPA* is *Milkypostman's ELPA* or *Milkypostman's Experimental Lisp
 Package Archive* if you're not into the whole brevity thing.

## Scripts

* `buildpkg` -- Create an archive of the package(s) passed as
arguments to the script. Built packages are put in the `packages/`
folder with version corresponding to the newest HEAD revision
available; given according to the `%Y%m%d` format.

* `melpa` -- All the logic for generating everything in the repository
based on the recipe files.  By default this will clean the `packages/` directory,
build all packages
listed under `recipes/`, and compile the `index.html` file for the [melpa]
website front page.

The following arguments are accepted:

clear
:   clean out the `packages/` directory

build
:   build all packages in `pkglist`

index
:   build the `index.html` file

validate
:   naively validate that the correct number of packages were built.

Note that these scripts require an Emacs with `package.el` installed,
such as Emacs 24. If you have an older version of Emacs, you can get a
suitable `package.el` [here](http://bit.ly/pkg-el23).

[melpa]: http://melpa.milkbox.net

## Code

The `package-build.el` file contains all the heavy lifting. The
scripts above call the `package-build-archive` function from the
command-line to actually build the package(s).

Use `(package-build-all)` to build all melpa packages.

Alternatively you can
load this file from within Emacs and issues commands from there.

The `package-build.el` automatically generates any required
information for the package. For multi-file packages this include
generating the file `<NAME>-pkg.el` which contains *description*,
*version*, and *requires* information determined by searching
`<NAME>-pkg.el`, `<NAME>.el`, and `<NAME>-pkg.el.in` if they exist in
the repository.

## Contributing New Packages

Packages are specified by files in the `recipes` directory.  You can
contribute a new package by adding a new file under `recipes` using
the following form,

```elisp
(name
 :fetcher [git|github|bzr|hg|darcs|svn|wiki]
 [:url "<repo url>"]
 [:repo "github-user/repo-name"]
 [:files ("<file1>", ...)])
```     

`name`
:   a lisp symbol that has the same name as the package being specified.

`:url`
:   specifies the URL of the version control repository. *required for
the `git`, `bzr`, `hg`, `darcs` and `svn` fetchers*

`:fetcher`
:   specifies the type of repository that `:url` points to.  Right now
package-build supports [git][git], [github][github],
[bazaar (bzr)][bzr], [mercurial (hg)][hg],
[subversion (svn)][svn], [darcs][darcs], and
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

`:files`
:   optional property specifying the explicit files used to build the
package.  Automatically populated by matching all `.el` files in the
root of the repository.  This is necessary when there are multiple
`.el` files in the repository but the package should only be built
from a subset.

[git]: http://git-scm.com/
[github]: https://github.com/
[bzr]: http://bazaar.canonical.com/en/
[hg]: http://mercurial.selenic.com/
[svn]: http://subversion.apache.org/
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

### Submitting the Package

You should first fork the MELPA repository, add your new file under
`recipes`, and confirm your new package builds properly by running
`buildpkg <NAME>`.  You can install the package that you built by
running the interactive command `package-install-file` in Emacs, and
specifying the newly built package which should be in the `packages/`
subdirectory under the melpa directory.

After verifying the entry works properly please open a pull request on Github.

## Configuration

Packages end up in the `packages/` directory by default.
This can be configured using the `package-build-archive-dir` variable.

Repositories are checked out to the `working/` directory by default.
This can be configured using the `package-build-working-dir` variable.
