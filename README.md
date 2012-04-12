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
 :fetcher [git|github|bzr|hg|darcs|svn|wiki|targz|raw]
 [:url "<repo url>"]
 [:repo "github-user/repo-name"]
 [:files ("<file1>", ...)])
```     

name`
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
The `targz` fetcher can be used to build packages from a 
downloaded tarball `:url`. 
The `raw` fetcher expects a `:url` of an emacs lisp file.

`:files`
:   optional property specifying the explicit files used to build the
package.  Automatically populated by matching all `.el` files in the
root of the repository.  This is necessary when there are multiple
`.el` files in the repository but the package should only be built
from a subset.

`:version`
:   optional property specifying the package version to report as present on this elisp archive. Every fetcher type determines automatically this version number from the date-of-retrieval depending on the method, however in some cases you would like this melpa archive to report it has the always-newest-version of a package, this could be handy when you have a custom melpa archive and want some elisp packages to always install from it, even if other archives have more really-up-to-date versions. A way to acomplish this would be to use a bigger natural-order version on your custom melpa archive, that way that particular package will always be taken from your custom melpa archive.

[git]: http://git-scm.com/
[github]: https://github.com/
[bzr]: http://bazaar.canonical.com/en/
[hg]: http://mercurial.selenic.com/
[svn]: http://subversion.apache.org/
[darcs]: http://darcs.net/
[emacswiki]: http://www.emacswiki.org/

### Recipe Examples

#### wiki

To create a package for an elisp maintained on the emacs wiki, for example
if you wanted to create a recipe for [wn-mode.el](http://www.emacswiki.org/emacs/wn-mode.el) you just need to create a recipe that looks like this:

```lisp
(wn-mode :fetcher wiki)
```

Other packages can consist of many files like [Icicles](http://emacswiki.org/emacs/Icicles)

```lisp
(icicles :fetcher wiki :files
         ("icicles.el" "icicles-chg.el" "icicles-cmd1.el" "icicles-cmd2.el" "icicles-doc1.el" "icicles-doc2.el" "icicles-face.el" "icicles-fn.el" "icicles-mac.el" "icicles-mcmd.el" "icicles-mode.el" "icicles-opt.el" "icicles-var.el"))
```

#### git

Most recently many elisp libraries are being developed using git, to create a recipe for one of them, say [Evil](http://emacswiki.org/emacs/Evil) you do:

```lisp
(evil :url "git://gitorious.org/evil/evil.git" :fetcher git)
```

An optional `:commit` keyword can be used to specify the commit to checkout.

#### github

This fetcher is a shortcut for packages fetched from github repos.

```lisp
(helm :repo "emacs-helm/helm" :fetcher github)
```

#### targz

Retrieving a tarball from a github download or gist tar. 
The difference between using a `github` or `targz` fetcher
is that the later just downloads a tarball and doesnt keep a local
clone of the github repo. The `:url` can point to any
http-exposed tarball not only github downloads and gist tars.
Note that because you're using github's tarball urls, you can
replace `master` with any tag or commit number.

```lisp
(ido-better-flex
 :fetcher targz
 :url "https://github.com/vic/ido-better-flex/tarball/master")
```

#### raw

The `raw` fetcher can be used to obtain elisp libraries from http-exposed
plain files.

For example, say you found [Edward O'Connor elisp files](http://edward.oconnor.cx/elisp/) and wanted to create a package for his `color-theme-hober2`

```lisp
(color-theme-hober2
  :fetcher raw
  :url "http://edward.oconnor.cx/elisp/color-theme-hober2.el")
```

Or you want to create a package for his OS-X hacks

```lisp
(oconnor-osx-hacks
  :fetcher raw
  :url "http://edward.oconnor.cx/elisp"
  :files ("growl.el" "osx-plist.el"))
```

Note that if you dont provide a `:url` keyword,
`:files` are expected to be absolute urls.

```lisp
(my-secrets
  :fetcher raw
  :files ("file:///my/secrets.el" "ftp://my.corp/dev/env.el"))
```

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
