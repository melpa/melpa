# Contributing to MELPA

## Contributing New Recipes

New recipe submissions should adhere to the following guidelines,

* One pull request per recipe. You can create multiple branches and
  create a pull request for each branch.

* Upstream source must be stored in an authoritative
  [SCM](https://en.wikipedia.org/wiki/Software_configuration_management)
  repository. EmacsWiki recipes are no longer accepted.

* Packages should be built from the *official* package repository.
  Forks of the official repository will not be accepted except in
  *extreme* circumstances.

* The package name should match the name of the feature provided.  See
  the `package` function for more information.

* The package should follow the conventions of [Emacs Lisp](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html)

* Packages should adhere to the `package.el` format as specified by
  `(info "(elisp) Packaging")`. More information on this format is
  provided by the
  [marmalade package manual](https://web.archive.org/web/20111120220609/http://marmalade-repo.org/doc-files/package.5.html).

* Recipes should try to minimize the size of the resulting package by
  specifying only files relevant to the package. See the
  [Recipe Format](README.md#recipe-format) section of the README for more
  information on specifying package files.

* To have a stable version generated for your package simply tag the
  repository using a naming compatible with `version-to-list`. The
  repo state of this tag will be used to generate the stable package.


## Expediting Recipe Reviews

Because we care about the quality of packages that are part of MELPA
we review all submissions. The following steps can help us with this
process and expedite the recipe review process,

* Use [package-lint](https://github.com/purcell/package-lint)
  and [flycheck-package](https://github.com/purcell/flycheck-package)
  to help you identify common errors in your package metadata.

* Use *checkdoc* to make sure that your package follows the
  conventions for documentation strings. See the official
  [Emacs manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Documentation-Tips.html#Documentation-Tips) for
  details.

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


## Testing

Let `<NAME>` denote the name of the recipe to submit.

1. Fork the MELPA repository.
2. Add your new file under the directory specified by
`package-build-recipes-dir` (default: `recipes/` directory where
`package-build` was loaded). If you prefer, the interactive command
`package-build-create-recipe` in `package-build/package-build.el` will
guide you through this process.

3. Confirm your package builds properly by running

        make recipes/<NAME>

  (Be sure that the `emacs` on your path is at least version 23, or
  set `$EMACS_COMMAND` to the location of a suitable binary.)

  Alternatively, open the recipe in Emacs and press `C-c C-c` in the
  recipe buffer: this will also prompt you to install the
  freshly-built package.

  If the repository contains tags for releases, confirm that the correct
  version is detected by running `STABLE=t make recipes/<NAME>`.  The
  version detection can be adjusted by specifying `:version-regexp` in
  the recipe (see ["Recipe Format"](README.md#recipe-format) in the
  README).

4. Install the file you built by running `package-install-file` from
within Emacs and specifying the newly built package in the directory
specified by `package-build-archive-dir` (default: `packages/`
directory where `package-build` was loaded).

You can optionally run a sandboxed Emacs in which locally-built
packages will be available for installation along with those already
in MELPA:

```
EMACS_COMMAND=/path/to/emacs make sandbox INSTALL=package-name
```

where `package-name` is the name of the package you want to install
into the sandbox, then install and test as appropriate. This is a
useful way to discover missing dependencies!

## Submitting

After verifying the entry works properly please open a pull request on
GitHub. Consider the [hub](https://github.com/github/hub)
command-line utility by [defunkt](http://chriswanstrath.com/) which
helps simplify this process.
