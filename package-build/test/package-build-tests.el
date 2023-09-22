;;; package-build-tests.el --- Tests for Package-Build  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Jonas Bernoulli

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)

(require 'package-build)

(defmacro package-build-test-package (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((package-build-verbose nil)
          (package-build-fetch-function #'ignore)
          (package-build-checkout-function #'ignore)
          (package-build-stable nil)
          (package-build-snapshot-version-functions
           (list #'package-build-release+count-version))
          (package-build-release-version-functions
           (list #'package-build-tag-version))
          (test (ert-running-test))
          (name (symbol-name (ert-test-name test)))
          (_ (string-match "\\`package-build-test-\\([0-9]+\\)-\\(.+\\)" name))
          (num  (match-string 1 name))
          (desc (match-string 2 name))
          (package-build-working-dir
           (file-name-as-directory
            (expand-file-name num package-build-working-dir)))
          (elpa-n ?A)
          (verbose (member (getenv "PB_TEST_VERBOSE") '("t" "true"))))
     (when verbose
       (message "\n=== %s %s ===\n\n%s\n" num desc
                (replace-regexp-in-string
                 "^" "  " (ert-test-documentation test))))
     (make-directory package-build-working-dir t)
     (make-directory package-build-archive-dir t)
     (make-directory package-build-recipes-dir t)
     (with-temp-file (expand-file-name "pkg" package-build-recipes-dir)
       (insert "(pkg :fetcher git :url \"https://example.com\")\n"))
     (cl-flet* ((git (&rest args)
                  (with-temp-buffer
                    (unless (zerop (apply #'call-process "git" nil t nil args))
                      (error "%s" (buffer-string)))
                    (buffer-string)))
                (tag (&rest args)
                  (apply #'git "tag" "--no-sign" "--force" args))
                (rec (msg &optional file &rest args)
                  (git "add" (or file "."))
                  (let ((process-environment
                         (copy-sequence process-environment)))
                    (setenv "GIT_AUTHOR_NAME" "abc")
                    (setenv "GIT_AUTHOR_EMAIL" "a@b.c")
                    (setenv "GIT_AUTHOR_DATE" "1970-01-01T00:00:00Z")
                    (setenv "GIT_COMMITTER_NAME" "abc")
                    (setenv "GIT_COMMITTER_EMAIL" "a@b.c")
                    (setenv "GIT_COMMITTER_DATE" "1970-01-01T00:00:00Z")
                    (apply #'git "commit" "--allow-empty" "-m" msg args))
                  (git "update-ref" "refs/remotes/origin/main" "main"))
                (mod (file &optional content msg)
                  (with-temp-file file
                    (when content
                      (insert content "\n"))
                    (when (equal file "pkg.el")
                      (insert "(require 'pkg)\n")))
                  (when msg
                    (rec msg file)))
                (reset (rev)
                  (git "reset" "--hard" rev)
                  (git "update-ref" "refs/remotes/origin/main" "main"))
                (build ()
                  (when verbose
                    (message "Building from %s/working/%s/pkg"
                             package-build--melpa-base num))
                  (package-build-archive "pkg" t))
                (check (version commit &optional silent)
                  (when (zerop (call-process "git" nil t nil
                                             "rev-parse" "--verify" commit))
                    (tag (format "elpa_%c__%s" elpa-n version) commit))
                  (setq elpa-n (1+ elpa-n))
                  (when (and (not silent) verbose)
                    (message
                     "\n%s"
                     (git "log" "--oneline" "--graph" "--color"
                          ;; "--no-abbrev-commit"
                          "--branches" "--tags" "--decorate"
                          "--decorate-refs-exclude=HEAD"
                          "--decorate-refs-exclude=refs/heads/main"
                          "--decorate-refs-exclude=refs/remotes/origin/HEAD")))
                  (let ((elt (cdr (assq 'pkg (package-build-archive-alist)))))
                    (should (equal (cdr (assq :commit (aref elt 4))) commit))
                    (should (equal (package-version-join (aref elt 0))
                                   version))))
                (run (version commit &optional silent)
                  (build)
                  (check version commit silent)))
       (let ((wtree (expand-file-name "pkg" package-build-working-dir)))
         (git "init" "-b" "main" wtree)
         (let ((default-directory wtree))
           (mod "pkg.el" nil "Initial import")
           (git "symbolic-ref"
                "refs/remotes/origin/HEAD"
                "refs/remotes/origin/main")
           ,@body)))))

(ert-deftest package-build-test-001-use-latest-relevant-commit ()
  "Base the snapshot version string on the greatest release tag.

Determine the last reachable commit that touches a file that we
want to include in packages.  Skip over commits that touch only
files that are not included.  By doing so, we avoid building new
snapshots that are identical to the previous snapshot, except for
the version string.

Then determine the greatest release tag, while ignoring whether
that is an ancestor of the selected commit.  (Below we will look
into what it means, if that is not the case, but for now assume
it is.)

The snapshot version string has the format \"VERSION.0.COUNT\".
VERSION is the version string derived from the selected tag and
COUNT is the number of commits from that tag to the selected
commit.

Inject the \"separator\" \".0\" inbetween the VERSION and the
COUNT to sufficiently decrease the odds that the version for a
future release is smaller than the version for this snapshot.

(Ideally Emacs would not only support \"pre-releases\" but also
\"post-releases\".  If that were the case, we could use something
like \"1.0-git42\", for a commit that comes 42 commits after the
tag \"1.0\".  But as it is implemented in `version<' et al.,
\"1.0-git\" is actually smaller than \"1.0\".)

Injecting one \".0\" is both necessary and sufficient.  Just
because the last release is \"1.0\", we cannot be sure that the
next release will be either \"2.0\" or \"1.1\".  It might also be
\"1.0.1\".  By using \"1.0.0.COUNT\" instead of just
\"1.0.COUNT\", we nearly ensure that that any potential future
release is smaller than the snapshot.  Of course the next release
after \"1.0\" could also be \"1.0.0.1\" (or \"1.0.0.0.0.0.1\" for
that matter) but that is much less likely.

(We could use the separator \".1-snapshot\" to be absolutely sure
that a future release is always greater than a snapshot, but that
is disgustingly long.  Note that we could not use the shorter
\".0-git\" because `version-to-list' encodes both -snapshot and
-git as -4, and `package-version-join' turns -4 into -snapshot.)"
  (package-build-test-package
    (tag "1.0.0")
    (mod "pkg.el" "a" "Edit pkg.el")
    (mod "other" "b" "Add other")
    (run "1.0.0.0.1" "5cae135f5352549a5b989c2ca8f3a3a38268c12a")))

(ert-deftest package-build-test-002-dont-append-count-when-using-tag ()
  "When the latest relevant commit is a tagged release, then use
that version without appending a commit count."
  (package-build-test-package
    (tag "1.0.0")
    (mod "other" "b" "Add other")
    (run "1.0.0" "ded491870604ef18249181b96e54396a849da707")))

(ert-deftest package-build-test-003-dont-go-past-tag ()
  "If the latest commit that modifies a relevant file is an ancestor
of the latest release tag (as opposed to the other way around),
then use the release commit, because the version on the snapshot
channel should never be older than the version on the release
channel."
  (package-build-test-package
    (tag "1.0.0")
    (mod "pkg.el" "a" "Edit pkg.el")
    (rec "Release 1.0.1 without changes")
    (tag "1.0.1")
    (mod "other" "b" "Add other")
    (run "1.0.1" "b208dc6b8343dd95a07cbb97142351b9426bac20")))

(ert-deftest package-build-test-004-use-zeros-if-no-tag ()
  "If there is no release tag, use two zero parts before the
separator zero part and count part.  In this case the count is
the total number of commit, reachable from the selected commit.

So, if there are no tags, then the count part is always prefixed
with exactly three zero parts.  If the package author later uses
\"0.0.1\" as the first tag, then that is larger than existing
snapshots.  However if they begin with \"0.0.0.1\", then that is
not the case.  We have to draw the line somewhere and, IMO, if it
matters at all whether a certain release is used or not, then the
first bumped part of the version string should not be the
sub-sub-minor-part (this applies to the first release, but also
future version bumps)."
  (package-build-test-package
    (mod "other" "b" "Add other")
    (mod "pkg.el" "a" "Edit pkg.el")
    (run "0.0.0.3" "3ce2b15e048169dcc4193c18b5589bc90c60a32b")))

(ert-deftest package-build-test-005-count-since-merge-base ()
  "Sometimes release are made on dedicated release branches, and
sometimes these release branches are not merged back into the
development branch, immediately or at all.

If that happens, we cannot count the commits from the tag to the
latest relevant commit.  Instead we count the commits from the
the merge-base of the selected commit and the selected tag.  The
merge-base is the last common ancestor of two commits."
  (package-build-test-package
    (mod "other" "b" "Add other")
    (git "checkout" "-b" "releases" "HEAD~")
    (rec "Release 1.0.0")
    (tag "1.0.0")
    (git "checkout" "main")
    (mod "pkg.el" "a" "Edit pkg.el")
    (run "1.0.0.0.2" "3ce2b15e048169dcc4193c18b5589bc90c60a32b")))

(ert-deftest package-build-test-006-merge-base-required ()
  "Due to extremely sloppy history rewriting, it is possible that
tags share no history at all with the branches that still exist
in the repository.  This is the result of an upstream mistake,
and it is not our job to fix it.  Again, we have to draw the line
somewhere.

So, if the greatest release tag shares no history with the
tracked branch, then we pretend there are no tags at all.  (We
could instead ignore that tag and use the next greatest release
tag, which actually shares some history, if any, but IMO that
would be even more confusing to users.)"
  (package-build-test-package
    (tag "6.0.0")
    (mod "pkg.el" "a" "Edit pkg.el")
    (git "checkout" "--orphan" "detached")
    (rec "A root commit")
    (tag "6.0.1")
    (git "checkout" "main")
    ;; Pretend there are no tags.
    (run "0.0.0.2" "5cae135f5352549a5b989c2ca8f3a3a38268c12a")
    ;; Ignore orphan tags (here 6.0.1), but not other tags (6.0.0).
    ;; (run "6.0.0.1" "5cae135f5352549a5b989c2ca8f3a3a38268c12a")
    ))

(ert-deftest package-build-test-007-amending-adds-count-part ()
  "If HEAD is amended, add an additional count version part."
  (package-build-test-package
    (tag "7.0")
    (mod "pkg.el" "a" "Edit pkg.el")
    (run "7.0.0.1" "5cae135f5352549a5b989c2ca8f3a3a38268c12a" t)
    (rec "Edit pkg.el (modified)" nil "--amend")
    (run "7.0.0.1.1" "6d26f69fa2fa150d12b0b485b6f3e5f4948151fe")))

(ert-deftest package-build-test-008-dropping-adds-count-part ()
  "If HEAD is removed, add an additional count version part."
  (package-build-test-package
    (tag "8.0.0")
    (mod "pkg.el" "a" "Edit pkg.el")
    (mod "pkg.el" "b" "Edit pkg.el (will be dropped)")
    (run "8.0.0.0.2" "f138089d7313b60a729f75c5bb235fb6a2911ba9" t)
    (reset "HEAD~")
    (run "8.0.0.0.2.1" "5cae135f5352549a5b989c2ca8f3a3a38268c12a")))

(ert-deftest package-build-test-009-dropping-adds-count-part ()
  "Accumulate and shed smaller count parts.  The previous snapshot
may have multiple count parts.  Compare the new count with the
last of these.  If the new count is smaller than the last count,
then append the new count.  Otherwise remove the old count part.
Continue this process for preceeding count parts until there are
none left, or it is larger than the new count."
  (package-build-test-package
    (tag "1.0")
    (mod "pkg.el" "1" "1")
    (mod "pkg.el" "2" "2")
    (mod "pkg.el" "3" "3")
    (run "1.0.0.3" "313e86cfc8aea39b8eb2a5be15adf116eed6cce4" t)
    (reset "HEAD~")
    (mod "pkg.el" "4" "4")
    (run "1.0.0.3.3" "433ebf7c7586514389036ed2af449e00df54a2b8" t)
    (reset "HEAD~3")
    (mod "pkg.el" "5" "5")
    (run "1.0.0.3.3.1" "323cbd45df8b333689b9b5e7e0c5ff469b39b564" t)
    (reset "HEAD~1")
    (mod "pkg.el" "6" "6")
    (mod "pkg.el" "7" "7")
    (run "1.0.0.3.3.2" "43d78a452ad43a3ab4a603258779b95a028122fb" t)
    (reset "HEAD~1")
    (mod "pkg.el" "8" "8")
    (mod "pkg.el" "9" "9")
    (mod "pkg.el" "A" "A")
    (run "1.0.0.4" "5f141cefedd3f0104eb5ba6bda090ea9187adc01")))

;;; package-build-tests.el ends here
