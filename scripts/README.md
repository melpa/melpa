# MELPA Scripts

This directory contains standalone scripts.

* `parallel_build_all` will build all packages using multiple emacs
  instances.

* `process_log.py` scans the nginx log files and pulls out download
  totals.

* `expired` shows which packages are out of date based on the fact
  that the package file has not been updated in the last 24 hours.

* `bootstrap` is a script to help bring up a new MELPA server running
  on Ubuntu.

* `missing` shows which packages have a recipe but do not have a
  corresponding package file.

