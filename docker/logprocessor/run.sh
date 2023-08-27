#!/bin/bash -e

THIS_DIR=$(dirname "$0")

# Unstable
"$THIS_DIR/buildstats" /mnt/store/log > html/download_counts_new.json

# Stable
"$THIS_DIR/buildstats" /mnt/store/log-stable > html-stable/download_counts_new.json
