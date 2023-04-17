#!/bin/bash -e

THIS_DIR=$(dirname "$0")

# TODO: switch output file name once tested & working

# Unstable
"$THIS_DIR/buildstats" /mnt/store/log /mnt/db/parquet html/download_counts.json

# Stable
"$THIS_DIR/buildstats" /mnt/store/log-stable /mnt/db/parquet-stable html-stable/download_counts.json

# Snapshots
"$THIS_DIR/buildstats" /mnt/store/log-snapshots /mnt/db/parquet-snapshots html-snapshots/download_counts.json

# Releases
"$THIS_DIR/buildstats" /mnt/store/log-releases /mnt/db/parquet-releases html-releases/download_counts.json

echo "Sleeping"
sleep 1800
