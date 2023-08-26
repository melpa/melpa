#!/bin/bash -e

MELPA_REPO=/mnt/store/melpa
cd "${MELPA_REPO}"

# Unstable
/usr/bin/python3 ${MELPA_REPO}/docker/logprocessor/process_log.py \
  --db /mnt/db/download_log_full.duckdb \
  --jsondir html \
  /mnt/store/log/melpa.access.log

# Stable
/usr/bin/python3 ${MELPA_REPO}/docker/logprocessor/process_log.py \
   --db /mnt/db/download_log_stable_full.duckdb \
   --jsondir html-stable \
   /mnt/store/log-stable/melpa.access.log
