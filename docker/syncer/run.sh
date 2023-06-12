#!/bin/bash -e

MELPA_REPO=/mnt/store/melpa
cd "${MELPA_REPO}"

unset STABLE

make -j4 archive-contents json html

export STABLE=t

make -j4 archive-contents json html

# Sync every 5 minutes.
sleep 5m
