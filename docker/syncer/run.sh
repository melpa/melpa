#!/bin/bash -e

MELPA_REPO=/mnt/store/melpa
cd "${MELPA_REPO}"

unset STABLE

make archive-contents json html

export STABLE=t

make archive-contents json html

# Sync every 5 minutes.
sleep 5m
