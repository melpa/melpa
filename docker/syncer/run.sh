#!/bin/bash -e

MELPA_REPO=/mnt/store/melpa
cd "${MELPA_REPO}"

unset STABLE

make cleanup
make html

export STABLE=t

make cleanup
make html

# Sync every 5 minutes.
sleep 5m
