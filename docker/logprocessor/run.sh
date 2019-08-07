#!/bin/bash -e

MELPA_REPO=/mnt/store/melpa
cd "${MELPA_REPO}"

unset STABLE
/usr/bin/python ${MELPA_REPO}/docker/logprocessor/process_log.py

export STABLE=t
/usr/bin/python ${MELPA_REPO}/docker/logprocessor/process_log.py

