#!/bin/bash -e

MELPA_CHANNEL=unstable make archive-contents json html
MELPA_CHANNEL=stable   make archive-contents json html

# Sync every 5 minutes.
sleep 5m
