#!/bin/bash -e

CHANNEL=unstable  make archive-contents json html
CHANNEL=stable    make archive-contents json html
CHANNEL=snapshots make archive-contents json html

# Sync every 5 minutes.
sleep 5m
