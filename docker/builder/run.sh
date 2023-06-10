#!/bin/bash -e

# Break taken between runs, in seconds.
BUILD_DELAY=3600

# A timeout is only needed for unattended builds, so we set this
# here instead of forcing it on everyone in the Makefile or even
# by giving the lisp variable a non-nil default value.
LISP_CONFIG="(setq package-build-timeout-secs 600)"

MELPA_REPO=/mnt/store/melpa
cd "${MELPA_REPO}"

echo ">>> Pulling MELPA repository"
MELPA_BRANCH=$(git rev-parse --abbrev-ref HEAD)
git fetch origin
git reset --hard "origin/${MELPA_BRANCH}"
git pull origin "${MELPA_BRANCH}"
echo

record_build_status() {
    local file="${MELPA_REPO}/html/build-status.json"
    echo "Recording build status in $file"
    cat <<EOF > $file
{
  "started": $BUILD_STARTED,
  "completed": ${BUILD_COMPLETED-null},
  "duration": ${BUILD_DURATION-null},
  "next": ${BUILD_NEXT-null},
}
EOF
    cat "$file"
    # FIXME "melpa.js" expects this file in a channel-specific
    # location, but we no longer record this per channel.  For
    # now just duplicate the file.
    cp "$file" "${MELPA_REPO}/html-stable/build-status.json"
}

# Indicate that the build is in progress
BUILD_DURATION=$(jq ".duration" build-status.json)
BUILD_STARTED=$(date "+%s")
record_build_status

echo ">>> Starting UNSTABLE build"
unset STABLE
export BUILD_CONFIG="$LISP_CONFIG"
docker/builder/parallel_build_all

echo ">>> Starting STABLE build"
export STABLE=t
export BUILD_CONFIG="(progn $LISP_CONFIG
  (setq package-build-fetch-function 'ignore))"
docker/builder/parallel_build_all

# Indicate that the build has completed
BUILD_COMPLETED=$(date "+%s")
BUILD_DURATION=$((BUILD_COMPLETED - BUILD_STARTED))
BUILD_NEXT=$((BUILD_COMPLETED + BUILD_DELAY))
record_build_status

sleep $BUILD_DELAY
