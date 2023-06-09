#!/bin/bash -e

# Break taken between runs, in seconds.
BUILD_DELAY=3600

# A timeout is only needed for unattended builds, so we set this
# here instead of forcing it on everyone in the Makefile or even
# by giving the lisp variable a non-nil default value.
LISP_CONFIG="(setq package-build-timeout-secs 600)"

MELPA_REPO=/mnt/store/melpa
cd "${MELPA_REPO}"
MELPA_BRANCH=$( git rev-parse --abbrev-ref HEAD )

STATUS_JSON=${MELPA_REPO}/html/build-status.json
LAST_DURATION_FILE=${MELPA_REPO}/html/.last-build-duration
STABLE_STATUS_JSON=${MELPA_REPO}/html-stable/build-status.json
STABLE_LAST_DURATION_FILE=${MELPA_REPO}/html-stable/.last-build-duration

## update MELPA repo
git fetch origin
git reset --hard "origin/${MELPA_BRANCH}"
git pull origin "${MELPA_BRANCH}"
echo

update_json() {
    cat <<EOF > $BUILD_STATUS_JSON
{
  "started": $BUILD_STARTED,
  "completed": ${BUILD_COMPLETED-null},
  "next": ${BUILD_NEXT-null},
  "duration": ${BUILD_DURATION-null}
}
EOF
    echo "Writing $BUILD_STATUS_JSON"
    cat "$BUILD_STATUS_JSON"
}

flux_capacitor() {

    if [ -f "$BUILD_LAST_DURATION_FILE" ]; then
        BUILD_DURATION=$(cat "$BUILD_LAST_DURATION_FILE")
    fi


    BUILD_STARTED=$(date "+%s")
    update_json

    # Build all the packages.
    docker/builder/parallel_build_all

    # Store completed date
    BUILD_COMPLETED=$(date "+%s")
    BUILD_DURATION=$((BUILD_COMPLETED - BUILD_STARTED))
    echo -n "$BUILD_DURATION" > $BUILD_LAST_DURATION_FILE
    BUILD_NEXT=$((BUILD_COMPLETED + BUILD_DELAY))
    update_json

}

echo '>>>> STARTING UNSTABLE BUILD'
unset STABLE
export BUILD_CONFIG="$LISP_CONFIG"

BUILD_STATUS_JSON=${STATUS_JSON}
BUILD_LAST_DURATION_FILE=${LAST_DURATION_FILE}
if [ -f "$STABLE_LAST_DURATION_FILE" ]; then
    BUILD_DELAY=$(cat "$STABLE_LAST_DURATION_FILE")
fi

flux_capacitor

echo '>>>> STARTING STABLE BUILD'
export STABLE=t
export BUILD_CONFIG="$LISP_CONFIG"

BUILD_STATUS_JSON=${STABLE_STATUS_JSON}
BUILD_LAST_DURATION_FILE=${STABLE_LAST_DURATION_FILE}
if [ -f "$LAST_DURATION_FILE" ]; then
    BUILD_DELAY=$(cat "$LAST_DURATION_FILE")
fi

flux_capacitor

sleep $BUILD_DELAY
