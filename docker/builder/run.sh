#!/bin/bash -e

# Envvars with empty defaults:
# - INHIBIT_PACKAGE_PULL
# - INHIBIT_MELPA_PULL

: ${BUILD_CHANNELS="unstable:stable"}

# Break taken between runs, in seconds.
: ${BUILD_PAUSE="-300"}

# A timeout is only needed for unattended builds, so we set this
# here instead of forcing it on everyone in the Makefile or even
# by giving the lisp variable a non-nil default value.
LISP_CONFIG="(setq package-build-timeout-secs 600)"

MELPA_REPO=/mnt/store/melpa
cd "${MELPA_REPO}"

BUILD_STATUS_FILE="${MELPA_REPO}/html/build-status.json"

export INSIDE_DOCKER=true
export GIT_HTTP_USER_AGENT="melpa.org"
export LANG=en_US.UTF-8

if [ -z "$INHIBIT_MELPA_PULL" ]
then
    echo ">>> Pulling MELPA repository"
    MELPA_BRANCH=$(git rev-parse --abbrev-ref HEAD)
    git fetch origin
    git reset --hard "origin/${MELPA_BRANCH}"
    git pull origin "${MELPA_BRANCH}"
    echo
fi

record_build_status() {
    echo "Recording build status in $BUILD_STATUS_FILE"
    cat <<EOF | tee $BUILD_STATUS_FILE
{
  "started": $BUILD_STARTED,
  "completed": ${BUILD_COMPLETED-null},
  "duration": ${BUILD_DURATION-null},
  "next": ${BUILD_NEXT-null}
}
EOF
}

# Indicate that the build is in progress.
if [ -e ${BUILD_STATUS_FILE} ]
then
    BUILD_DURATION=$(jq ".duration" ${BUILD_STATUS_FILE})
else
    BUILD_DURATION=0
fi
BUILD_STARTED=$(date "+%s")
record_build_status

if [ -z "$INHIBIT_PACKAGE_PULL" ]
then
    if [ -n "$BUILD_CHANNELS" ]
    then
        # Fetch all packages when updating first channel.
        export BUILD_CONFIG="$LISP_CONFIG"
    else
        # Fetch all packages but don't build any channel.
        export BUILD_CONFIG="(progn $LISP_CONFIG\
          (setq package-build-build-function 'ignore))"
        make -k -j8 build || true
    fi
else
    # Don't fetch packages.
    export BUILD_CONFIG="(progn $LISP_CONFIG\
      (setq package-build-fetch-function 'ignore))"
fi

for channel in $(echo "$BUILD_CHANNELS" | tr ":" " ")
do
    echo ">>> Starting to build \"$channel\" channel"
    export MELPA_CHANNEL=$channel
    pkgdir=$(make get-pkgdir)
    if [ -e "$pkgdir/errors.log" ];
    then
        mv "$pkgdir/errors.log" "$pkgdir/errors-previous.log"
    fi
    make -k -j8 build || true
    make indices
    # Don't fetch packages a second time.
    export BUILD_CONFIG="(progn $LISP_CONFIG\
      (setq package-build-fetch-function 'ignore))"
done

# Indicate that the build has completed.
BUILD_COMPLETED=$(date "+%s")
BUILD_DURATION=$((BUILD_COMPLETED - BUILD_STARTED))
BUILD_NEXT=$((BUILD_COMPLETED + BUILD_PAUSE))
record_build_status

if [ ! "$BUILD_PAUSE" = 0 ]
then
    echo "Sleeping for $BUILD_PAUSE seconds before next build"
    sleep $BUILD_PAUSE
fi
