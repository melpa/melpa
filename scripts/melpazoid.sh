#!/bin/bash

set -eux

EMACS="${EMACS:-emacs}"
PYTHON=$(which python3 || true)
PYTHON="${PYTHON:-python}"
ROOT=$(git rev-parse --show-toplevel)
WORKING="${ROOT}/working"
PACKAGES="${ROOT}/packages"
PKG_NAME=${1:-$(basename $(ls -1trd ${WORKING}/* | tail -1))}
PKG_REPO="${WORKING}/${PKG_NAME}"
MZDIR=${MZDIR:-$ROOT}
cd ${MZDIR}
if [[ -z $(du -s melpazoid-master 2>/dev/null | cut -f1) ]] || \
       [[ $(du -s melpazoid-master 2>/dev/null | cut -f1) -le "100" ]] ; then
    curl -sLk -O https://github.com/riscy/melpazoid/archive/master.zip
    unzip master.zip
    rm -f master.zip
fi

PKG_TAR=$(ls -1trd ${PACKAGES}/${PKG_NAME}*.{tar,el} 2>/dev/null | tail -1)
PKG_PATH="${PKG_TAR%.*}"
rm -rf ${PKG_PATH}
if [ "${PKG_TAR##*.}" == "tar" ]; then
    tar -C ${PACKAGES} -xf ${PKG_TAR}
else
    mkdir -p ${PKG_PATH}
    cp -p ${PKG_REPO}/${PKG_NAME}.el ${PKG_PATH}/
fi
PKG_MAIN=$(find ${PKG_REPO} -name ${PKG_NAME}-pkg.el || true)
if [ -s "${PKG_REPO}/LICENSE" ]; then
  cp -p "${PKG_REPO}/LICENSE" ${PKG_PATH}  #necessary anymore?
fi

cd melpazoid-master
${PYTHON} -m pip install --user -U .
sed -i -e 's/ -it / -i /' Makefile
sed -i -e 's/ -ti / -i /' Makefile
if [ ! -s ./python ]; then rm -f ./python ; ln -s ${PYTHON} ./python ; fi

cd ${ROOT}
CASK_PACKAGES=$(cask package-directory)
if [ ! -z "${GITHUB_ACTIONS:-}" ]; then
    mkdir -p $HOME/.emacs.d/elpa
    rsync -a $HOME/.emacs.d/elpa/ ${CASK_PACKAGES}/   # pull gha cache, if any
fi

ARTIFACT="/dev/null"
if [ ! -z "${GITHUB_ACTIONS:-}" ]; then
    mkdir -p /var/tmp/mz.artifact
    >/var/tmp/mz.artifact/mz.number basename $(dirname $GITHUB_REF)
    ARTIFACT="/var/tmp/mz.artifact/mz.out"
fi

PACKAGE_MAIN=${PKG_MAIN} EMACS=${EMACS} cask emacs -Q --batch -l package \
  --eval "(customize-set-variable 'package-user-dir \"${CASK_PACKAGES}\")" \
  -L ${MZDIR}/melpazoid-master/melpazoid \
  -f package-initialize \
  --eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
  --eval "(package-refresh-contents)" \
  --eval "(package-install (quote pkg-info))" \
  --eval "(package-install (quote package-lint))" \
  --eval "(package-install-file \"${PKG_TAR}\")" \
  --eval "(setq debug-on-error t)" \
  --eval "(setq default-directory \"${PKG_PATH}\")" \
  -l melpazoid | tee ${ARTIFACT}
if [ ! -z "${GITHUB_ACTIONS:-}" ]; then
    rsync -a ${CASK_PACKAGES}/ $HOME/.emacs.d/elpa/   # push gha cache
fi
