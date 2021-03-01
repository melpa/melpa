#!/bin/bash -x

# Modified from gonewest818 https://github.com/clojure-emacs/cider/pull/2139

WORKDIR=${HOME}/local
CASKDIR=$WORKDIR/cask

update_elpa_keys() {
    mkdir -p $HOME/.emacs.d/elpa/gnupg || true
    chmod 700 $HOME/.emacs.d/elpa/gnupg
    GPG=gpg
    if which gpg2 ; then
        GPG=gpg2
    fi
    for i in 1 2 3 ; do
        if ${GPG} -q --homedir $HOME/.emacs.d/elpa/gnupg -k | grep 81E42C40 ; then
            return 0
        fi
        if [ $i -gt 1 ] ; then
            sleep 5
        fi
        ${GPG} --keyserver hkp://ipv4.pool.sks-keyservers.net --homedir $HOME/.emacs.d/elpa/gnupg --recv-keys 066DAFCB81E42C40
    done
    return 1
}

copy_keys() {
    mkdir -p $(cask package-directory) || true
    mkdir -p $HOME/.cask || true
    rsync -azSHe ssh $HOME/.cask $(dirname $(dirname $(dirname $(cask package-directory))))
    rsync -azSHe ssh $HOME/.emacs.d/elpa/gnupg $(cask package-directory)
}

cask_upgrade_cask_or_reset() {
    cask upgrade-cask || { rm -rf $HOME/.emacs.d/.cask && false; }
}

cask_install_or_reset() {
    cask install </dev/null
    find $(cask package-directory)/archives -print | xargs ls -l
    find $(cask package-directory)/gnupg -print | xargs ls -l

    # travis cache
    rsync -azSHe ssh $(dirname $(dirname $(cask package-directory))) $HOME/
}

# Bootstrap the cask tool and its dependencies
if [ ! -d $CASKDIR ] ; then
    git clone https://github.com/cask/cask.git $CASKDIR
fi

update_elpa_keys
copy_keys
cask_upgrade_cask_or_reset
cask_install_or_reset
