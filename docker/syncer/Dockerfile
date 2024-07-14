FROM debian:bookworm

RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install emacs-nox git make mercurial ruby && \
        apt-get clean && rm -rf /var/lib/apt/lists/

WORKDIR /mnt/store/melpa
CMD docker/syncer/run.sh
