FROM debian:bookworm

RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install curl emacs-nox git make mercurial ruby texinfo imagemagick jq && \
        apt-get clean && rm -rf /var/lib/apt/lists/

WORKDIR /mnt/store/melpa
CMD docker/builder/run.sh
