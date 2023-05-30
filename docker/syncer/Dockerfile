FROM debian:bookworm
RUN apt-get update

RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install locales emacs-nox curl git make mercurial ruby jq miller && \
  apt-get clean && rm -rf /var/lib/apt/lists/

RUN locale-gen --no-purge C.UTF-8
ENV LANG C.UTF-8
WORKDIR /mnt/store/melpa
CMD docker/syncer/run.sh
