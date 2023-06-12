FROM debian:bullseye
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install locales emacs-nox curl git make mercurial ruby jq
RUN curl -ssLO https://github.com/johnkerl/miller/releases/download/v6.8.0/miller-6.8.0-linux-amd64.deb \
    && apt-get install ./*.deb \
    && rm ./*.deb

RUN locale-gen --no-purge C.UTF-8
ENV LANG C.UTF-8
WORKDIR /mnt/store/melpa
CMD docker/syncer/run.sh
