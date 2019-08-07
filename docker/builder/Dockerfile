FROM debian:unstable
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install curl emacs25-nox git make mercurial ruby texinfo
WORKDIR /mnt/store/melpa
CMD docker/builder/run.sh
