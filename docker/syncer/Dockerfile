FROM debian:unstable
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install emacs25-nox git make mercurial ruby
WORKDIR /mnt/store/melpa
CMD docker/syncer/run.sh
