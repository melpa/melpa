FROM debian:bullseye
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install emacs-nox git make mercurial ruby
WORKDIR /mnt/store/melpa
CMD docker/syncer/run.sh
