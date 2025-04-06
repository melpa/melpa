# This is identical to "builder", except for the following:
#
# - The user inside docker uses the same UID and GID as the user that
#   ran docker.  That way, newly created files in mounted directories
#   do not end up being owned by root.
#
# - Build the channels specified by the DOCKER_BUILD_CHANNELS envvar,
#   instead of hardcoding "unstable" and "stable".
#
# - Respect INHIBIT_PACKAGE_PULL envvar.
#
# - Don't generate any html files.

FROM debian:bookworm
ARG UID
ARG GID

RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get --yes install curl emacs-nox git make mercurial ruby texinfo imagemagick jq && \
    apt-get clean && rm -rf /var/lib/apt/lists/ && \
    addgroup --gid $GID builder && \
    adduser --uid $UID --gid $GID --disabled-password --gecos "" builder
USER builder
WORKDIR /mnt/store/melpa
CMD docker/builder-ng/run.sh
