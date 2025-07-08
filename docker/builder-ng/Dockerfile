FROM debian:bookworm
ARG UID
ARG GID

RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get --yes install \
    curl emacs-nox git make mercurial ruby texinfo imagemagick jq \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists \
    && addgroup --gid $GID builder \
    && adduser --uid $UID --gid $GID --disabled-password --gecos "" builder
USER builder
WORKDIR /mnt/store/melpa
CMD docker/builder-ng/run.sh
