FROM debian:unstable
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install python
WORKDIR /mnt/store/melpa
CMD docker/logprocessor/run.sh
