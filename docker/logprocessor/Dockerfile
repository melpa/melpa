FROM debian:bookworm
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install python3
WORKDIR /mnt/store/melpa
CMD docker/logprocessor/run.sh
