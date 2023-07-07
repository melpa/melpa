FROM debian:bookworm
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install python3 && \
        apt-get clean && rm -rf /var/lib/apt/lists/
WORKDIR /mnt/store/melpa
CMD docker/logprocessor/run.sh
