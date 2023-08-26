FROM debian:bookworm-slim
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install python3 python3-pip
RUN python3 -m pip install --no-cache duckdb==0.8.1
WORKDIR /mnt/store/melpa
CMD docker/logprocessor/run.sh
