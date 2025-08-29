FROM nginx:bookworm

RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get --yes install certbot openssl logrotate && \
        apt-get clean && rm -rf /var/lib/apt/lists/

COPY default.conf /etc/nginx/conf.d/
COPY logrotate /etc/logrotate
WORKDIR /mnt/store/melpa
CMD docker/nginx/run.sh
