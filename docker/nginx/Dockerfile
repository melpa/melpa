FROM nginx:alpine
RUN apk add --no-cache certbot openssl logrotate
COPY default.conf /etc/nginx/conf.d/
COPY logrotate /etc/logrotate
WORKDIR /mnt/store/melpa
CMD docker/nginx/run.sh
