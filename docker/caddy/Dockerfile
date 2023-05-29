FROM caddy:2.6-builder AS builder
RUN xcaddy build \
  --with github.com/caddyserver/transform-encoder \
  --with github.com/caddyserver/cache-handler

FROM caddy:2.6
COPY --from=builder /usr/bin/caddy /usr/bin/caddy

RUN apk add --no-cache certbot logrotate

# COPY Caddyfile /etc/caddy/Caddyfile
COPY logrotate /etc/logrotate

WORKDIR /mnt/store/melpa
CMD docker/caddy/run.sh
