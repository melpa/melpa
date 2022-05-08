FROM nginx:alpine
RUN apk add --no-cache rsync
COPY rsyncd.conf /etc/
CMD /usr/bin/rsync --no-detach --daemon --config /etc/rsyncd.conf
