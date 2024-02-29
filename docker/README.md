# Docker Notes

Use `docker compose` to run the jobs in this directory.

```
docker compose start -d <component>
```

For debugging, you can run bash in a running container,

```
docker compose exec <component> /bin/bash
```


## Setup

- The `daemon.json` file should be installed to `/etc/docker/` to limit the number of logs.


