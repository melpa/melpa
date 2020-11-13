# MELPA Services

The following directories contain services that can be run on the
MELPA build server.

To enable a service here you should link the directory into the
`~/service` directory on the build server. This directory is defined
by the *runit* config which can be found in `cfg/etc/runit/melpa/run`
file stored in the repo.

**Note:** if you link to the `run` file directly the scripts will fail
because of the way in which the repo directory is define. This could
easily be resolved but I don't care to make the change because you
should just link to the entire service directory.