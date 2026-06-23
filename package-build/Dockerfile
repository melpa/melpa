FROM alpine:latest
RUN apk add bash emacs-nox font-dejavu git imagemagick make mercurial tar texinfo
COPY package-*.el package-build.mk /builder/
ENTRYPOINT ["make", "-f", "/builder/package-build.mk", "action-setup"]
