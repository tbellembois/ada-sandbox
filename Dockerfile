FROM debian:bookworm as builder

COPY bin/dockerproxy-main /bin/dockerproxy-main

COPY docker/entrypoint.sh /
RUN chmod +x /entrypoint.sh

USER root
EXPOSE 8080
ENTRYPOINT [ "/entrypoint.sh" ]
