FROM haskell:8.10.4 AS builder
COPY . /home/src
WORKDIR /home/src

RUN stack setup && \
    stack install IMP-lang

FROM alpine:latest
COPY --from=builder /root/.local/bin/IMP-lang-exe /app/imp

ENTRYPOINT [ "/app/imp" ]
