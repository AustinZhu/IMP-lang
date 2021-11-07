FROM haskell:8.10.4 AS builder
COPY . /home/src
WORKDIR /home/src

RUN stack install IMP-lang

FROM alpine:latest
COPY --from=builder /home/.local/bin/IMP-lang /app/IMP-lang

ENTRYPOINT ["/app/IMP-lang"]
