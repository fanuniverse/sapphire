FROM debian:stretch-slim

RUN apt-get update \
 && apt-get install -y netbase libgmp10 \
 && rm -rfv /var/lib/apt/lists/*

COPY .stack-work/install/x86_64-linux*/lts-8.*/8.*/bin/sapphire .

USER nobody

CMD ./sapphire
