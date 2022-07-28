FROM erlang:24.3-alpine AS builder

RUN apk add --no-cache --update \
    make autoconf automake bison build-base bzip2 cmake curl \
    dbus-dev flex git gmp-dev libsodium-dev libtool linux-headers lz4 \
    openssl-dev pkgconfig protoc sed tar wget vim

WORKDIR /opt/hit

ADD rebar3 rebar3
ADD rebar.config rebar.config
ADD rebar.lock rebar.lock
RUN ./rebar3 get-deps
RUN make

ADD Makefile Makefile
ADD src/ src/
ADD include/ include/
RUN make

ADD config/ config/
RUN make rel
# add hit to path for easy interactions
ENV PATH=$PATH:_build/default/rel/hit/bin

CMD ["make", "run"]

FROM erlang:24.3-alpine
WORKDIR /opt/hit

# Copy the built release
COPY --from=builder /opt/hit .
# Bring over exe like make
COPY --from=builder /usr/bin /usr/bin

CMD ["make", "run"]
