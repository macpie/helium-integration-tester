FROM erlang:24.1

WORKDIR /opt/hit

ADD rebar3 rebar3
ADD rebar.config rebar.config
ADD rebar.lock rebar.lock
RUN ./rebar3 get-deps
RUN ./rebar3 compile

ADD Makefile Makefile
ADD src/ src/
RUN make

ADD config/ config/

RUN make rel
# add hit to path for easy interactions
ENV PATH=$PATH:_build/default/rel/hit/bin

CMD ["make", "run"]
