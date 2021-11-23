.PHONY: compile clean test rel run docker-build docker-run


REBAR=./rebar3

compile:
	$(REBAR) compile
	$(REBAR) format

clean:
	git clean -dXfffffffffff

test:
	$(REBAR) fmt --verbose --check rebar.config
	$(REBAR) fmt --verbose --check "{src,include,test}/**/*.{hrl,erl,app.src}"
	$(REBAR) xref
	$(REBAR) eunit
	$(REBAR) ct
	$(REBAR) dialyzer

rel:
	$(REBAR) release

run:
	_build/default/rel/hit/bin/hit foreground

docker-build:
	docker build --force-rm -t hit:local .

docker-run: 
	docker run --rm -it --init -p 3000:80 --name=helium_hit hit:local

docker-exec: 
	docker exec -it helium_hit _build/default/rel/hit/bin/hit remote_console

# Pass all unknown targets straight to rebar3 (e.g. `make dialyzer`)
%:
	$(REBAR) $@
