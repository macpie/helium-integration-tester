.PHONY: compile clean test rel run docker-build docker-run docker-test docker-exec

REBAR=./rebar3

compile: |
	$(REBAR) compile
	$(REBAR) format

clean:
	git clean -dXfffffffffff

test: |
	$(REBAR) fmt --verbose --check rebar.config
	$(REBAR) fmt --verbose --check "{src,include,test}/**/*.{hrl,erl,app.src}"
	$(REBAR) xref
	$(REBAR) eunit -v
	$(REBAR) ct --readable=true
	$(REBAR) dialyzer

rel:
	$(REBAR) release

run: |
	_build/default/rel/hit/bin/hit foreground

docker-build:
	docker build --force-rm -t helium_integration_tester:local .

docker-test: 
	docker run --rm -it --init --name=helium_integration_tester_test helium_integration_tester:local make test

docker-run: 
	docker run --rm -it --init -p 3000:80 --name=helium_integration_tester helium_integration_tester:local

docker-exec: 
	docker exec -it helium_integration_tester _build/default/rel/hit/bin/hit remote_console

# Pass all unknown targets straight to rebar3 (e.g. `make dialyzer`)
%:
	$(REBAR) $@
