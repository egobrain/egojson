build:
	@rebar compile skip_deps=true

compile:
	@rebar compile

test: build
	@rebar eunit skip_deps=true

.PHONY: deps compile build test
