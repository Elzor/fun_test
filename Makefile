REBAR = $(shell pwd)/rebar3
CWD = $(shell pwd)
.PHONY: deps

all: compile

compile:
	$(REBAR) compile

deps:
	$(REBAR) deps

clean:
	$(REBAR) clean

prod:
	$(REBAR) as prod release

dev_rel: compile
	$(REBAR) release

run: dev_rel
	$(REBAR) run

# Test
#   usage examples:
#   make test                      :: Test all
#	make test m=simple_test        :: Test module
test: compile
	$(REBAR) as test eunit --module=$(m) --suite=$(s) skip_deps=true