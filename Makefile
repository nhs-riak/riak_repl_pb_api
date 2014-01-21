.PHONY: compile deps clean

DIALYZER_APPS = kernel stdlib sasl erts inets crypto

all: deps compile test

compile:
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean


include tools.mk
