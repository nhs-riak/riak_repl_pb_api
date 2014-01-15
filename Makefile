.PHONY: compile deps clean

all: deps compile test

compile:
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean


include tools.mk
