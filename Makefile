.PHONY: compile deps clean

DIALYZER_APPS = kernel stdlib sasl erts inets crypto

all: deps compile test

include tools.mk

compile:
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

protogen:
	@$(REBAR) --config protogen.config clean
	@$(REBAR) --config protogen.config get-deps
	@$(REBAR) --config protogen.config compile
