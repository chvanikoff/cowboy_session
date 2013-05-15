REBAR=`which rebar || echo ./rebar`

all: deps compile

deps:
	@( $(REBAR) get-deps )

compile:
	@( $(REBAR) compile )

.PHONY: all, deps, compile