
PREFIX:=../

REBAR=rebar3

.PHONY: all edoc clean test dialyzer

all:
	@$(REBAR) compile

edoc:
	@$(REBAR) edoc

test:
	@$(REBAR) test

clean:
	@$(REBAR) clean

dialyzer:
	@$(REBAR) dialyzer || $(REBAR) dialyzer

xref:
	@$(REBAR) xref

