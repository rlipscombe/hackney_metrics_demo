REBAR3 ?= rebar3

all:
	$(REBAR3) get-deps
	$(REBAR3) compile

shell:
	$(REBAR3) shell
