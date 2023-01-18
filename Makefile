REBAR3 ?= rebar3

all:
	$(REBAR3) get-deps
	$(REBAR3) compile

# rebar3 shell will fetch the dependencies and compile everything,
# but it _also_ leaves a live connection to hex.pm.
# Doing the build explicitly avoids that.
shell: all
	$(REBAR3) shell
