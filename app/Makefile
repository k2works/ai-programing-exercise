ERLC_FLAGS = -I include +warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +warn_deprecated_function

all: compile

compile:
	erlc $(ERLC_FLAGS) fizzbuzz.erl
	erlc $(ERLC_FLAGS) fizzbuzz_tests.erl

test: compile
	erl -noshell -eval "eunit:test(fizzbuzz_tests), halt()."

clean:
	rm -f *.beam

.PHONY: all compile test clean