.PHONY: all deps build build-plt dialyze test test-total doc clean distclean start

all: build

deps:
	rebar get-deps
	rebar update-deps

build:
	rebar compile

build-plt:
	rm -vf .dialyzer_plt
	dialyzer \
		--build_plt \
		--output_plt .dialyzer_plt \
		--apps erts kernel stdlib crypto public_key ssl inets \
		`find deps -d 1 -type d`

dialyze:
	dialyzer \
		--src src \
		--plt .dialyzer_plt \
		--no_native \
		-Werror_handling \
		-Wrace_conditions \
		-Wunmatched_returns 

test: build
	rebar -v skip_deps=true eunit

test-total: dialyze test

doc:
	rebar doc skip_deps=true

clean:
	rebar clean

distclean: clean
	rebar delete-deps

start: build
	erl \
		-pa ebin deps/*/ebin \
		-eval 'application:ensure_all_started(pal)' \
		-boot start_sasl \
		-sasl errlog_type error

