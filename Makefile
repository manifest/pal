.PHONY: all deps build build-plt dialyze xref test test-total doc clean distclean start

all: build

deps:
	rebar get-deps

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

xref:
	rebar xref skip_deps=true

test: build
	rebar eunit -v skip_deps=true

test-total: test xref dialyze

doc:
	rebar doc skip_deps=true

clean:
	rebar clean

distclean: clean
	rebar delete-deps

start: build
	erl \
		-pa ebin deps/*/ebin \
		-eval 'application:ensure_all_started(pal, permanent)' \
		-boot start_sasl \
		-sasl errlog_type error

