all: get-deps compile

get-deps:
	./rebar get-deps

compile:
	./rebar compile

run:
	erl -pa ebin/ deps/erlang-serial/ebin/ -eval "application:start(txrx)"

test:
	erl -pa ebin/ deps/erlang-serial/ebin/ -eval "test:init()"


