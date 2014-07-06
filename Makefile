APP=proxy

all: compile xref eunit dialyze

init:
	@eval "if ! [ -f 'src/${APP}.app.src' ]; then ./rebar create-app appid=${APP}; fi"
	@./rebar get-deps compile

compile:
	@./rebar compile

xref:
	@./rebar xref

clean:
	@./rebar clean
	@rm -f .dialyze.plt

eunit:
	@./rebar eunit

edoc:
	@./rebar doc

start: compile
	@erl -pz ebin deps/*/ebin -eval 'erlang:display({start_app, $(APP), application:ensure_all_started($(APP))}).'

.dialyzer.plt:
	touch .dialyzer.plt
	dialyzer --build_plt --plt .dialyzer.plt --apps erts kernel stdlib

dialyze: .dialyzer.plt compile
	dialyzer --plt .dialyzer.plt -r ebin
