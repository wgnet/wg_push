compile:
	rebar compile

clean:
	rebar clean
	rm -f erl_crash.dump

eunit:
	rebar eunit skip_deps=true

ct:
	rebar ct skip_deps=true

d:
	dialyzer --src -I include src

run:
	erl -pa ebin -s wg_push_app start
