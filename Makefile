compile:
	rebar compile

clean:
	rebar clean
	rm -f erl_crash.dump

eunit:
	rebar eunit skip_deps=true

run:
	erl -pa ebin -boot start_sasl -s wg_push_app start
