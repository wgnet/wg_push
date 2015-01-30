build:
	erlc apns_emulator.erl

start-emulator: build
	erl -detached \
	-name apns_emulator@127.0.0.1 \
	-setcookie apns \
	-s apns_emulator start

stop-emulator:
	erl -noshell \
	-name stop_apns_emulator@127.0.0.1 \
	-setcookie apns \
	-eval "rpc:call('apns_emulator@127.0.0.1', init, stop, [])." -s init stop

run:
	erl -s apns_emulator start
