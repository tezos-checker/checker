all: compile

compile: entrypoint.tz storage.tz

entrypoint.tz: entrypoint.mligo
	ligo compile-contract entrypoint.mligo main > entrypoint.tz

storage.tz: entrypoint.mligo
	ligo compile-storage entrypoint.mligo main initial_storage > storage.tz
