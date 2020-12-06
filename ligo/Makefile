all: compile

compile: checker.tz storage.tz

checker.tz: checker.mligo avl.mligo
	ligo compile-contract checker.mligo main > checker.tz

storage.tz: checker.mligo avl.mligo
	ligo compile-storage checker.mligo main initial_storage > storage.tz
