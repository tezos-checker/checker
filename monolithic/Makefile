all: tests

run:
	dune exec ./script.exe

tests:
	dune exec ./tests.exe

clean:
	$(RM) *.cmo *.cmi

indent:
	ocp-indent -i *.ml

distclean: clean
	$(RM) a.out

