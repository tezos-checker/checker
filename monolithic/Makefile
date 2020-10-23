all: tests

run:
	dune exec ./script.exe

tests:
	dune runtest .

clean:
	$(RM) *.cmo *.cmi

indent:
	ocp-indent -i *.ml

distclean: clean
	$(RM) a.out

