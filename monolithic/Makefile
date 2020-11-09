all: tests

tests:
	dune runtest .

clean:
	$(RM) *.cmo *.cmi

indent:
	ocp-indent -i *.ml

distclean: clean
	$(RM) a.out

