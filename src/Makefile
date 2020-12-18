all: tests

tests:
	dune runtest .

clean:
	$(RM) -r _build

indent:
	ocp-indent -i *.ml

docs:
	dune build @doc

distclean: clean

