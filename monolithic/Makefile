
all:
	ocamlc script.ml

run: all
	./a.out

clean:
	$(RM) *.cmo *.cmi

indent:
	ocp-indent -i script.ml

distclean: clean
	$(RM) a.out

