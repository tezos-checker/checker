all: install-git-hooks tests

tests:
	dune runtest .

clean:
	$(RM) -r _build

indent:
	ocp-indent -i *.ml

docs:
	dune build @doc

distclean: clean

install-git-hooks:
	@[ -x ../.git/hooks/pre-commit ] || (cd ../.git/hooks && rm -f pre-commit && ln -s ../../.pre-commit-hook.sh pre-commit && echo "pre-commit hook installed")

.PHONY: all tests clean indent docs distclean install-git-hooks
