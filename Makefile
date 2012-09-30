.PHONY: test doc

all: doc
	./rebar update-deps
	./rebar get-deps
	./rebar compile
	./rebar xref skip_deps=true
	./rebar eunit skip_deps=true

doc:
	cd doc; make html

compile:
	./rebar compile skip_deps=true

xref: compile
	./rebar xref skip_deps=true

clean:
	./rebar clean skip_deps=true
	cd doc; make clean

test: xref
	./rebar skip_deps=true eunit

