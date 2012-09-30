all:
	./rebar compile
	./rebar xref
	./rebar eunit

compile:
	./rebar compile

xref: compile
	./rebar xref

clean:
	./rebar clean

test: xref
	./rebar eunit

