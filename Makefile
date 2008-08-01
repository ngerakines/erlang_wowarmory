all: code

code: clean
	erlc src/armory.erl

clean:
	rm -fv *.beam *.rel *.script *.boot erl_crash.dump usr

package-debian:
	mkdir -p usr/lib/erlang/lib/armory-0.1/ebin/ && cp armory.beam usr/lib/erlang/lib/armory-0.1/ebin/armory.beam
	dpkg -b
