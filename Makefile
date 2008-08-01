all: code

code: clean
	erlc src/armory.erl

clean:
	rm -rfv *.beam *.rel *.script *.boot erl_crash.dump erlang_armory/

package-debian: code
	mkdir -p erlang_armory/usr/lib/erlang/lib/armory-0.1/ebin/ && cp armory.beam erlang_armory/usr/lib/erlang/lib/armory-0.1/ebin/armory.beam
	mkdir -p erlang_armory/DEBIAN/ && cp control erlang_armory/DEBIAN/control
	dpkg -b erlang_armory erlang_armory.deb
