all: code

code: clean
	erlc src/armory.erl

clean:
	rm -rfv *.beam erl_crash.dump

dist-src: clean
	tar zcf erlang_wowarmory-0.4.tgz Makefile src/
