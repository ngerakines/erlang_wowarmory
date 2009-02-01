LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`

all: code

code: clean
	erlc src/armory.erl

clean:
	rm -rfv *.beam erl_crash.dump

dist-src: clean
	tar zcf erlang_wowarmory-0.4.tgz Makefile src/


install: all
	mkdir -p ${LIBDIR}/armory/ebin
	for i in *.beam; do install $$i $(LIBDIR)/armory/ebin/$$i ; done
