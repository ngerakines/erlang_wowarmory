LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION=0.4.2

all:
	mkdir -p ebin/
	(cd src;$(MAKE))

test: all
	prove -v t/*.t

clean:
	(cd src;$(MAKE) clean)
	rm -rf erl_crash.dump *.beam *.hrl erlang_wowarmory-$(VERSION).tgz

package: clean
	@mkdir erlang_wowarmory-$(VERSION)/ && cp -rf ebin src support t Makefile README.markdown erlang_wowarmory-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf erlang_wowarmory-$(VERSION).tgz erlang_wowarmory-$(VERSION)
	@rm -rf erlang_wowarmory-$(VERSION)/

install:
	mkdir -p $(prefix)/$(LIBDIR)/erlang_wowarmory-$(VERSION)/ebin
	for i in ebin/*; do install $$i $(prefix)/$(LIBDIR)/erlang_wowarmory-$(VERSION)/$$i ; done
