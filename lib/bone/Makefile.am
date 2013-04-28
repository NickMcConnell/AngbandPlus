## Makefile.am -- Process this file with automake to process Makefile.in

angbanddir = $(datadir)/games/$(PACKAGE)/bone

install-data-hook:
	mkdir -p "$(DESTDIR)$(angbanddir)"
	chown -R root:games "$(DESTDIR)$(angbanddir)"
	chmod -R 070 "$(DESTDIR)$(angbanddir)"
