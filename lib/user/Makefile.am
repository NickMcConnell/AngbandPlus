## Makefile.am -- Process this file with automake to process Makefile.in

angbanddir = $(datadir)/games/$(PACKAGE)/user

install-data-hook:
	mkdir -p "$(DESTDIR)$(angbanddir)"
	chown -R root:games "$(DESTDIR)$(angbanddir)"
	chmod -R 775 "$(DESTDIR)$(angbanddir)"
