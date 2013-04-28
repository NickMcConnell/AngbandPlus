## Makefile.am -- Process this file with automake to process Makefile.in

angbanddir = $(datadir)/games/$(PACKAGE)/apex

clean-local:
	-rm scores.raw

install-data-hook:
	mkdir -p "$(DESTDIR)$(angbanddir)"
	touch "$(DESTDIR)$(angbanddir)/scores.raw"
	chown -R root:games "$(DESTDIR)$(angbanddir)"
	chmod -R g+w "$(DESTDIR)$(angbanddir)"
