MKPATH=../../mk/
include $(MKPATH)buildsys.mk

PACKAGE = scores

install-extra:
	if [ "x$(SETEGID)" != "x" ]; then \
		if [ "x$(DRY)" = "x" ]; then \
			${MKDIR_P} ${DESTDIR}${vardatadir}${PACKAGE}; \
			chown -R root:${SETEGID} ${DESTDIR}${vardatadir}${PACKAGE}; \
			chmod -R g+w ${DESTDIR}${vardatadir}${PACKAGE}; \
		fi; \
	fi
