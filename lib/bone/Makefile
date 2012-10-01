MKPATH=../../mk/
include $(MKPATH)buildsys.mk

CONFIG = bone.128 bone.129 bone.130 bone.131 bone.132 \
bone.133 bone.134 bone.135 bone.136 bone.137 bone.138 \
bone.139 bone.140 bone.141 bone.142 bone.143 bone.144 \
bone.145 bone.146 bone.147

PACKAGE = bone

install-extra:
	if [ "x$(SETEGID)" != "x" ]; then \
		if [ "x$(DRY)" = "x" ]; then \
			${MKDIR_P} ${DESTDIR}${vardatadir}${PACKAGE}; \
			chown root:${SETEGID} ${DESTDIR}${vardatadir}${PACKAGE}; \
			chmod 070 ${DESTDIR}${vardatadir}${PACKAGE}; \
		fi; \
	fi
