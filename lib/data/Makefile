MKPATH=../../mk/
include $(MKPATH)buildsys.mk

PACKAGE = ${PACKAGE_NAME}/data
DATA = delete.me
DATA_ = a_info d_info e_info f_info k_info m_info r_info s_info v_info z_info

install-extra:
	if [ "x$(SETEGID)" != "x" ]; then \
		${MKDIR_P} ${DESTDIR}${datadir}/$(PACKAGE); \
		for i in ${DATA_}; do \
			touch ${DESTDIR}${datadir}/$(PACKAGE)/$${i}.raw; \
		done; \
		chown -R root:${SETEGID} ${DESTDIR}${datadir}/$(PACKAGE); \
		chmod -R g+w ${DESTDIR}${datadir}/$(PACKAGE); \
	fi
