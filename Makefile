MKPATH=mk/
include $(MKPATH)buildsys.mk

SUBDIRS = src lib
CLEAN = config.status config.log *.dll *.exe

.PHONY: dist
TAG = poschengband-`git describe`
OUT = $(TAG).tar.gz

