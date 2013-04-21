## ---- Start simple customisation

# what gui environments do you want
# (currently only GCU, X11 and GTK supported)
ENVIRONMENTS=X11 GCU


# set this variable to wanted lisp-system
# should not bee needed anymore
#LISP_SYSTEM=CMUCL
#LISP_SYSTEM=ACL
#LISP_SYSTEM=CLISP


# do you want to use experimental sound
#SOUND_USE=YES
SOUND_USE=NO


## ---- End simple customisation

CC = gcc
BASEOBJS = z-form.o z-rand.o z-term.o z-util.o z-virt.o collected.o variable.o util.o lisp_if.o #init2.o
UIOBJS=
UIDEFINES=
UIFLAGS=
BASEDEFINES=
BASEFLAGS=-Wall -W -g

#ifeq ($(LISP_SYSTEM),CMUCL)
#BASEDEFINES= -DUSE_CMUCL=1
#endif

#ifeq ($(LISP_SYSTEM),ACL)
#BASEDEFINES= -DUSE_ACL=1
#endif

#ifeq ($(LISP_SYSTEM),CLISP)
#BASEDEFINES= -DUSE_CLISP=1
#endif




# GCU settings
ifeq ($(findstring GCU,$(ENVIRONMENTS)), GCU)
UIDEFINES += -D"USE_GCU" -D"USE_NCURSES" $(BASEDEFINES)
UIOBJS += main-gcu.o
UILIBS += -lncurses
endif

# X11 Settings
ifeq ($(findstring X11,$(ENVIRONMENTS)), X11)
UIDEFINES += -D"USE_X11" 
UIOBJS += main-x11.o
UILIBS += -L/usr/X11R6/lib -lX11
endif

#GTK Settings
ifeq ($(findstring GTK,$(ENVIRONMENTS)), GTK)
UIDEFINES = -D"USE_GTK" 
UIFLAGS = += `gtk-config --cflags`
UIOBJS += main-gtk.o
UILIBS += `gtk-config --libs`
endif

DEFINES=$(UIDEFINES) $(BASEDEFINES)
CFLAGS= $(BASEFLAGS) $(DEFINES) $(UIFLAGS)
OBJS=$(BASEOBJS) $(UIOBJS)
LIBS = $(UILIBS)

DAEMONOBJ=asdaemon.o
DAEMONPROG=

ifeq ($(SOUND_USE),YES)
DEFINES += -D"USE_SOUND"
DAEMONPROG = sound_daemon
OBJS += use_sound.o
endif

PICFLAG=-fPIC

%.o : %.c
	$(CC) $(CFLAGS) $(PICFLAG) -c $<

ifneq ("$(strip $(ENVIRONMENTS))","")
all: shared_objs
else
all:;@echo 'Please specify environments'
endif

shared_objs: $(OBJS) $(DAEMONPROG) dummy_call.o
	$(CC) -shared -o liblangband_ui.so $(OBJS) $(LIBS)
	$(CC) -shared -o liblangband_dc.so dummy_call.o

sound_daemon:$(DAEMONOBJ)
	$(CC) -o $@ $(DAEMONOBJ)

clean:
	$(RM) *.o *.so *.fasl

