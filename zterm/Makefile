## ---- Start simple customisation

# what gui environments do you want
# (currently only GCU, X11, WIN and GTK supported)

ENVIRONMENTS=X11 GCU
#ENVIRONMENTS=GTK
#ENVIRONMENTS=WIN

# do you want to use experimental sound
#SOUND_USE=YES
SOUND_USE=NO


## ---- End simple customisation

CC = gcc
BASEOBJS = z-rand.o z-term.o z-util.o z-virt.o collected.o \
variable.o util.o lisp_if.o #signals.o #tests.o
UIOBJS=
UIDEFINES=
UIFLAGS=
BASEDEFINES=
BASEFLAGS=-Wall -W -ggdb
DLLEXT=so
PICFLAG=-fPIC



# GCU settings
ifeq ($(findstring GCU,$(ENVIRONMENTS)), GCU)
UIDEFINES += -D"USE_GCU" -D"USE_NCURSES" $(BASEDEFINES)
UIOBJS += main-gcu.o
UILIBS += -lncurses
endif

# X11 Settings
ifeq ($(findstring X11,$(ENVIRONMENTS)), X11)
UIDEFINES += -D"USE_X11" 
UIOBJS += maid-x11.o main-x11.o
UILIBS += -L/usr/X11R6/lib -lX11
endif

#GTK Settings
ifeq ($(findstring GTK,$(ENVIRONMENTS)), GTK)
UIDEFINES = -D"USE_GTK" 
UIFLAGS += `gtk-config --cflags`
UIOBJS += main-gtk.o
UILIBS += `gtk-config --libs`
endif

# hack
ifeq ($(findstring WIN,$(ENVIRONMENTS)), WIN)
UIDEFINES += -D"WIN_MAKEDLL" -D"USE_WIN" -D"WINDOWS" -D"WIN32" #-mno-cygwin
DLLEXT=dll
UIOBJS += dummy_call.o main-win.o #hack
UILIBS += cygwin1.dll -s -mwindows -lwinmm
LINKOPT=#-Wl,--out-implib,libfoo.import.a
PICFLAG=
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


%.o : %.c
	$(CC) $(CFLAGS) $(PICFLAG) -c $<

ifneq ("$(strip $(ENVIRONMENTS))","")
all: shared_objs
else
all:;@echo 'Please specify environments'
endif

shared_objs: $(OBJS) $(DAEMONPROG) dummy_call.o
	$(CC) $(LINKOPT) -shared -o liblangband_ui.$(DLLEXT) $(OBJS) $(LIBS)
	$(CC) $(LINKOPT) -shared -o liblangband_dc.$(DLLEXT) dummy_call.o

sound_daemon:$(DAEMONOBJ)
	$(CC) -o $@ $(DAEMONOBJ)

clean:
	$(RM) *.o *.so *.fasl *.dll *.a *~

