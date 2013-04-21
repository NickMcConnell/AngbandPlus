## ---- Start simple customisation

# what gui environment do you want
#ENVIRONMENT=GCU
ENVIRONMENT=X11
#ENVIRONMENT=GTK


# set this variable to wanted lisp-system
LISP_SYSTEM=CMUCL
#LISP_SYSTEM=ACL
#LISP_SYSTEM=CLISP


# do you want to use experimental sound
#SOUND_USE=YES
SOUND_USE=NO


## ---- End simple customisation

CC = gcc
BASEOBJS = z-form.o z-rand.o z-term.o z-util.o z-virt.o collected.o variable.o util.o #init2.o


ifeq ($(LISP_SYSTEM),CMUCL)
BASEDEFINES= -DUSE_CMUCL=1
endif

ifeq ($(LISP_SYSTEM),ACL)
BASEDEFINES= -DUSE_ACL=1
endif

ifeq ($(LISP_SYSTEM),CLISP)
BASEDEFINES= -DUSE_CLISP=1
endif

# GCU settings
ifeq ($(ENVIRONMENT),GCU)
DEFINES = -D"USE_GCU" -D"USE_NCURSES" $(BASEDEFINES)
CFLAGS = -Wall -W -g $(DEFINES)
OBJS = main-gcu.o $(BASEOBJS)
LIBS = -lncurses
endif

# X11 Settings
ifeq ($(ENVIRONMENT),X11)
DEFINES = -D"USE_X11"  $(BASEDEFINES)
CFLAGS = -Wall -W -g $(DEFINES) 
OBJS = main-x11.o $(BASEOBJS)
LIBS = -L/usr/X11R6/lib -lX11
endif

#GTK Settings
ifeq ($(ENVIRONMENT),GTK)
DEFINES = -D"USE_GTK"  $(BASEDEFINES) 
CFLAGS = -Wall -W -g $(DEFINES) `gtk-config --cflags`
OBJS = main-gtk.o $(BASEOBJS)
LIBS = `gtk-config --libs`
endif

DAEMONOBJ=asdaemon.o
DAEMONPROG=

ifeq ($(SOUND_USE),YES)
DEFINES += -D"USE_SOUND"
DAEMONPROG = sound_daemon
endif

%.o : %.c
	$(CC) $(CFLAGS) -c $<


all: $(OBJS) $(DAEMONPROG)
	$(CC) -shared -o liblang_ui.so $(OBJS) $(LIBS)

sound_daemon:$(DAEMONOBJ)
	$(CC) -o $@ $(DAEMONOBJ)

clean:
	$(RM) *.o *.so *.fasl

