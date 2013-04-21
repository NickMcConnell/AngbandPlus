CC = gcc

#ENVIRONMENT=GCU
ENVIRONMENT=X11
#ENVIRONMENT=GTK

BASEOBJS = z-form.o z-rand.o z-term.o z-util.o z-virt.o collected.o variable.o util.o #init2.o
BASEDEFINES= -DCMUCL=1

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

%.o : %.c
	$(CC) $(CFLAGS) -c $<


all: $(OBJS)
	$(CC) -shared -o liblang_ui.so $(OBJS) $(LIBS)

clean:
	$(RM) *.o *.so *.fasl

