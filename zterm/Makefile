## ---- Start simple customisation

# what gui environments do you want (only SDL, WIN and GCU supported)
# To use SDL on Win, simply say WIN

ENVIRONMENTS=GCU SDL
#ENVIRONMENTS=GCU
#ENVIRONMENTS=SDL
#ENVIRONMENTS=WIN

## ---- End simple customisation

CC = gcc
BASEOBJS = z-rand.o z-term.o z-util.o z-virt.o collected.o \
frame.o variable.o util.o lisp_if.o 
UIOBJS=
UIDEFINES=
UIFLAGS=
BASEDEFINES=
BASEFLAGS=-Wall -W -ggdb
DLLEXT=so
PICFLAG=-fPIC
CONFIGFILE=../config/settings.cfg
SDLCONFIG=/usr/bin/sdl-config
DLLCOPY=/bin/true

# GCU settings
ifeq ($(findstring GCU,$(ENVIRONMENTS)), GCU)
UIDEFINES += -D"USE_GCU" -D"USE_NCURSES" $(BASEDEFINES)
UIOBJS += main-gcu.o
UILIBS += -lncurses
endif

#SDL Settings
ifeq ($(findstring SDL,$(ENVIRONMENTS)), SDL)
UIDEFINES += -D"USE_SDL" -D"ALLOW_TTF" -DDEBUG
BASEFLAGS += `${SDLCONFIG} --cflags`
UIOBJS += sdl-extra.o main-sdl.o 
UILIBS += -lSDL_mixer -lSDL_image -lSDL_ttf `${SDLCONFIG} --libs`
endif


# hack
ifeq ($(findstring WIN,$(ENVIRONMENTS)), WIN)
UIDEFINES += -D"USE_SDL" -D"WIN_MAKEDLL" -D"WINDOWS" -D"WIN32" -D"DEBUG"
UIFLAGS += -mno-cygwin -ISDL
DLLEXT=dll
UIOBJS += sdl-extra.o main-sdl.o main.o #hack
UILIBS += SDL/SDL.dll SDL/SDL_image.dll SDL/SDL_mixer.dll
LINKOPT = -mno-cygwin
PICFLAG=
DLLCOPY=/bin/cp -f SDL/*.dll ../
endif

DEFINES=$(UIDEFINES) $(BASEDEFINES)
CFLAGS= $(BASEFLAGS) $(DEFINES) $(UIFLAGS)
OBJS=$(BASEOBJS) $(UIOBJS)
LIBS = $(UILIBS)


%.o : %.c
	$(CC) $(CFLAGS) $(PICFLAG) -c $<

ifneq ("$(strip $(ENVIRONMENTS))","")
all: shared_objs write_config copy_dlls
else
all:;@echo 'Please specify environments'
endif

shared_objs: $(OBJS) $(DAEMONPROG) dummy_call.o
	$(CC) $(LINKOPT) -shared -o liblangband_ui.$(DLLEXT) $(OBJS) $(LIBS)
	$(CC) $(LINKOPT) -shared -o liblangband_dc.$(DLLEXT) dummy_call.o

write_config:
	@echo "(ENVIRONMENTS ${ENVIRONMENTS})" >> $(CONFIGFILE)

copy_dlls:
	@$(DLLCOPY)

clean:
	$(RM) *.o liblangband*.so *.fasl liblangband*.dll *.a *~

