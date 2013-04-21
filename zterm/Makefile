## ---- Start simple customisation

# what gui environments do you want (only SDL, WIN and GCU supported)
# To use SDL on Win, simply say WIN

#ENVIRONMENTS=GCU SDL
#ENVIRONMENTS=GCU
ENVIRONMENTS=SDL
#ENVIRONMENTS=WIN

# Select one of these, or none  (fix paths further down if necessary)
SOUNDSYSTEM=SDL_MIXER
#SOUNDSYSTEM=OPENAL

## ---- End simple customisation

CC = gcc
#BASEOBJS = z-rand.o z-term.o z-util.o z-virt.o collected.o \
#frame.o variable.o util.o lisp_if.o
BASEOBJS = collected.o frame.o lisp_if.o
UIOBJS=
UIDEFINES=
UIFLAGS=
BASEDEFINES= -D_REENTRANT -DREENTRANT
BASEFLAGS=-Wall -W -ggdb
DLLEXT=so
PICFLAG=-fPIC
CONFIGFILE=../config/settings.cfg
SDLCONFIG=/usr/bin/sdl-config
DLLCOPY=true

#= Truetype setting, select ttf if you have SDL_ttf installed
TTF_FLAG=-D"ALLOW_TTF"
TTF_LIB=-lSDL_ttf

#TTF_FLAG=
#TTF_LIB=
#= end truetype

# GCU settings
ifeq ($(findstring GCU,$(ENVIRONMENTS)), GCU)
UIDEFINES += -D"USE_GCU" -D"USE_NCURSES" $(BASEDEFINES)
UIOBJS += main-gcu.o
UILIBS += -lncurses
endif

#SDL Settings
ifeq ($(findstring SDL,$(ENVIRONMENTS)), SDL)
UIDEFINES += -D"USE_SDL" ${TTF_FLAG} -DDEBUG
BASEFLAGS += `${SDLCONFIG} --cflags`
UIOBJS += sdl-extra.o main-sdl.o 
UILIBS += -lSDL_image ${TTF_LIB} `${SDLCONFIG} --libs`
endif


# hack
ifeq ($(findstring WIN,$(ENVIRONMENTS)), WIN)
UIDEFINES += -D"USE_SDL" -D"WIN_MAKEDLL" -D"WINDOWS" -D"WIN32" -D"DEBUG"
UIFLAGS += -mno-cygwin -ISDL
DLLEXT=dll
UIOBJS += sdl-extra.o main-sdl.o main.o #hack
UILIBS += SDL/SDL.dll SDL/SDL_image.dll
LINKOPT = -mno-cygwin
PICFLAG=
DLLCOPY=/bin/cp -f SDL/*.dll ../
endif

#Sound Settings
ifeq ($(findstring OPENAL,$(SOUNDSYSTEM)), OPENAL)
#OPENALPATH=/usr/local
OPENALPATH=/usr
UIDEFINES += -D"USE_OPENAL" -I$(OPENALPATH)/include
UIOBJS += al-sound.o
UILIBS += -L$(OPENALPATH)/lib -lopenal
endif

ifeq ($(findstring SDL_MIXER,$(SOUNDSYSTEM)), SDL_MIXER)
UIDEFINES += -D"USE_SDL_MIXER"
UIOBJS += sdl-sound.o
ifeq ($(findstring WIN,$(ENVIRONMENTS)), WIN)
UILIBS += SDL/SDL_mixer.dll
else
UILIBS += -lSDL_mixer
endif
endif


DEFINES=$(UIDEFINES) $(BASEDEFINES)
CFLAGS= $(BASEFLAGS) $(DEFINES) $(UIFLAGS)
OBJS=$(BASEOBJS) $(UIOBJS)
LIBS = $(UILIBS)


%.o : %.c
	$(CC) $(CFLAGS) $(PICFLAG) -c $<

ifneq ("$(strip $(ENVIRONMENTS))","")
all: shared_objs copy_dlls
else
all:;@echo 'Please specify environments'
endif

deb-all: deb_shared_objs

deb_shared_objs: $(OBJS) $(DAEMONPROG) dummy_call.o
	$(CC) $(LINKOPT) -shared -Wl,-soname,liblangband_ui.$(DLLEXT) -o liblangband_ui.$(DLLEXT) $(OBJS) $(LIBS)
	$(CC) $(LINKOPT) -shared -Wl,-soname,liblangband_dc.$(DLLEXT) -o liblangband_dc.$(DLLEXT) dummy_call.o

shared_objs: $(OBJS) $(DAEMONPROG) dummy_call.o
	$(CC) $(LINKOPT) -shared -o liblangband_ui.$(DLLEXT) $(OBJS) $(LIBS)
	$(CC) $(LINKOPT) -shared -o liblangband_dc.$(DLLEXT) dummy_call.o


#write_config:
#	@echo "(ENVIRONMENTS ${ENVIRONMENTS})" >> $(CONFIGFILE)

copy_dlls:
	@$(DLLCOPY)

clean:
	$(RM) *.o liblangband*.so *.fasl liblangband*.dll *.a *~

