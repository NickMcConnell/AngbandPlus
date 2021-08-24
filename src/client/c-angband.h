/*
 * File: c-angband.h
 * Purpose: Main Angband header file (client)
 */

#ifndef INCLUDED_C_ANGBAND_H
#define INCLUDED_C_ANGBAND_H

/*
 * Include the low-level includes
 */
#include "../common/angband.h"

/*
 * Include the mid-level includes
 */
#include "cmd-core.h"
#include "ui-event.h"
#include "ui-term.h"

/*
 * Include the high-level includes
 */
#include "c-cmds.h"
#include "c-option.h"
#include "c-player.h"
#include "conf.h"
#include "game-event.h"
#include "game-input.h"
#include "grafmode.h"
#include "main.h"
#include "netclient.h"
#include "player-properties.h"
#include "sound.h"
#include "snd-sdl.h"
#include "snd-win.h"
#include "ui-birth.h"
#include "ui-command.h"
#include "ui-context.h"
#include "ui-curse.h"
#include "ui-death.h"
#include "ui-display.h"
#include "ui-game.h"
#include "ui-init.h"
#include "ui-input.h"
#include "ui-keymap.h"
#include "ui-knowledge.h"
#include "ui-menu.h"
#include "ui-message.h"
#include "ui-object.h"
#include "ui-options.h"
#include "ui-output.h"
#include "ui-prefs.h"
#include "ui-spell.h"
#include "ui-store.h"

#endif
