#-------------------------------------------------
#
# Project created by QtCreator 2013-12-25T17:24:56
#
#-------------------------------------------------

QT       += core gui

QT += widgets

TARGET = NPPGAMES


SOURCES +=  src/qt_main.cpp \
            src/qt_mainwindow.cpp \
            src/qt_commands.cpp  \
            src/qt_main_grid_cursor.cpp \
            src/qt_win_char_equipment.cpp \
            src/qt_win_char_info_equip.cpp \
            src/qt_win_char_info_basic.cpp \
            src/qt_win_char_inventory.cpp \
            src/qt_win_dun_map.cpp \
            src/qt_win_dun_overhead.cpp \
            src/qt_win_feat_recall.cpp \
            src/qt_win_messages.cpp \
            src/qt_win_mon_list.cpp \
            src/qt_win_mon_recall.cpp \
            src/qt_win_obj_list.cpp \
            src/qt_win_obj_recall.cpp \
            src/qt_sidebar.cpp \
            src/qt_statusbar.cpp \
            src/qt_targeting.cpp \
            src/qt_ui_functions.cpp \
            src/calcs.cpp \
            src/cmd_actions.cpp \
            src/cmd_misc.cpp \
            src/cmd_objects.cpp \
            src/cmd_pickup.cpp \
            src/cmd_traps.cpp \
            src/cmd_spell.cpp \
            src/dungeon.cpp \
            src/dun_cave.cpp \
            src/dun_classes.cpp \
            src/dun_effect.cpp \
            src/dun_feat_info.cpp \
            src/dun_feature.cpp \
            src/dun_generate.cpp \
            src/dun_process.cpp \
            src/emitter.cpp \
            src/file_output.cpp \
            src/globals.cpp \
            src/griddialog.cpp \
            src/help.cpp \
            src/hotkeys.cpp \
            src/init_edit_files.cpp\
            src/init_game.cpp\
            src/knowledge.cpp\
            src/knowledge_monsters.cpp \
            src/knowledge_objects.cpp \
            src/knowledge_terrain.cpp \
            src/load.cpp \
            src/messages.cpp \
            src/mon_attack.cpp \
            src/mon_cast.cpp \
            src/mon_classes.cpp \
            src/mon_damage.cpp \
            src/mon_info.cpp \
            src/mon_move.cpp \
            src/mon_player_ghost.cpp \
            src/mon_process.cpp \
            src/mon_ranged_attacks.cpp \
            src/mon_util.cpp \
            src/nppdialog.cpp \
            src/object_all_menu.cpp \
            src/object_classes.cpp \
            src/object_desc.cpp \
            src/object_dialog.cpp \
            src/object_hooks.cpp \
            src/object_info.cpp \
            src/object_make.cpp \
            src/object_select.cpp \
            src/object_settings.cpp \
            src/object_use.cpp \
            src/object_util.cpp \
            src/optionsdialog.cpp \
            src/package.cpp \
            src/pathfind.cpp \
            src/player_attack.cpp \
            src/player_birth.cpp \
            src/player_birth_aux.cpp \
            src/player_death.cpp \
            src/player_classes.cpp \
            src/player_command.cpp \
            src/player_process.cpp \
            src/player_scores.cpp \
            src/player_screen.cpp \
            src/player_spell.cpp \
            src/player_util.cpp \          
            src/project.cpp \
            src/project_util.cpp \
            src/quest.cpp \
            src/quest_process.cpp \
            src/randart.cpp \
            src/random_numbers.cpp \
            src/save.cpp \
            src/sound.cpp \
            src/spells_detect.cpp \
            src/spells_misc.cpp \
            src/spoilers.cpp \
            src/squelch.cpp \
            src/store.cpp \
            src/storedialog.cpp \
            src/tables.cpp \
            src/target.cpp \
            src/tilebag.cpp \
            src/timed.cpp \
            src/utilities.cpp \
            src/wizard_mode.cpp


HEADERS  += src/qt_mainwindow.h\
            src/npp.h\
            src/defines.h\
            src/globals.h \
            src/init.h \
            src/monster.h \
            src/object.h \
            src/player.h \
            src/store.h \
            src/structures.h \
            src/terrain.h \
            src/random_numbers.h \
            src/function_declarations.h \
            src/cmds.h \
            src/command_list.h \
            src/dun_classes.h \
            src/dun_generate.h \
            src/dun_traps.h \
            src/emitter.h \
            src/griddialog.h \
            src/help.h \
            src/hotkeys.h \
            src/knowledge.h\
            src/loadsave.h \
            src/messages.h \
            src/mon_classes.h \
            src/nppdialog.h \
            src/object_all_menu.h \
            src/object_classes.h \
            src/object_dialog.h \
            src/object_select.h \
            src/object_settings.h \
            src/optionsdialog.h \
            src/package.h \
            src/player_birth.h \
            src/player_classes.h \
            src/player_command.h \
            src/player_death.h \
            src/player_scores.h \
            src/player_screen.h \
            src/randart.h \
            src/squelch.h \
            src/spells.h \
            src/storedialog.h \
            src/tilebag.h \
            src/user_macros.h \
            src/utilities.h \
            src/wizard_mode.h


RESOURCES += \
    NPP_Resources.qrc

DESTDIR = ../NPPQT

RC_FILE = lib/icons/nppicon.rc
