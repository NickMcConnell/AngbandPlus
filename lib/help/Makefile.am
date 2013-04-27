## Makefile.am -- Process this file with automake to process Makefile.in

angband_files = \
	j_feature.txt j_general.txt j_history.txt j_item1.txt \
	j_item2.txt j_trans.txt jeditor.txt jgambling.txt \
	jattack.txt jbirth.txt jcharattr.txt \
	jcommand.txt jdefend.txt jdungeon.txt jgeneral.txt \
	jhelp.hlp jhelpinfo.txt jmonster.txt jobjects.txt \
	joption.txt jpref.txt jspecial.hlp jtown.txt jversion.txt \
	arena.txt bldg.txt gambling.txt \
	attack.txt birth.txt charattr.txt \
	command.txt defend.txt dungeon.txt general.txt \
	help.hlp helpinfo.txt monster.txt objects.txt \
	option.txt pref.txt town.txt version.txt

EXTRA_DIST = \
  $(angband_files)

if SET_GID
angbanddir = @DEFAULT_PATH@help

angband_DATA = \
  $(angband_files)
endif

