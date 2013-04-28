IndexOne NSAbout about.tcl
IndexOne NSAlternate alternate.tcl
IndexOne NSAsk ask.tcl
IndexOne NSAssign assign.tcl
IndexOne NSAutobar autobar.tcl
if {[llength [angband player spell_book]]} {
	IndexOne NSBookMenu book-menu.tcl
	IndexOne NSBookWindow book-window.tcl
}
if {[variant KANGBANDTK ZANGBANDTK]} {
	IndexOne NSBuilding building.tcl
}
IndexOne NSCharacterWindow character-window.tcl
IndexOne NSCharFlagsCanvas charflags-canvas.tcl
IndexOne NSCharInfoCanvas charinfo-canvas.tcl
IndexOne NSChoiceWindow choice-window.tcl
IndexOne NSChooseMonster choose-monster.tcl
IndexOne NSColorPreferences color.tcl
IndexOne NSControls controls.tcl
IndexOne NSDisplayInfo display-info.tcl
IndexOne NSFont font.tcl
IndexOne NSHelp help-html.tcl
IndexOne NSHelpList help-list.tcl
IndexOne NSHighScore highscore.tcl
IndexOne NSIconWindow icon-window.tcl
IndexOne NSInfoWindow info-window.tcl
IndexOne NSInventory inventory.tcl
IndexOne NSInventory2 inventory2.tcl
IndexOne NSIconBrowser icon-browser.tcl
#IndexOne NSKeymap keymap.tcl
IndexOne NSKnowledge knowledge.tcl
IndexOne NSList stdlist.tcl
#IndexOne NSMacros macros.tcl
IndexOne NSMainWindow main-window.tcl
IndexOne NSMap map.tcl
IndexOne NSMapEditor map-editor.tcl
IndexOne NSMessageHistory message-history.tcl
IndexOne NSMessageWindow message-window.tcl
IndexOne NSMessagesWindow messages-window.tcl
IndexOne NSMicroMapWindow micromap-window.tcl
IndexOne NSMiscCanvas misc-canvas.tcl
IndexOne NSMiscToolbar misc-toolbar.tcl
IndexOne NSMiscWindow misc-window.tcl
IndexOne NSMusic music.tcl
IndexOne NSOptions options.tcl
if {[variant KANGBANDTK]} {
	IndexOne NSPets pets.tcl
}
if {[variant ZANGBANDTK]} {
	if {[string equal [angband player class] Mindcrafter]} {
		IndexOne NSMindcraftMenu mindcraft-menu.tcl
		IndexOne NSMindcraftWindow mindcraft-window.tcl
	}
	IndexOne NSPets pets.tcl
	IndexOne NSPower power.tcl
}
IndexOne NSPhoto photo.tcl
IndexOne NSPhotoWindow photo-window.tcl
IndexOne NSPlayerFlags player-flags.tcl
IndexOne NSPopup popup.tcl
IndexOne NSPrfFile prffile.tcl
IndexOne NSPrfWindow prf-window.tcl
IndexOne NSProgressCanvas progress-canvas.tcl
IndexOne NSProgressWindow progress-window.tcl
IndexOne NSRecord record.tcl
IndexOne NSRecall recall.tcl
IndexOne NSSound sound.tcl
IndexOne NSSprite sprite.tcl
IndexOne NSSquelch squelch.tcl
IndexOne NSStore store.tcl
IndexOne NSStore2 store2.tcl
IndexOne NSStatus status.tcl
IndexOne NSTips tips.tcl
IndexOne NSTomb tomb.tcl
IndexOne NSWidget widget.tcl

