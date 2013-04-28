set loc en

# Window title
::msgcat::mcset $loc title Prf


::msgcat::mcset $loc tab-macro Macros
::msgcat::mcset $loc tab-keymap Keymaps

::msgcat::mcset $loc Trigger:
::msgcat::mcset $loc Action:
::msgcat::mcset $loc status-new "Create a new macro or keymap."
::msgcat::mcset $loc status-delete "Delete selected macros or keymaps."
::msgcat::mcset $loc status-trigger "Type a new trigger for the selected item."
::msgcat::mcset $loc status-action "Type an encoded action and hit Enter."
::msgcat::mcset $loc Trigger
::msgcat::mcset $loc Action

::msgcat::mcset $loc Prf
::msgcat::mcset $loc "New Prf File..."
::msgcat::mcset $loc "Save Selected File"
::msgcat::mcset $loc "Save Modified Files"
::msgcat::mcset $loc "Revert Selected File"
::msgcat::mcset $loc "Rename Selected File"
::msgcat::mcset $loc "Remove Selected File"
::msgcat::mcset $loc "Display Load Order"
::msgcat::mcset $loc Edit
::msgcat::mcset $loc Cut
::msgcat::mcset $loc Copy
::msgcat::mcset $loc Paste
::msgcat::mcset $loc Delete
::msgcat::mcset $loc "Select All"

::msgcat::mcset $loc title-new "File Not Created"
::msgcat::mcset $loc msg-new "Prf files must be saved in the lib\\pref or lib\\user directories."

::msgcat::mcset $loc title-extension "Invalid File Name"
::msgcat::mcset $loc msg-extension "Prf files must be have the \".prf\" extension."

::msgcat::mcset $loc title-revert "Revert To Saved"
::msgcat::mcset $loc msg-revert "Really revert \"%s\"?\nAll unsaved changes will be lost."

::msgcat::mcset $loc Rename
::msgcat::mcset $loc title-rename "Rename File"
::msgcat::mcset $loc prompt-rename Name

::msgcat::mcset $loc title-remove "Remove File"
::msgcat::mcset $loc msg-remove "Really remove \"%s\"?\nIt will be deleted from your hard drive."

::msgcat::mcset $loc title-modified "Save Changes"
::msgcat::mcset $loc msg-modified "Save changes to modified files?\nYou can save the changes later if you want."

::msgcat::mcset $loc Macro
::msgcat::mcset $loc "New Macro"
::msgcat::mcset $loc "Remove Duplicates"
::msgcat::mcset $loc "Load Selected Macros"
::msgcat::mcset $loc "Load All Macros"
::msgcat::mcset $loc "Display Loaded Macros"
::msgcat::mcset $loc E_MACRO_AUTOLOAD "Auto-load Edited Macro"
::msgcat::mcset $loc "%d macros"
::msgcat::mcset $loc "1 macro"

::msgcat::mcset $loc Keymap
::msgcat::mcset $loc "New Keymap"
::msgcat::mcset $loc "Load Selected Keymaps"
::msgcat::mcset $loc "Load All Keymaps"
::msgcat::mcset $loc "Display Loaded Keymaps"
::msgcat::mcset $loc E_KEYMAP_AUTOLOAD "Auto-load Edited Keymap"
::msgcat::mcset $loc "%d keymaps"
::msgcat::mcset $loc "1 keymap"
::msgcat::mcset $loc "Rogue-like Keyset"

::msgcat::mcset $loc Alt+
::msgcat::mcset $loc Ctrl+
::msgcat::mcset $loc Shift+

::msgcat::mcset $loc title-order "Prf Load Order"
::msgcat::mcset $loc msg-order \
	"This list shows the order that .prf files are loaded in when the game starts.\
	You can see which files are included by other files. Some of the files may not really exist."

::msgcat::mcset $loc title-loaded "Loaded Macros & Keymaps"

