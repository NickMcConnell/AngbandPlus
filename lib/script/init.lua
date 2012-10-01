-- Boolean constants definitions
FALSE=false
TRUE=true

-- Redirect error messages to Angband's msg_print()
_ALERT = function(text)
	msg_print(text)
end

_TRACEBACK = function(text)
	msg_print(text)
	msg_print(debug.traceback())
end

-- Load the modules
script_do_file(angband.build_script_path("event.lua"))
script_do_file(angband.build_script_path("combat.lua"))
script_do_file(angband.build_script_path("scripts.lua"))
script_do_file(angband.build_script_path("passive.lua"))
script_do_file(angband.build_script_path("abilities.lua"))
