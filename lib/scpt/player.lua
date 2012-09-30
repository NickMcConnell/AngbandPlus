------------------------------------------------------------------------------
----------------------- Hook to create birth objects -------------------------
------------------------------------------------------------------------------
function __birth_hook_objects()
	-- Provide a book of blink to rangers
	if get_class_name() == "Ranger" then
		local obj = create_object(TV_BOOK, 255);
		obj.pval = find_spell("Phase Door")
		inven_carry(obj, FALSE)
		end_object(obj)
	end

	-- Provide a book of Geyser to Geomancers
	if get_class_name() == "Geomancer" then
		local obj = create_object(TV_BOOK, 255);
		obj.pval = find_spell("Geyser")
		inven_carry(obj, FALSE)
		end_object(obj)
	end

	-- Provide a book of prayer to priests
	if get_class_name() == "Priest(Eru)" then
		local obj = create_object(TV_BOOK, 255);
		obj.pval = find_spell("See the Music")
		inven_carry(obj, FALSE)
		end_object(obj)
	end
	if get_class_name() == "Priest(Manwe)" then
		local obj = create_object(TV_BOOK, 255);
		obj.pval = find_spell("Manwe's Blessing")
		inven_carry(obj, FALSE)
		end_object(obj)
	end
	if get_class_name() == "Druid" then
		local obj = create_object(TV_BOOK, 255);
		obj.pval = find_spell("Charm Animal")
		inven_carry(obj, FALSE)
		end_object(obj)
	end
	if get_class_name() == "Dark-Priest" then
		local obj = create_object(TV_BOOK, 255);
		obj.pval = find_spell("Curse")
		inven_carry(obj, FALSE)
		end_object(obj)
	end
	if get_class_name() == "Paladin" then
		local obj = create_object(TV_BOOK, 255);
		obj.pval = find_spell("Divine Aim")
		inven_carry(obj, FALSE)
		end_object(obj)
	end

	-- Start the undeads, as undeads with the corruptions
	if get_subrace_name() == "Vampire" then
		player.corruption(CORRUPT_VAMPIRE_TEETH, TRUE)
		player.corruption(CORRUPT_VAMPIRE_STRENGTH, TRUE)
		player.corruption(CORRUPT_VAMPIRE_VAMPIRE, TRUE)
	end
end

-- Register in the hook list
add_hook_script(HOOK_BIRTH_OBJECTS, "__birth_hook_objects", "__birth_hook_objects")
