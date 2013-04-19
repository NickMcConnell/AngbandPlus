------------------------------------------------------------------------------
----------------------- Hook to create birth objects -------------------------
------------------------------------------------------------------------------

function __birth_hook_objects()

	-- Grace delay for adding piety
	GRACE_DELAY = 0

	-- Provide a book of Geyser to Geomancers
	if get_class_name() == "Geomancer" then
		local obj = create_object(TV_BOOK, 255);
		obj.pval = find_spell("Geyser")
		obj.ident = bor(obj.ident, IDENT_MENTAL, IDENT_KNOWN)
		inven_carry(obj, FALSE)
		end_object(obj)
	end

	-- Provide a book of prayer to priests
	
	if get_class_name() == "Priest(Eru)" then
		local obj = create_object(115, 20);
		obj.ident = bor(obj.ident, IDENT_MENTAL, IDENT_KNOWN)
		inven_carry(obj, FALSE)
		end_object(obj)
	end

	if get_class_name() == "Priest(Manwe)" then
		local obj = create_object(115, 21);
		obj.ident = bor(obj.ident, IDENT_MENTAL, IDENT_KNOWN)
		inven_carry(obj, FALSE)
		end_object(obj)
	end
	
	if get_class_name() == "Druid" then
		local obj = create_object(115, 24);
		obj.ident = bor(obj.ident, IDENT_MENTAL, IDENT_KNOWN)
		inven_carry(obj, FALSE)
		end_object(obj)
	end

	if get_class_name() == "Dark-Priest" then
		local obj = create_object(115, 23);
		obj.ident = bor(obj.ident, IDENT_MENTAL, IDENT_KNOWN)
		inven_carry(obj, FALSE)
		end_object(obj)
	end
	if get_class_name() == "Paladin" then
		local obj = create_object(115, 22);
		obj.ident = bor(obj.ident, IDENT_MENTAL, IDENT_KNOWN)
		inven_carry(obj, FALSE)
		end_object(obj)
	end
	if get_class_name() == "Stonewright" then
		local obj = create_object(115, 63);
		obj.ident = bor(obj.ident, IDENT_MENTAL, IDENT_KNOWN)
		inven_carry(obj, FALSE)
		end_object(obj)
	end
	if get_class_name() == "Priest(Varda)" then
		local obj = create_object(115, 64);
		obj.ident = bor(obj.ident, IDENT_MENTAL, IDENT_KNOWN)
		inven_carry(obj, FALSE)
		end_object(obj)
	end
	if get_class_name() == "Priest(Ulmo)" then
		local obj = create_object(115, 65);
		obj.ident = bor(obj.ident, IDENT_MENTAL, IDENT_KNOWN)
		inven_carry(obj, FALSE)
		end_object(obj)
	end
	if get_class_name() == "Priest(Mandos)" then
		local obj = create_object(115, 66);
		obj.ident = bor(obj.ident, IDENT_MENTAL, IDENT_KNOWN)
		inven_carry(obj, FALSE)
		end_object(obj)
	end

		-- Mimics get a cloak

	if get_class_name() == "Mimic" then
		local obj = create_object(TV_CLOAK, 100);
		obj.pval2 = resolve_mimic_name("Mouse")
		obj.ident = bor(obj.ident, IDENT_MENTAL, IDENT_KNOWN)
		inven_carry(obj, FALSE)
		end_object(obj)
	end

	-- Start the undeads, as undeads with the corruptions
	if get_subrace_name() == "Vampire" then
		player.corruption(CORRUPT_VAMPIRE_TEETH, TRUE)
		player.corruption(CORRUPT_VAMPIRE_STRENGTH, TRUE)
		player.corruption(CORRUPT_VAMPIRE_VAMPIRE, TRUE)
	end

	

	-- To offset the human's stat differences they can carry more loot.
	if get_race_name() == "Human" then
	local obj = create_object(TV_TOOL, SV_PORTABLE_HOLE);
		inven_carry(obj, FALSE)
		end_object(obj)
		identify_pack_fully()
    end
       
-- Subrace Artifacts for a slightly easier start. Best for new players.

	if get_subrace_name() == "Aragorn" then
		local artifact = create_artifact(164);
		inven_carry(artifact, FALSE) 
		identify_pack_fully()
		local artifact1 = create_artifact(8);
		inven_carry(artifact1, FALSE) 
		identify_pack_fully()
	end
	
	
	if get_subrace_name() == "Pippin" then
		local artifact = create_artifact(184);
		inven_carry(artifact, FALSE) 
		identify_pack_fully()
		local artifact1 = create_artifact(67);
		inven_carry(artifact1, FALSE) 
		identify_pack_fully()
		local artifact2 = create_artifact(165);
		inven_carry(artifact2, FALSE) 
		identify_pack_fully()
	end
  

	if get_subrace_name() == "Frodo" then
		local artifact = create_artifact(88);
		inven_carry(artifact, FALSE) 
		identify_pack_fully()
		local artifact1 = create_artifact(207);
		inven_carry(artifact1, FALSE) 
		identify_pack_fully()
		local artifact2 = create_artifact(1);
		inven_carry(artifact2, FALSE) 
		identify_pack_fully()		
		local artifact3 = create_artifact(13);
		inven_carry(artifact3, FALSE) 
		identify_pack_fully()		
	end
	
	
	if get_subrace_name() == "Gimli" then
		local artifact = create_artifact(180);
		inven_carry(artifact, FALSE) 
		identify_pack_fully()
		local artifact1 = create_artifact(174);
		inven_carry(artifact1, FALSE) 
		identify_pack_fully()		
		local artifact2 = create_artifact(105);
		inven_carry(artifact2, FALSE) 
		identify_pack_fully()		
	end		

   
	if get_subrace_name() == "Legolas" then
		local artifact = create_artifact(239);
		inven_carry(artifact, FALSE) 
		identify_pack_fully()
		local artifact1 = create_artifact(214);
		inven_carry(artifact1, FALSE) 
		identify_pack_fully()		
		local artifact2 = create_artifact(70);
		inven_carry(artifact2, FALSE) 
		identify_pack_fully()		
	end

end

-- For the automagic statgaining
add_hooks
{
 [HOOK_BIRTH_OBJECTS] = function()
    if player.last_rewarded_level >= 1
       then player.last_rewarded_level = 1
      else
      end
   end
}

-- The Fury's Awesome Starting Town Script Hook Thing

add_hooks
{
[HOOK_BIRTH_OBJECTS] = function()

-- Hunters need to be in their own town

if get_class_name() == "Hunter" then
	player.wilderness_y = 3;
	player.wilderness_x = 3;
	
	elseif one_town == TRUE then
	player.wilderness_y = 64;
	player.wilderness_x = 1;


	
-- so do Jedi	
	elseif get_class_name() == "Jedi" then
	quest(1).status = QUEST_STATUS_UNTAKEN
	player.wilderness_y = 36;
	player.wilderness_x = 72;	
	
else
-- Otherwise scan the races and display the town choices
	start_town()

	end
end	
}



-- The Fury's Awesome Starting Town Script Function Thing

function start_town()
	chosen_starting_town = 0
	if (get_race_name() == "Dwarf") or (get_race_name() == "Petty-Dwarf") or (get_race_name() == "Golem") then
	
			table_townchoice_text = { 
			[1] = "Bree, the small town near Hobbiton", 
			[2] = "Helm's Deep, the mighty fortress", 
			[3] = "Khazud-dum, the town deep in Moria. Suggested for advanced players only", 
			} 

			table_townchoice_values = 
			{ 
			[1] = { 21,35,0,0 },	
			[2] = { 46,45,60,90 },
			[3] = { 39,44,0,0 },
			}
			
	elseif (get_race_name() == "RohanKnight") or  (get_race_name() == "Human") then
	
			table_townchoice_text = { 
			[1] = "Bree, the small town near Hobbiton", 
			[2] = "Edoras, the stronghold of the Rohirrim", 
			[3] = "Minas Arnor, the stronghold of Gondor", 
			[4] = "Esgaroth, the Cith of the Lakemen", 
			} 

			table_townchoice_values = 
			{ 
			[1] = { 21,35,0,0 },	
			[2] = { 50,51,29,96 },
			[3] = { 56,60,0,50 },
			[4] = { 18,66,40,40 },
			}
		
	elseif (get_race_name() == "Dunadan") or (get_race_name() == "Half-Elf") then
	
			table_townchoice_text = { 
			[1] = "Bree, the small town near Hobbiton", 
			[2] = "Henneth Annun, the Ranger Outpost", 
			[3] = "Pelagir, the Great Southern City", 
			[4] = "Rivendell, Elrond's Land", 
			} 

			table_townchoice_values = 
			{ 
			[1] = { 21,35,0,0 },	
			[2] = { 51,63,0,0 },
			[3] = { 62,57,0,0 },
			[4] = { 21,48,10,100 },
			}	
		
	elseif (get_race_name() == "High-Elf") or (get_race_name() == "Elf") then
	
			table_townchoice_text = { 
			[1] = "Bree, the small town near Hobbiton", 
			[2] = "Gondolin, the hidden elven fortress", 
			[3] = "Rivendell, Elrond's Land", 
			} 

			table_townchoice_values = 
			{ 
			[1] = { 21,35,0,0 },	
			[2] = { 11,3,0,0 },
			[4] = { 21,48,10,100 },
			}					
		
	elseif get_race_name() == "Hobbit" then
	
			table_townchoice_text = { 
			[1] = "Bree, the small town near Hobbiton", 
			[2] = "Hobbiton, the small town near Bree", 
			
			} 

			table_townchoice_values = 
			{ 
			[1] = { 21,35,0,0 },	
			[2] = { 21,25,0,0 },
			}	
			
	elseif get_race_name() == "Wood-Elf" then
	
			table_townchoice_text = { 
			[1] = "Bree, the small town near Hobbiton", 
			[2] = "Thranduil's Halls, the hidden Wood-Elf realm", 
			[3] = "Rivendell, Elrond's Land", 
			} 

			table_townchoice_values = 
			{ 
			[1] = { 21,35,0,0 },	
			[2] = { 16,61,0,0 },
			[4] = { 21,48,10,100 },
			}	
			
	elseif get_race_name() == "Beorning" then
	
			table_townchoice_text = { 
			[1] = "Bree, the small town near Hobbiton", 
			[2] = "Beorn's Halls", 
			
			} 

			table_townchoice_values = 
			{ 
			[1] = { 21,35,0,0 },	
			[2] = { 19,55,0,0 },
		
			}	
			
			
	else 
		chosen_starting_town = -1
	end


	 selected = 1
	 while chosen_starting_town == 0 do
	 	display_list(12, 0, getn(table_townchoice_text)+1, 50, "Select a starting town:", table_townchoice_text, 1, selected, TERM_L_RED)

			
			ret,c = get_com("Make your choice using the DOWN(2),UP(8) and ENTER keys...",strbyte("a"))

			if c == strbyte("2") and selected < getn(table_townchoice_text) then
				selected = selected + 1
			end
			if c == strbyte("8") and selected > 1 then
				selected = selected - 1
			end


			if c == strbyte("\r") and selected > 0 then
				
				chosen_starting_town = selected
				local t = table_townchoice_values[chosen_starting_town]

					player.wilderness_y = t[1];
					player.wilderness_x = t[2];
						player.oldpy = t[3];
						player.oldpx = t[4];

			end
			
	  end
end



-- Register in the hook list
    --    add_hooks
	add_hook_script(HOOK_BIRTH_OBJECTS, "__birth_hook_objects", "__birth_hook_objects")
