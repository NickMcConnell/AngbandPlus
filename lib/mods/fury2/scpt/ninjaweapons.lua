--- Ninja can choose their starting weapon
---Hacked from Zonk's weapon choice script with thanks from Cosmic Gerbil

function __weapon_choice()
	chosen_starting_weapon = 0



	if get_class_name() == "Ninja" then
	
			table_weaponchoice_text = { 
			[1] = "Jutte", 
			[2] = "Wakizashi", 
			[3] = "Shuriken", 
			[4] = "Nunchuks", 
			[5] = "Katana", 
			} 

			table_weaponchoice_values = 
			{ 
			[1] = { 22,31 },	
			[2] = { 23,80 },
			[3] = { 11,30 },
			[4] = { 21,21 },
			[5] = { 23,20 },
			}
 
		
		
			
			--} 
	else 
		chosen_starting_weapon = -1
	end


	 selected = 1
	 while chosen_starting_weapon == 0 do
	 	display_list(12, 0, getn(table_weaponchoice_text)+1, 50, "Select a weapon:", table_weaponchoice_text, 1, selected, TERM_L_RED)

			
			ret,c = get_com("Make your choice using the DOWN(2),UP(8) and ENTER keys...",strbyte("a"))

			if c == strbyte("2") and selected < getn(table_weaponchoice_text) then
				selected = selected + 1
			end
			if c == strbyte("8") and selected > 1 then
				selected = selected - 1
			end


			if c == strbyte("\r") and selected > 0 then
				
				chosen_starting_weapon = selected
				local t = table_weaponchoice_values[chosen_starting_weapon]
				local obj = create_object(t[1], t[2]);
				obj.ident = IDENT_MENTAL
				inven_carry(obj, TRUE)
				end_object(obj)
				identify_pack()
			end
			
	  end
end


add_hook_script(HOOK_BIRTH_OBJECTS, "__weapon_choice","__weapon_choice")