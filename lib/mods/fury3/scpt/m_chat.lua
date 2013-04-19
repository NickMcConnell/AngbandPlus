--[[ Made for Intets Hevn 0.2.3 by Baumog.
     $LastChangedDate: 2004-06-06 13:51:28 +0200 (Sun, 06 Jun 2004) $


     This file contains creature chat lines and 
     other communication-related stuff.

     I think it's better to use chat for common creatures instead of
     using the CAN_SPEAK flag, since you can trigger their chatting
     yourself. 

     More creature interaction coming later (e.g., give an item to a 
     rogue, and it might leave you alone, or even fight for you).
]]

--[[ Hacked by BlackSmurf for aWESoME, ToME and whatever.

     I think it's better to use the CAN_SPEAK flag for common creatures 
     instead of using chat, since you haven't to trigger their chatting
     yourself. But chatting can reveal more specific information...
]]

dog_chat = {
	"says 'Woof!'",
};

cat_chat = {
	"says 'Meow'",
};

bird_chat = {
	"chitters angrily!",
};

beggar_chat = {
	"pleads, 'Can you spare some cutter, me brother?'",
};

urchin_chat = {
	"begs, 'Hey you, give us some money!'",
};

idiot_chat = {
	"says, 'I bet you're just after my magic sticks!'",
};

gin_chat = {
    "says, 'I'm thirsty.'",
};

rogue_chat = {
	"says, 'Just give me all your stuff.'",
};

merchant_chat = {
	"says, 'Hey, you, wanna sell me an amulet?'",
};

woodsman_chat = {
	"says, 'My axe is losing edge!'",
};

chatlist = {
	{ name = "Sparrow", 	chat = bird_chat 	},
	{ name = "Chaffinch", 	chat = bird_chat 	},
	{ name = "Scrawny cat", 	chat = cat_chat 	},
	{ name = "Scruffy little dog", 	chat = dog_chat 	},
	
	{ name = "Mangy-looking leper", 	chat = beggar_chat 	},
	{ name = "Pitiful-looking beggar", 	chat = beggar_chat 	},
	{ name = "Filthy street urchin", 	chat = urchin_chat 	},
	
	{ name = "Boil-covered wretch", 	chat = idiot_chat 	},
	{ name = "Blubbering idiot", 	chat = idiot_chat 	},
	{ name = "Village idiot", 	chat = idiot_chat 	},
	
	{ name = "Singing, happy drunk", 		chat = gin_chat 	},
	{ name = "Mean-looking mercenary", 	chat = gin_chat 	},
	{ name = "Battle-scarred veteran", 	chat = gin_chat 	},
	
	{ name = "Squint-eyed rogue", 	chat = rogue_chat 	}, 
	{ name = "Agent of the black market", 	chat = rogue_chat 	}, 
	{ name = "Aimless-looking merchant", 	chat = merchant_chat 	},
	{ name = "Woodsman", 			chat = woodsman_chat 	},
	

};

function __hook_monster_chat(m_idx)
	m_ptr = monster(m_idx);
	
	for index, value in chatlist do
		if(m_ptr.r_idx ==  test_monster_name(value.name)) then
			msg_print(format("The %s %s", value.name, value.chat[randint(getn(value.chat))]));
			return TRUE
		end
	end
end

add_hook_script(HOOK_CHAT, "__hook_monster_chat", "__hook_monster_chat");