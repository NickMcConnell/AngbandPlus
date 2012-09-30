LUA_PKG = w_quest.c w_mnster.c w_player.c w_z_pack.c w_obj.c \
	  w_util.c w_spells.c w_quest.c w_play_c.c w_dun.c

all: $(LUA_PKG)
	echo -n
        
clean:
	rm -f $(LUA_PKG)

w_mnster.c: monster.pkg
	tolua -n monster -o w_mnster.c monster.pkg

w_player.c: player.pkg
	tolua -n player -o w_player.c player.pkg

w_play_c.c: player_c.pkg
	tolua -n player_c -o w_play_c.c player_c.pkg

w_z_pack.c: z_pack.pkg
	tolua -n z_pack -o w_z_pack.c z_pack.pkg

w_obj.c: object.pkg
	tolua -n object -o w_obj.c object.pkg

w_util.c: util.pkg
	tolua -n util -o w_util.c util.pkg

w_spells.c: spells.pkg
	tolua -n spells -o w_spells.c spells.pkg

w_quest.c: quest.pkg
	tolua -n quest -o w_quest.c quest.pkg

w_dun.c: dungeon.pkg
	tolua -n dungeon -o w_dun.c dungeon.pkg
