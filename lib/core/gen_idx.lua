-- Place here the list of files to parse
files =
{
	"skills.txt",
	"dunspoil.txt",
	"inscrip.txt",
	"magic.txt",
	"m_mindcr.txt",
	"r_yeek.txt",
	"r_dunad.txt",
	"r_elf.txt",
	"r_ent.txt",
	"r_hafogr.txt",
	"r_hielf.txt",
	"r_hobbit.txt",
	"r_thlord.txt",
	"r_troll.txt",
	"r_wodelf.txt",
	"rm_lsoul.txt",
	"c_warper.txt",
	"corspoil.txt",
	"g_eru.txt",
	"g_manwe.txt",
	"g_melkor.txt",
	"g_tulkas.txt",
	"g_yavann.txt",
	"gods.txt",
	"c_sorcer.txt",
	"c_thaum.txt",
	"c_rogue.txt",
	"c_runecr.txt",
	"c_lorema.txt",
	"c_mage.txt",
	"c_monk.txt",
	"c_necro.txt",
	"c_ranger.txt",
	"c_alchem.txt",
	"automat.txt",
	"help.hlp",
	"m_demono.txt",
	"m_symbio.txt",
	"m_water.txt",
	"defines.txt",
	"c_swordm.txt",
	"c_symbia.txt",
	"c_palad.txt",
	"c_polear.txt",
	"c_pr_drk.txt",
	"c_pr_eru.txt",
	"c_druid.txt",
	"c_hafted.txt",
	"c_mindcr.txt",
	"birth.txt",
	"c_axemas.txt",
	"m_udun.txt",
	"command.txt",
	"m_necrom.txt",
	"c_priest.txt",
	"m_earth.txt",
	"m_nature.txt",
	"m_tempo.txt",
	"m_convey.txt",
	"essences.txt",
	"tome_faq.txt",
	"m_air.txt",
	"m_divin.txt",
	"m_fire.txt",
	"m_mana.txt",
	"m_meta.txt",
	"m_mind.txt",
	"m_thaum.txt",
	"c_mimic.txt",
	"c_pr_man.txt",
	"c_demono.txt",
	"c_assass.txt",
	"wishing.txt",
	"spoiler.hlp",
	"version.txt",
	"rm_spec.txt",
	"rm_vamp.txt",
	"rm_zomb.txt",
	"rm_barb.txt",
	"rm_class.txt",
	"rm_corru.txt",
	"rm_herm.txt",
	"rm_skel.txt",
	"r_kobold.txt",
	"r_maia.txt",
	"r_orc.txt",
	"r_pettyd.txt",
	"r_rohank.txt",
	"r_gnome.txt",
	"r_hafelf.txt",
	"r_human.txt",
	"r_beorn.txt",
	"r_deathm.txt",
	"r_drkelf.txt",
	"r_dwarf.txt",
	"newbie.hlp",
	"option.txt",
	"macrofaq.txt",
	"lua_util.txt",
	"luckspoi.txt",
	"lua_ques.txt",
	"lua_skil.txt",
	"lua_spel.txt",
	"lua.hlp",
	"lua_gf.txt",
	"lua_intr.txt",
	"lua_mon.txt",
	"lua_play.txt",
	"lua_pow.txt",
	"gambling.txt",
	"general.txt",
	"fatespoi.txt",
	"dungeon.txt",
	"c_warrio.txt",
	"c_summon.txt",
	"c_unbel.txt",
	"c_posses.txt",
	"c_merch.txt",
	"bldg.txt",
	"c_archer.txt",
	"TANG.txt",
	"attack.txt",
	"c_bard.txt",
	"m_music.txt",
	"ability.txt",
	"m_geoman.txt",
	"c_geoman.txt",
}

out_file = "index.txt"

index = {}

function parse_file(file)
	local fff = openfile(path_build(ANGBAND_DIR_HELP, file), "r")
	local line

	line = read(fff, "*l")
	while line do
		local i, j, anchor, name, subname = strfind(line, "~~~~~(%d+)|([%d%a -]+)|([%d%a -]+)")
		if not i then
			i, j, anchor, name = strfind(line, "~~~~~(%d+)|([%d%a -]+)")

			subname = nil
		end

		if i then
			if not index[name] then
				index[name] = {}
			end
			if subname then
				tinsert(index[name], { __name__ = subname, __file__ = file, __anchor__ = anchor})
			else
				tinsert(index[name], { __name__ = "__primary__", __file__ = file, __anchor__ = anchor})
			end
		end

		line = read(fff, "*l")
	end

	closefile(fff)
end

function sort_fct(a, b)
	local i, len

	a = a.__name__
	b = b.__name__

	if strlen(a) > strlen(b) then len = strlen(b) else len = strlen(a) end

	for i = 1, len do
		local ac = strbyte(a, i)
		local bc = strbyte(b, i)

		if ac < bc then
			return not nil
		elseif ac > bc then
			return nil
		end
	end
	if strlen(a) > strlen(b) then return nil else return not nil end
end

function generate_index()
	local k, e, index_list
	for _, e in files do
		parse_file(e)
	end

	index_list = {}
	for k, e in index do
		-- Ok either my sort function or lua sort function sucks ass ..
		sort(e, sort_fct)
		sort(e, sort_fct)
		sort(e, sort_fct)
		sort(e, sort_fct)
		sort(e, sort_fct)
		tinsert(index_list, {__name__= k, __table__ = e})
	end

	-- Ok either my sort function or lua sort function sucks ass ..
	sort(index_list, sort_fct)
	sort(index_list, sort_fct)
	sort(index_list, sort_fct)
	sort(index_list, sort_fct)
	sort(index_list, sort_fct)
	index = index_list
end

function out_link(fff, space, name, file, anchor)
	write(fff, space.."*****"..file.."*"..anchor.."["..name.."]\n")
end

function print_index()
	local i, j, c, new_c
	local fff = openfile(path_build(ANGBAND_DIR_HELP, out_file), "w")

	write(fff,
[[|||||oy
#####R	     /----------------------------------------\
#####R	    <		 Help Index	       >
#####R	     \----------------------------------------/

This is the index of everything in the T.o.M.E. documentation.

#####BHit a letter key to jump to the entries for that letter.

Some entries in the index link to the same place as other entries. This is 
intentional, so that the information you want is easy to find.

Don't forget you can browse the help from the *****help.hlp*02[Main menu].

#####sSpotted a problem with the help files, or some content thats missing? 
#####sContact fearoffours@t-o-m-e.net .

]])

	c = ' '
	for i = 1, getn(index) do
		new_c = strbyte(index[i].__name__, 1)
		if c ~= new_c then
			c = new_c
			write(fff, "~~~~~"..c.."\n")
			write(fff, "*****/"..strchar(c)..out_file.."*"..c.."["..strchar(c).."]\n")
		end
		for j = 1, getn(index[i].__table__) do
			if index[i].__table__[j].__name__ == "__primary__" then
				out_link(fff, "  ", index[i].__name__, index[i].__table__[j].__file__, index[i].__table__[j].__anchor__)
			end
		end
		for j = 1, getn(index[i].__table__) do
			if index[i].__table__[j].__name__ ~= "__primary__" then
				out_link(fff, "    ", index[i].__table__[j].__name__, index[i].__table__[j].__file__, index[i].__table__[j].__anchor__)
			end
		end
	end
	closefile(fff)
end

generate_index()

print_index()
