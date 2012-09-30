-- Monster stuff, do not touch

function summon_monster(y, x, lev, friend, typ)
	if type(typ) == "number" then
        	if friend == TRUE then
                	summon_specific_friendly(y, x, lev, typ, FALSE)
                else
                	summon_specific(y, x, lev, typ)
                end
        else
        	summon_monster_aux(y, x, lev, friend, typ)
        end
end
