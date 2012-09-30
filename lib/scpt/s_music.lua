-- handle the music school
-- *ALL* lasting spell must return the mana cost in the lasting function

MUSIC_STOP = add_spell
{
        ["name"] =      "Stop singing",
        ["school"] =    {SCHOOL_MUSIC},
        ["level"] =     1,
        ["mana"] =      0,
        ["mana_max"] =  0,
        ["fail"] =      0,
        ["stat"] =      A_CHR,
        ["random"] =    SKILL_MUSIC,
        ["spell"] =     function()
                        player.start_lasting_spell(0)
                        return TRUE
        end,
        ["info"] =      function()
                        return ""
        end,
        ["desc"] =      {
                        "Stops the current song, if any."
        }
}

--- Monster affecting songs(patterns) ---
MUSIC_HOLD = add_spell
{
        ["name"] =      "Holding Pattern",
        ["school"] =    {SCHOOL_MUSIC},
        ["level"] =     1,
        ["mana"] =      1,
        ["mana_max"] =  10,
        ["fail"] =      20,
        ["stat"] =      A_CHR,
        ["random"] =    SKILL_MUSIC,
        ["lasting"] =   function()
                        project_los(GF_OLD_SLOW, 10 + get_level(MUSIC_HOLD, 100))
                        return get_mana(MUSIC_HOLD)
        end,
        ["spell"] =     function()
                        player.start_lasting_spell(MUSIC_HOLD)
                        return TRUE
        end,
        ["info"] =      function()
                        return "power "..(10 + get_level(MUSIC_HOLD, 100))
        end,
        ["desc"] =      {
                        "Slows down all monsters listening the song.",
                        "Consumes the amount of mana each turn.",
        }
}

MUSIC_CONF = add_spell
{
        ["name"] =      "Illusion Pattern",
        ["school"] =    {SCHOOL_MUSIC},
        ["level"] =     5,
        ["mana"] =      2,
        ["mana_max"] =  15,
        ["fail"] =      30,
        ["stat"] =      A_CHR,
        ["random"] =    SKILL_MUSIC,
        ["lasting"] =   function()
                        project_los(GF_OLD_CONF, 10 + get_level(MUSIC_CONF, 100))
                        return get_mana(MUSIC_CONF)
        end,
        ["spell"] =     function()
                        player.start_lasting_spell(MUSIC_CONF)
                        return TRUE
        end,
        ["info"] =      function()
                        return "power "..(10 + get_level(MUSIC_CONF, 100))
        end,
        ["desc"] =      {
                        "Tries to confuse all monsters listening the song.",
                        "Consumes the amount of mana each turn.",
        }
}

MUSIC_STUN = add_spell
{
        ["name"] =      "Stun Pattern",
        ["school"] =    {SCHOOL_MUSIC},
        ["level"] =     10,
        ["mana"] =      3,
        ["mana_max"] =  25,
        ["fail"] =      45,
        ["stat"] =      A_CHR,
        ["random"] =    SKILL_MUSIC,
        ["lasting"] =   function()
                        project_los(GF_STUN, 10 + get_level(MUSIC_STUN, 90))
                        return get_mana(MUSIC_STUN)
        end,
        ["spell"] =     function()
                        player.start_lasting_spell(MUSIC_STUN)
                        return TRUE
        end,
        ["info"] =      function()
                        return "power "..(10 + get_level(MUSIC_STUN, 90))
        end,
        ["desc"] =      {
                        "Stuns all monsters listening the song.",
                        "Consumes the amount of mana each turn.",
        }
}

--- Player affecting songs ---
MUSIC_LITE = add_spell
{
        ["name"] =      "Song of the Sun",
        ["school"] =    {SCHOOL_MUSIC},
        ["level"] =     2,
        ["mana"] =      1,
        ["mana_max"] =  1,
        ["fail"] =      20,
        ["stat"] =      A_CHR,
        ["random"] =    SKILL_MUSIC,
        ["blind"] =     FALSE,
        ["lasting"] =   function()
                        set_lite(5)
                        return 1
        end,
        ["spell"] =     function()
                        player.start_lasting_spell(MUSIC_LITE)
                        return TRUE
        end,
        ["info"] =      function()
                        return ""
        end,
        ["desc"] =      {
                        "Provides light as long as you sing.",
                        "Consumes the amount of mana each turn.",
        }
}

MUSIC_HEAL = add_spell
{
        ["name"] =      "Flow of Life",
        ["school"] =    {SCHOOL_MUSIC},
        ["level"] =     7,
        ["mana"] =      5,
        ["mana_max"] =  35,
        ["fail"] =      35,
        ["stat"] =      A_CHR,
        ["random"] =    SKILL_MUSIC,
        ["lasting"] =   function()
                        hp_player(7 + get_level(MUSIC_HEAL, 60))
                        return get_mana(MUSIC_HEAL)
        end,
        ["spell"] =     function()
                        player.start_lasting_spell(MUSIC_HEAL)
                        return TRUE
        end,
        ["info"] =      function()
                        return "heal "..(7 + get_level(MUSIC_HEAL, 60)).."/turn"
        end,
        ["desc"] =      {
                        "Heals you as long as you sing.",
                        "Consumes the amount of mana each turn.",
        }
}

MUSIC_HERO = add_spell
{
        ["name"] =      "Heroic Ballad",
        ["school"] =    {SCHOOL_MUSIC},
        ["level"] =     10,
        ["mana"] =      4,
        ["mana_max"] =  14,
        ["fail"] =      45,
        ["stat"] =      A_CHR,
        ["random"] =    SKILL_MUSIC,
        ["lasting"] =   function()
                        set_hero(5)
                        if get_level(MUSIC_HERO) >= 10 then
                                set_shero(5)
                        end
                        if get_level(MUSIC_HERO) >= 20 then
                                set_strike(5)
                        end
                        if get_level(MUSIC_HERO) >= 25 then
                                set_oppose_cc(5)
                        end
                        return get_mana(MUSIC_HERO)
        end,
        ["spell"] =     function()
                        player.start_lasting_spell(MUSIC_HERO)
                        return TRUE
        end,
        ["info"] =      function()
                        return ""
        end,
        ["desc"] =      {
                        "Increases melee accuracy",
                        "At level 10 it increases it even more and reduces armor a bit",
                        "At level 20 it increases it again",
                        "At level 25 it grants protection against chaos and confusion",
                        "Consumes the amount of mana each turn.",
        }
}

MUSIC_TIME = add_spell
{
        ["name"] =      "Hobbit Melodies",
        ["school"] =    {SCHOOL_MUSIC},
        ["level"] =     20,
        ["mana"] =      10,
        ["mana_max"] =  30,
        ["fail"] =      70,
        ["stat"] =      A_CHR,
        ["random"] =    SKILL_MUSIC,
        ["lasting"] =   function()
                        set_shield(5, 10 + get_level(MUSIC_TIME, 50), 0, 0, 0)
                        if get_level(MUSIC_TIME) >= 15 then
                                set_fast(5, 7 + get_level(MUSIC_TIME, 10))
                        end
                        return get_mana(MUSIC_TIME)
        end,
        ["spell"] =     function()
                        player.start_lasting_spell(MUSIC_TIME)
                        return TRUE
        end,
        ["info"] =      function()
                        if get_level(MUSIC_TIME) >= 15 then
                                return "AC "..(10 + get_level(MUSIC_TIME, 50)).." speed "..(7 + get_level(MUSIC_TIME, 10))
                        else
                                return "AC "..(10 + get_level(MUSIC_TIME, 50))
                        end
        end,
        ["desc"] =      {
                        "Greatly increases your reflexes allowing you to block more melee blows.",
                        "At level 15 it also makes you faster.",
                        "Consumes the amount of mana each turn.",
        }
}

MUSIC_MIND = add_spell
{
        ["name"] =      "Clairaudience",
        ["school"] =    {SCHOOL_MUSIC},
        ["level"] =     20,
        ["mana"] =      15,
        ["mana_max"] =  30,
        ["fail"] =      75,
        ["stat"] =      A_CHR,
        ["random"] =    SKILL_MUSIC,
        ["lasting"] =   function()
                        set_tim_esp(5)
                        if get_level(MUSIC_MIND) >= 10 then
                                fire_ball(GF_IDENTIFY, 0, 1, 1 + get_level(MUSIC_MIND, 3, 0))
                        end
                        return get_mana(MUSIC_MIND)
        end,
        ["spell"] =     function()
                        player.start_lasting_spell(MUSIC_MIND)
                        return TRUE
        end,
        ["info"] =      function()
                        if get_level(MUSIC_MIND) >= 10 then
                                return "rad "..(1 + get_level(MUSIC_MIND, 3, 0))
                        else
                                return ""
                        end
        end,
        ["desc"] =      {
                        "Allows you to sense monster minds as long as you sing.",
                        "At level 10 it identifies all objects in a radius on the floor,",
                        "as well as probing monsters in that radius.",
                        "Consumes the amount of mana each turn.",
        }
}

--[[
MUSIC_ = add_spell
{
        ["name"] =      "",
        ["school"] =    {SCHOOL_MUSIC},
        ["level"] =     1,
        ["mana"] =      0,
        ["mana_max"] =  0,
        ["fail"] =      20,
        ["stat"] =      A_CHR,
        ["random"] =    SKILL_MUSIC,
        ["lasting"] =   function()
                        return get_mana(MUSIC_)
        end,
        ["spell"] =     function()
                        player.start_lasting_spell(MUSIC_)
                        return TRUE
        end,
        ["info"] =      function()
                        return ""
        end,
        ["desc"] =      {
                        "",
                        "Consumes the amount of mana each turn.",
        }
}
]]
