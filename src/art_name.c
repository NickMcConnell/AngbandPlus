#include "angband.h"

#include "str_map.h"
#include <assert.h>

static cptr _panic_name(void) { return format("'%s'", player_name); }

/******************************************************************************
 * The Object we are naming
 ******************************************************************************/
typedef struct {
    obj_ptr obj;
    int     bias;
    int     score;
} _forge_t, *_forge_ptr;

static _forge_t _forge(obj_ptr obj, int bias)
{
    _forge_t forge;
    forge.obj = obj;
    forge.bias = bias;
    forge.score = new_object_cost(obj, COST_REAL);
    return forge;
}

/******************************************************************************
 * Avoid Duplicate Names
 * When picking from a list, we first try to find an unused name. If the
 * list is exhausted, we'll run through the list a second time until every
 * name has been used twice, and so on. Names are remembered in the savefile.
 *
 * Note that ego->art names are specified in ../lib/edit/e_info.txt and stored
 * in ego_type.art_names and ego_type.art_names_high. Still, its really nice
 * to avoid duplicates here as well! art_get_name_ego will remember names as they
 * are assigned.
 ******************************************************************************/
static str_map_ptr _names(void)
{
    static str_map_ptr map = NULL;
    if (!map)
        map = str_map_alloc(NULL);
    return map;
}

static int _name_count(cptr name)
{
    return str_map_find_int(_names(), name);
}

static void _remember_name(cptr name)
{
    str_map_add_int(_names(), name, _name_count(name) + 1);
}

static int _min_count(cptr list[])
{
    int result = 999999, i, ct;
    /* Find the minimum usage count for this list. We can
     * short circuit once we find an unused name (normal case) */
    for (i = 0; result; i++)
    {
        cptr name = list[i];
        if (!name) break;
        ct = _name_count(name);
        if (ct < result)
            result = ct;
    }
    return result;
}

static cptr _random_aux(cptr list[], int count)
{
    int i, tot = 0, n;
    for (i = 0; ; i++)
    {
        cptr name = list[i];
        if (!name) break;
        if (_name_count(name) > count) continue;
        tot++;
    }
    assert(tot > 0);
    n = randint0(tot);
    for (i = 0; ; i++)
    {
        cptr name = list[i];
        if (!name) break;
        if (_name_count(name) > count) continue;
        n--;
        if (n < 0) return name;
    }
    return NULL; /* bug */
}

static cptr _random(cptr list[])
{
    return _random_aux(list, _min_count(list));
}

/* for art_create_ego ... avoid duplicates */
static int _vmin_count(vec_ptr v)
{
    int result = 999999, i, ct;
    int j = vec_length(v);
    for (i = 0; result && i < j; i++)
    {
        cptr name = vec_get(v, i);
        ct = _name_count(name);
        if (ct < result)
            result = ct;
    }
    return result;
}

static cptr _vrandom_aux(vec_ptr v, int count)
{
    int i, tot = 0, n;
    int ct = vec_length(v);
    for (i = 0; i < ct; i++)
    {
        cptr name = vec_get(v, i);
        if (_name_count(name) > count) continue;
        tot++;
    }
    assert(tot > 0);
    n = randint0(tot);
    for (i = 0; i < ct; i++)
    {
        cptr name = vec_get(v, i);
        if (_name_count(name) > count) continue;
        n--;
        if (n < 0) return name;
    }
    return NULL; /* bug */
}

static cptr _vrandom(vec_ptr v)
{
    return _vrandom_aux(v, _vmin_count(v));
}

/******************************************************************************
 * Lights (Names by Andrew Levy for Chengband)
 * XXX We should use separate names for OF_DARKNESS
 ******************************************************************************/
static cptr _light_low(_forge_ptr forge)
{
    static cptr name[] = { "'Aquilon Lamp'", "'Corona'", "'Eye of Argon'", "'Fairie Light'",
        "'Firefly'", "'Foxfire'", "'Lucky Star'", "'Moonglow'", "'Pathfinder'", "'Planaria'",
        "'Scintillator'", "'Silverlight'", "'Skycleaver'", "'Sungazer'", "'Talisman'", "'Tarazed'",
        "'Trailblazer'", "of Alhazred", "of Mordenkainen", "of Otiluke", NULL };
    return _random(name);
}

static cptr _light_med(_forge_ptr forge)
{
    static cptr name[] = { "'Andromeda'", "'Astral Ray'", "'Divine Light'", "'Eye of the Overworld'",
        "'Hellion'", "'Iron Star'", "'Oculus Dei'", "'Powersphere'", "'Pulcherrima'", "'Sadachbia'",
        "'Shadowbane'", "'Silmaril Jr.'", "'Spectral Compass'", "'Sunray'", "'Wizard Eye'",
        "'Zephyrlight'", "of Drawmij", "of Phantasmagoria", "of Tenser", "of Transcendence",
        "of the Trapped Genie", NULL };
    return _random(name);
}

static cptr _light_high(_forge_ptr forge)
{
    static cptr name[] = { "'Altair'", "'Apocalypse Beacon'", "'Arcturus'", "'Aurora Borealis'",
        "'Avatar of Light'", "'Dawnbringer'", "'Destiny Star'", "'Eye of Ghidora'",
        "'Furnace of Light'", "'Lightbringer'", "'Lux Aeterna'", "'Mirzam'", "'Morinehtar'",
        "'Phoenix Eye'", "'Suncaller'", "'Sundrop of Varda'", "'Vaevictis'", "of Celestial Guidance",
        "of Nystul", "of Revelation", "of Sustarre", "of the Firmament", "of the Zodiac", NULL };
    return _random(name);
}

static cptr _dark_med(_forge_ptr forge)
{
    static cptr name[] = { "of Unlight", "of Remorse", "'Dark Side of the Moon'",
        "of the Neverborn", "'Blacksphere'", "'Shadowstalk'",
        "'Darkwalker'", "'Dark Guide'", "'Deluminator'", "of the Dark Age", 
        "of the Benighted", "'Soul of Blackness'", "'Jewel of the Underworld'",
        "'Nightbringer'", "of Midnight", NULL };
    return _random(name);
}

static cptr _dark_high(_forge_ptr forge)
{
    static cptr name[] = { "'Dark Prophecy'", "of the Dark One", "of Vlad", "'Eye of Vecna'",
        "of the Lord of Darkness", "'Dark Star'", "of Ungoliant", "'Heart of Darkness'",
        "'Darkenstone'", "of the Lord of Shadows", "'Black Breath'", NULL };
    return _random(name);
}

static cptr _light(_forge_ptr forge)
{
    if (forge->obj->sval == SV_LIGHT_DARK)
    {
        if (forge->score < 30000) return _dark_med(forge);
        return _dark_high(forge);
    }
    if (forge->score < 5000) return _light_low(forge);
    else if (forge->score < 15000) return _light_med(forge);
    return _light_high(forge);
}

/******************************************************************************
 * Weapons
 ******************************************************************************/
static cptr _weapon_low(_forge_ptr forge)
{
    if (forge->obj->tval == TV_BOW)
    {
        static cptr name[] = { "'Phaser'", "'Black Hawk'", "'Plinker'", "'Crackdown'",
            "'Peashooter'", "'Bugslayer'", "'Magnum'", "of the Novice Archer", "of the Low Ranger",
            "'Magic Missile'", "'Widowmaker'", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_DEMON)
    {
        static cptr name[] = { "'Evil Soldlier'", "'Satan's Claw'", "of the Nurgling", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_NECROMANTIC)
    {
        static cptr name[] = { "'Blood'", "'Tombstone'", "'Rotten Child'", "'Necrophiliac'",
            "'Dread of Night'", "'Dungeon Shade'", "'Elfrist'", "of Misery", "of Boodoo", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_PRIESTLY)
    {
        static cptr name[] = { "'Indulgence'", "'Angel's Love Song'", "'Monastery'", "'Pure Mischief'",
            "'Self-Enlightenment'", "'Angelic Blessing'", "'Path of Peace'", "'Starlit Angel'",
            "'Angelic Page'", "'Disciple of Law'", "'Worship'", "of Blessing", "of Holy Smoke", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_MAGE)
    {
        static cptr name[] = { "'Instant Mage Maker'", "'Force of Hand'", "'Kumon'", "'Math Magic'",
            "'Cheating'", "of the Novice Mage", "of Jongnamk, Son of Jongil", "of Tips", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_WARRIOR)
    {
        static cptr name[] = { "'Basher'", "'Body Power'", "'Muscle'", "'Abslicer'", "'Peace Maker'",
            "'Protein'", "'Grizzly Bear'", "'Warrior's Charge'", "'Valorous Charge'",
            "'Warrior's Stand'", "'Spoils of Victory'", "'Ogresmacher'", "'Weekend Warrior'",
            "of Slay Munchkin", NULL };
        return _random(name);
    }
    else
    {
        static cptr name[] = { "'Grayswandie'", "'Plutonium'", "'X-Winblow'", "'Wormbiter'",
            "'Shrieker'", "'Rotten Egg'", "'Invader'", "'Budokan'", "'Breakout'", "'Kryptonite'",
            "'Virus'", "'Patriot'", "'Bungee'", "'Pig's Eye'", "'Gilettar'", "'Angrist'",
            "'Belangil'", "of Severe Pain", "of Horror", "of Agony", "'Wrath of Khan'", NULL };
        return _random(name);
    }
}

static cptr _weapon_med(_forge_ptr forge)
{
    if (forge->obj->tval == TV_BOW)
    {
        static cptr name[] = { "'Cubragol'", "'Deadshot'", "'Bullseye'", "of the Archer", "of the Ranger",
            "of the Sniper", "of Archery", "'Peacemaker'", "'Quickdraw'", "'Doom Bolt'", "'Persuader'",
            "'Bird of Prey'", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_DEMON)
    {
        static cptr name[] = { "'Hellwind'", "'Chief of Souls'", "'Edge of Miasma'",
            "'Hell Blade'", "of Helthing", "'Lunatic'", "'Warped Edge'", "'Lawslayer'",
            "'Distorter'", "'Banquet of Azathoth'", "'Cosmic Horror'", "of Hionhurn",
            "'Bloodletter'", "'Crimson Tide'", "'Bloodlust'", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_NECROMANTIC)
    {
        static cptr name[] = { "'Reaper'", "'Death Sword'", "'Zombie Claw'", "'Pestilence'",
            "'Night Blade'", "'Dark Banisher'", "'Bloody Mary'", "of Darkness Storm",
            "of Genocide", "of Thuringwethil", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_PRIESTLY)
    {
        static cptr name[] = { "'Lawgiver'", "'Sun Sword'", "'Demonbane'", "'Gospel'", "'Advent'",
            "'Mundwine'", "'God Hand'", "'Ray Storm'", "'Crusader'", "'Demonbane'", "'Lightbringer'",
            "'Joyeuse'", "'Shiningcalibur'", "of Saladin", "of Holiness", "of Justice",
            "of the Priests", "of Awe", "of Mind", "of Soul", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_MAGE)
    {
        static cptr name[] = { "'Magicbane'", "'Hucker'", "'Manabringer'", "'Mirror of Seeing'",
            "'Whip of Evil Eye'", "'Brainstorm'", "'Laplace's Devil'", "of Grand Wizardry",
            "of Concentration", "of Ritual", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_WARRIOR)
    {
        static cptr name[] = { "'Skullbasher'", "'Bonebreaker'", "'Skullbuster'", "'Skull Smasher'",
            "'Giant Strength'", "'Dragonbane'", "'Crash of Rhinos'", "'Cycle of Life'",
            "'Bull Elephant'", "'Warrior's Honor'", "of Rage", "of Crushing", "of Tomoe",
            "of Erkenbrando", NULL };
        return _random(name);
    }
    else
    {
        static cptr name[] = { "'Weapon Alpha'", "'Werebane'", "'Dragon Claw'", "'Kraken'",
            "'Blue Steele'", "'Fury'", "'Howler'", "'Roarer'", "'Red October'", "'Witchfire'",
            "'Lord of Flies'", "'Pandemonium'", "'Bloodrain'", "'Delta'", "of Joshua",
            "of Slay Mold", "of Damocles", "of the Shogun", "of Hatred", "of Osric",
            "of Finndo", "'Suppressor'", "of Desolation", NULL };
        return _random(name);
    }
}

static cptr _weapon_high(_forge_ptr forge)
{
    if (forge->obj->tval == TV_BOW)
    {
        static cptr name[] = { "of the Master Archer", "of the High Ranger",
            "of the Ultimate Sniper", "of Grand Archery", "of Judgment", "'Deathbolt'",
            "'Shoot to Kill'", "of Apollo", "'Divine Retribution'", "'Niobe's Woe'",
            "'Final Arbiter'", "'Judgment Thunder'", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_DEMON)
    {
        static cptr name[] = { "'Angel of Ruin'", "of Abyss", "of Demogorgon", "of Namo",
            "'Doomblade'", "'Armageddon'", "'Mournblade'", "'Laplace's Devil'", "of Khorne",
            "of Tzeentch", "of Vishnu", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_NECROMANTIC)
    {
        static cptr name[] = { "'Soulsucker'", "'Deathstorm'", "'Word of Death'", "'Haradekket'",
            "'Soul Burn'", "'Taminalbringer'", "'Darknessbringer'", "of Vlad", "of Death",
            "'Bloodfeast'", "'Dance of Death'", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_PRIESTLY)
    {
        static cptr name[] = { "of the Guardian", "(*Holy Avenger*)", "'Word of God'", "'Hand of God'",
            "'Judgement of Heaven'", "'Apocalypse-Now'", "'Jehovah's Wrath'", "'Divine Intervention'",
            "'Right Hand of Justice'", "'Yggr-drasill'", "'Ascalon'", "'Herugrim'", "'Alondait'",
            "'Brionac'", "'Lawbringer'", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_MAGE)
    {
        static cptr name[] = { "'Eternal Verities'", "'Mana Spectrum'", "'Invisible Hand'",
            "'Ancient Wisdom'", "'Ancestral Knowledge'", "'Mana Stone'", "'Giver of Intelligence'",
            "of Olorin", "of Mana", "of Ganesa", "of Saruman", "of Ea", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_WARRIOR)
    {
        static cptr name[] = { "'Masamune'", "'Undying Samurai'", "'Vanquisher'", "'Snickersnee'",
            "'Iron Biter'", "'Spiral Wave'", "'Gaea's Might'", "'Destroyer'", "'Mundwine'",
            "of Ultimate Might", "of Shiva", "of Destruction", "of Gerard", "of Benedict", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_ELEC)
    {
        static cptr name[] = { "'Vajra'", "'Aeglin'", "'Kaladbolg'", "'Anguirel'", "'Storm Halberd'",
            "'Thunder Zap'", "'Angered Thunder God'", "'Thunder Bringer'", "'Rising Thunder'",
            "of Djinni", "of Poseidon", "of Raijin", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_FIRE)
    {
        static cptr name[] = { "'Red Storm'", "'Phoenix'", "'Flame Tongue'", "'Exodus'",
            "'Inferno'", "'Durin's Trouble'", "'Spitting Fire'", "'Volcanic Geyser'",
            "of Prominence", "'Flame of Udun'", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_COLD)
    {
        static cptr name[] = { "'Absolute Zero'", "'Ice Master'", "'Belangil'", "'Angrist'",
            "'Ice Edge'", "'Snowbringer'", "'Diamond Dust'", "'Ice Blast'", "'Icy Manipulator'",
            "of Ice", "of Ymir", NULL };
        return _random(name);
    }
    else
    {
        static cptr name[] = { "'Bloodscourge'", "'Soul Searer'", "'Cosmic Horror'", "'Nemesis'",
            "'Inferno'", "'Warhead'", "'Conqueror'", "'Angel of Death'", "'Destroying Angel'",
            "'Avenging Angel'", "'Black Star'", "'Kalma'", "'Megadeath'", "'Doomsday'", "'Eternity's End'",
            "'Word of Death'", "'Weapon Omega'", "'Finalizer'", "of Blays", "of Eric", "of Elric",
            "of Beowulf", "'Fatal Destiny'", "'Wrath of Achilles'", "'Sigil of Doom'", "of Terror",
            "of Panic", "of Strife", "'God of War'", "'Eternal Requiem'", "of Unchained Fury", "'Voidbringer'", NULL };
        return _random(name);
    }
}
static cptr _weapon(_forge_ptr forge)
{
    if (forge->score < 10000) return _weapon_low(forge);
    else if (forge->score < 40000) return _weapon_med(forge);
    return _weapon_high(forge);
}

/******************************************************************************
 * Harps
 ******************************************************************************/
static cptr _harp_low(_forge_ptr forge)
{
    static cptr name[] = { "of the Fool", "of the Court Jester", "of the Unfinished Symphony",
        "'Cacophony'", "'Screech'", "'Eternal Feedback'", "'Quiet Riot'", "'Karaoke'", NULL};
    return _random(name);
}
static cptr _harp_med(_forge_ptr forge)
{
    static cptr name[] = { "of the Bard", "of Soothing Melodies", "of Inspiration",
        "'Unending Song'", "'Harmonic Flux'", "'Musical Infusions'", "'Sonic Projection'",
        "'Tragic Symphony'", "'Gypsy Melodies'", "'Harmonizer'", NULL };
    return _random(name);
}
static cptr _harp_high(_forge_ptr forge)
{
    static cptr name[] = { "of the Master Bard", "of Rivendell", "of Beleriand", "of the Muse",
        "'Overtones of Despair'", "'Harmonic Storm'", "'Songs of Victory'", "'Amadeus'", NULL };
    return _random(name);
}
static cptr _harp(_forge_ptr forge)
{
    if (forge->score < 10000) return _harp_low(forge);
    else if (forge->score < 40000) return _harp_med(forge);
    return _harp_high(forge);
}

/******************************************************************************
 * Armor
 ******************************************************************************/
static cptr _armor_low(_forge_ptr forge)
{
    if (forge->bias & BIAS_DEMON)
    {
        static cptr name[] = { "'Infernal Contract'", "'Sacrifice to void'", "of Sabbath",
            "'Strange Attractor'", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_NECROMANTIC)
    {
        static cptr name[] = { "'Darkest Hour'", "'Unholy Strength'", "'Revenant'",
            "'Forgotten Fear'", "'Zombie Master'", "of Arcam", "of Deformity",
            "of Aleister Crowley", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_PRIESTLY)
    {
        static cptr name[] = { "'Preestablished Harmony'", "'Holiday'", "'Confession'",
            "'Soul Helper'", "'Spiritual Focus'", "'Holy Strength'", "'Avatar of Hope'",
            "'Keeper of Mind'", "'Oracle's Attendant'", "'Peacehoper'", "'Seeker of Skybreak'",
            "of the Buddhist Scriptures", "of Angel", "of New Life", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_ROGUE)
    {
        static cptr name[] = { "'Lurker'", "of Sneaking", "of the Novice Rogue", "of the Assassin's Apprentice", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_MAGE)
    {
        static cptr name[] = { "'Deja-Vu'", "'Caffeine'", "'Oath of Mages'", "'Whispers of Muse'",
            "of Talisman", "of Martine", "of Quiz King", "of Knowing", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_WARRIOR)
    {
        static cptr name[] = { "'Tournament'", "'Turbo'", "'Muscle Curtain'", "of Resist Smasher",
            "of the Minotaur", "of Sumo Wrestler", "of Minotaur", "of Meng Huo", NULL };
        return _random(name);
    }
    else if (forge->bias == BIAS_ELEMENTAL)
    {
        static cptr name[] = { "of Minor Protection", "'Glitterbug'", "of the Base Elements", NULL };
        return _random(name);
    }
    else if (obj_is_body_armor(forge->obj))
    {
        static cptr name[] = { "'Chrysalis'", "of the Legionnaire", "'Bladereturner'", "'Iron Maiden'",
            "of Monster Repelling", "of the Untouchables", "'Baby Dragon Scale Mail'",
            "'Back to the Front'", "'Full Metal Jacket'", "'Rattleshirt'", "of the Doughty Knight",
            "of the Fallen", "of the Slain", "of the Vanquished", "of the Squire", "of Rust",
            "'Widow's Lament'", "of the Vanguard", NULL };
        return _random(name);
    }
    else
    {
        static cptr name[] = { "'Sambo'", "of Minor Protection", "'Generator'",
            "of Discipline", "'Vainglory'", "of No Return", "of the Survivor", "of the Sellsword",
            "'Delusions of Grandeur'", "'Battle Fatigue'", "'Deliverance'", "of Minor Keeping",
            "'Spoils of Victory'", "'Triumph'", NULL };
        return _random(name);
    }
}
static cptr _armor_med(_forge_ptr forge)
{
    if (forge->bias & BIAS_DEMON)
    {
        static cptr name[] = { "'Day of the Dove'", "'Prince of Chaos'", "'Chaos Tile'",
            "'Sad Giant's Shield'", "'Ever-Faithful'", "'Guard of Law'", "of Harmony",
            "of Reason", "of Seldon", "of the Chaos Lord", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_NECROMANTIC)
    {
        static cptr name[] = { "'Total Eclipse'", "'The Depth of Abyss'", "'Jet-black Bird'",
            "'Necrologia'", "'Total Solar Eclipse'", "'Undertaker'", "of Rituals",
            "of the Ruler of Wallachia", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_PRIESTLY)
    {
        static cptr name[] = { "'Enlightenment from Heaven'", "'Holy Chant'", "'Holy Prayer'",
            "'Righteousness'", "'Gospel'", "'Guardian Angel'", "'Hope and Glory'", "'Aether Flash'",
            "'Parament of Holy Brume'", "'Holy Destiny'", "'Keeper of Light'", "'Righteous Aura'",
            "'Nuts of Wisdom'", "of Eden", "of Piety", "of the Pious Believer", "of Ceremony",
            "of Rite", "of the Unicorn", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_ROGUE)
    {
        static cptr name[] = { "of Shadows", "of Thievery", "of the Master Rogue", "'Darkness Lurker'", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_MAGE)
    {
        static cptr name[] = { "'Magi's Seeing'", "'Sign of Brilliance'", "'Tome of Intelligence'",
            "of *Intelligence*", "of the Wizard", "of Knowledge", "of the Spellcaster", "of Arcane",
            "of Martin", "of Vast Intellect", "of Primal Sentience", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_WARRIOR)
    {
        static cptr name[] = { "'Steel Bull'", "'Avatar of Might'", "'Terminater'", "'Die Hard'",
            "'Power Shield'", "'Power Matrix'", "'Stonecold'", "of Vader", "of the Valkyrie",
            "of Daybreak", "of Diamond", "of Sad Giant", NULL };
        return _random(name);
    }
    else if (forge->bias == BIAS_ELEMENTAL)
    {
        static cptr name[] = { "'Iridescent Barrier'", "'Shimmering Protection'", "of *Resistance*",
            "'Fifth Element'", NULL };
        return _random(name);
    }
    else if (obj_is_body_armor(forge->obj))
    {
        static cptr name[] = { "'Steelskin'", "of Camelot", "of the Champion", "of Crystal",
            "of Tournaments", "of Knights", "of Gondor", "of Chivalry", "'Endless Melee'",
            "'Last Stand'", "of Barahir", "of Faramir", "'Warrior's Honor'", "'Endless Battle'", 
            "'Knight's Regalia'", "'Fearless Champion'", "of the White Knight", "'Iron Cage'", NULL };
        return _random(name);
    }
    else
    {
        static cptr name[] = { "'Dal-i-Thalion'", "'Guardian'", "'Defender'", 
            "of Icarus", "of the Dark Tiger", "of Belegennon", "of Evermore",
            "'Pride of Battle'", "'March of Doom'", "'Drums of War'", "of Keeping",
            "'Call of Battle'", "'Dauntless'", "of the Dragonslayer", "of Protection",
            "of Unsung Glory", "of the Hero", "'Eternal Vigilance'", NULL };
        return _random(name);
    }
}
static cptr _armor_high(_forge_ptr forge)
{
    if (forge->obj->tval == TV_BOOTS && have_flag(forge->obj->flags, OF_SPEED) && forge->obj->pval > 4)
    {
        static cptr name[] = { "of Hermes", "of Great Haste", "of the Divine Messenger", 
            "of Light Speed", "'Avatar of Light'", "of Rolento", "of the Golden Angel",
            "of Swift-Footed Achilleus", "of Aias", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_DEMON)
    {
        static cptr name[] = { "'Bloodmail of Lucifer'", "'Bottomless Pit'", "of Hades",
            "of Mephistopheles", "of the Demon Lord", "'Eternal Torment'", "'Beelzebub's Wing'",
            "'Lord of Flies'", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_NECROMANTIC)
    {
        static cptr name[] = { "'Love of Demeter'", "of the Immortals", "of Sauron", "of Azriel",
            "of Nazgul", "of Gorgoroth", "of Angmar", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_PRIESTLY)
    {
        static cptr name[] = { "'Weirding'", "'Ever-Faithful'", "'Talisman of Simplicity'",
            "'God Mode'", "'Genesis'", "'Celestial Protection'", "'Avatar of Light'", "of the Gods",
            "of Divine Protection", "of the First Day", "of the Isles of the Blessed", "of the Holy Grail",
            "of the Platinum Dragon", "'Lawmaster'", "'Immortal Destiny'", "of Order", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_ROGUE)
    {
        static cptr name[] = { "of the Assassin", "'Dark Stalker'", "of the Grand Master Thief", "of Jack of Shadows", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_MAGE)
    {
        static cptr name[] = { "of Radagast", "of Eol", "of Merlin's Pattern", "of the Great Magi",
            "of Loki", "'Clear Mind'", "'the Eye of the Aethiopica'", "'Mana Prism'", "'Ivory Tower'",
            "of Revelation", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_WARRIOR)
    {
        static cptr name[] = { "'Vajra'", "'Aegis'", "'Cloth of Vice'", "of Lions", "of Samurai",
            "of Titans", "of *Power*", "of Nike", "of Kronos", "of Diomedes", NULL };
        return _random(name);
    }
    else if (forge->bias == BIAS_ELEMENTAL)
    {
        static cptr name[] = { "of the *Elements*", "of Many Colors", "of Hyper Resistance",
            "'Elemental Scourge'", "of Immunity", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_ACID)
    {
        static cptr name[] = { "'O-Parts'", "'Scale of Vrtra'", "of Baihu", "of Heket", "of Rebma",
            "of the Everlasting", "of Boldor", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_ELEC)
    {
        static cptr name[] = { "of Galactic Barrier", "of Resist *Lightning*", "of Thor", "of Zeus",
            "'Shambler'", "'Roaring Lightning Dragon'", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_FIRE)
    {
        static cptr name[] = { "'Firewall'", "'Phoenix'", "'Rengoku-Kaen'", "of Hellfire", "of Corona",
            "of Yama", "of Uriel", "'Firestorm'", "of Surtur", NULL };
        return _random(name);
    }
    else if (forge->bias & BIAS_COLD)
    {
        static cptr name[] = { "'Helka-Turma'", "'White Diamond'", "'Great Snowfield'",
            "'Scale of Ice Dragon'", "'Great Winter'", "of Ymir", "of Snow Field", "of Soul Freeze", NULL };
        return _random(name);
    }
    else if (obj_is_body_armor(forge->obj))
    {
        static cptr name[] = { "'Monolith'", "of Diamond", "'Living Armour'", "'Living Wall'",
            "'Citadel'", "of Great Barrier", "'Elite Fortress'", "'Dark Fortress'",
            "of the Makers", "of Celeborn", "'Eternal Bastion'", "of the Red Eagle", "of the Golden Crane",
            "of the Great Champion", "'Battle of Evermore'", "'Achilles' Last Stand'", "of the Undefeated",
            "'Sliver of Infinity'", "of Bright-Eyed Athene", "of the Golden Avatar", "of the Undying Hero",
            NULL };
        return _random(name);
    }
    else if (forge->obj->tval == TV_SHIELD)
    {
        static cptr name[] = { "'Sactuary'", "'Bulwark'", "'Divine Barrior'", "of Ultimate Protection",
            "of Invulnerability", "'Bladeturner'", "of the Steel Dragon", "'Swordbreaker'", 
            NULL };
        return _random(name);
    }
    else
    {
        static cptr name[] = { "'Destiny Star'", "of Malachite", "'Genesis'", "of the Dungeon Master",
            "of the Dungeon Keeper", "'Intact'", "'Battlemech'", "'Meganoid'", "'Imperium'",
            "of the Empire", "of Infinity", "of the Makers", "of Thrain", "of Deirdre", "of Random",
            "of Eternity", "of the Last Battle", "of Preservation", NULL };
        return _random(name);
    }
}
static cptr _armor(_forge_ptr forge)
{
    if (forge->score < 10000) return _armor_low(forge);
    else if (forge->score < 45000) return _armor_med(forge);
    return _armor_high(forge);
}

/******************************************************************************
 * Amulets
 ******************************************************************************/
static cptr _amulet_med(_forge_ptr forge)
{
    static cptr name[] = { "'Mayan Princess'", "'Firebird'", "of Shining", "of Dazzling",
        "'Cruciform'", "of Protection", "'Precious Protection'", "'Talisman'", "of Lore",
        "of Nobility", "of The Fair Maiden", "of Fortitude", "of Seeking", "of Questing",
        "'Ogre Shaman'", "'Final Stand'", "'Starburst'", "of Fantastic Wealth", "'Priceless Heirloom'",
        NULL };
    return _random(name);
}

static cptr _amulet_high(_forge_ptr forge)
{
    static cptr name[] = { "of the Pharaohs", "of Ancient Kings", "of Power", "of Tranquility",
        "of Shining Vengeance", "of The One True King", "of Ascension", "of Eternal Glory",
        "of Dwarvish Craft", "of Elvish Craft", "of Titans", "of the Gods", "of Eternal Life",
        "of Immortality", "of Domination", "of Honor", "of Glory", "of Dark Vengeance", "'Sun King'",
        NULL };
    return _random(name);
}

static cptr _amulet(_forge_ptr forge)
{
    if (forge->score < 30000) return _amulet_med(forge);
    return _amulet_high(forge);
}

/******************************************************************************
 * Rings
 ******************************************************************************/
static cptr _ring_med(_forge_ptr forge)
{
    static cptr name[] = { "'Precious'", "of Glory", "of Justice", "of Wishing", "of Invisibility",
        "'Angelic Beauty'", "of Past Glory", "of Kings", "of the Future King", "of Truth",
        "of Magic", "of Luck", "of Deliverance", "'Great Treasure'", NULL };
    return _random(name);
}

static cptr _ring_high(_forge_ptr forge)
{
    static cptr name[] = { "of Power", "of Elvish Power", "of Dwarvish Power", "of Giant Strength",
        "'Golden Avatar'", "of Immortality", "'Draupnir'", "of Gyges", "of Nimue", "of Merlin",
        "of Eternity", "'Avatar of Destruction'", "of Destruction", "'Dragonfire'", "of Titans",
        "of the Gods", "of Ascension", "of Dragonkind", "of the Dark Lord", "'Celestial Protection'",
        NULL };
    return _random(name);
}

static cptr _ring(_forge_ptr forge)
{
    if (forge->score < 40000) return _ring_med(forge);
    return _ring_high(forge);
}

/******************************************************************************
 * Entry Point
 ******************************************************************************/
static cptr _name(_forge_ptr forge)
{
    if (obj_is_(forge->obj, TV_BOW, SV_HARP))
        return _harp(forge);
    else if (obj_is_weapon_ammo(forge->obj) || obj_is_bow(forge->obj))
        return _weapon(forge);
    else if (obj_is_armor(forge->obj))
        return _armor(forge);
    else if (forge->obj->tval == TV_LIGHT)
        return _light(forge);
    else if (forge->obj->tval == TV_AMULET)
        return _amulet(forge);
    else if (forge->obj->tval == TV_RING)
        return _ring(forge);
    return NULL;
}

/******************************************************************************
 * Public
 ******************************************************************************/
cptr art_get_name(obj_ptr obj, int bias)
{
    _forge_t forge = _forge(obj, bias);
    cptr     name = _name(&forge);

    if (!name) return _panic_name();
    return name;
}

/* helper for _name in art.c */
cptr art_get_name_ego(vec_ptr names)
{
    cptr name = _vrandom(names);
    if (!name) return _panic_name();
    return name;
}

void art_remember_name(cptr name)
{
    /* see discussion in art.h */
    _remember_name(name);
}

int art_name_count(cptr name)
{
    return _name_count(name);
}

/******************************************************************************
 * Savefiles: Remember which names have been used
 ******************************************************************************/
void art_names_reset(void)
{
    str_map_clear(_names());
}

void art_names_save(savefile_ptr file)
{
    str_map_ptr      names = _names();
    str_map_iter_ptr iter;

    if (plr->wizard) /* XXX for my testing */
        str_map_clear(names);

    savefile_write_s16b(file, str_map_count(names));
    for (iter = str_map_iter_alloc(names);
            str_map_iter_is_valid(iter);
            str_map_iter_next(iter))
    {
        cptr name = str_map_iter_current_key(iter);
        int  ct = str_map_iter_current_int(iter);
        savefile_write_cptr(file, name);
        savefile_write_s16b(file, ct);
    }
    str_map_iter_free(iter);
}

void art_names_load(savefile_ptr file)
{
    str_map_ptr names = _names();
    int         ct = savefile_read_s16b(file);
    int         i;
    char        name[MAX_NLEN];

    for (i = 0; i < ct; i++)
    {
        int ct;
        savefile_read_cptr(file, name, MAX_NLEN);
        ct = savefile_read_s16b(file);
        str_map_add_int(names, name, ct);
    }
}

