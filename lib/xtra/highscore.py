
from string import atoi

__race_map = ['human', 'half-elf', 'elf', 'hobbit', 'gnome', 'dwarf',
              'half-orc', 'half-troll', 'dunadan', 'high-elf', 'kobold',
              'mutant', 'ghost', 'munchkin', 'golem', 'leprechaun',
              'death-mold', 'vortex']

__class_map = ['warrior', 'mage', 'priest', 'rogue', 'ranger', 'paladin',
               'illusionist', 'corrupted', 'beastmaster', 'lycanthrope',
               'mimic', 'vampire', 'bard', 'necromancer', 'elemental',
               'avatar']

__ispec_map = ['dungeon', 'arena', 'quest', 'magic-arena', 'store',
               'wilderness']


def __highscore_entry(entry):
    ret = {}

    ret['score'] = atoi(entry[8:17])
    ret['gold'] = atoi(entry[18:27])
    ret['turns'] = atoi(entry[28:37])
    ret['time'] = entry[38:46]
    ret['name'] = ''

    for i in range(48, 65):
        if entry[i] != '\0':
            ret['name'] = ret['name'] + entry[i]

    ret['sex'] = entry[72]
    ret['race'] = __race_map[atoi(entry[74:76])]
    ret['class'] = __class_map[atoi(entry[77:79])]
    ret['level'] = atoi(entry[80:83])
    ret['dungeon'] = atoi(entry[84:87])
    ret['max-level'] = atoi(entry[88:91])
    ret['max-dungeon'] = atoi(entry[92:95])
    ret['location'] = __ispec_map[atoi(entry[112:115])]
    ret['death'] = ''

    for i in range(120, 152):
        if entry[i] != '\0':
            ret['death'] = ret['death'] + entry[i]

    return ret
    


def highscore(file):
    f = open(file)
    ret = []

    while 1:
        s = f.read(152)

        if s != '':
            ret.append(s)
        else:
            break

    for i in range(len(ret)):
        ret[i] = __highscore_entry(ret[i])

    return ret

