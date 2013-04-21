#!/usr/bin/python


# Possible Keys:
# 
# score gold turns time name sex race class level dungeon max-level
# max-dungeon location death
#

from highscore import highscore
from sys import argv, exit

import string


if len(argv) != 3:
    print 'Usage: highscore-sort <highscore file> <key>'
    print 'Possible keys: score gold turns time name sex race class level'
    print '               dungeon max-level max-dungeon location death'
    exit(1)

def dump_on(list, key):
    l = {}
    
    for x in list:
        if x[key] in l.keys():
            l[x[key]] = l[x[key]] + 1
        else:
            l[x[key]] = 1

    d = {}

    for x in l.values():
        d[x] = None

    k = d.keys()
    k.sort()
    k.reverse()

    for x in k:
        d = []
        for y in l.keys():
            if l[y] == x:
                d.append(y)
        d.sort()

        for y in d:
            print '%2d:' % x, y



l = highscore(argv[1])

# Hack -- filter out unnecessary info.

for x in range(len(l)):
    d = l[x]['death']

    try:
        i = string.index(d, ", ")
        d = d[i+2:]
    except:
        pass

    try:
        i = string.index(d, " (")
        d = d[:i]
    except:
        pass

    l[x]['death'] = d


dump_on(l, argv[2])

    
