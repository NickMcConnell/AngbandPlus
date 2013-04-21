#!/usr/bin/python


from highscore import highscore
from sys import argv, exit


if len(argv) != 2:
    print 'Usage: highscore-dump <highscore file>'
    exit(1)

l = highscore(argv[1])

print '\nKamband 2.0 Highscore List:'
print '---------------------------\n'

__map = { 'm' : 'He', 'f' : 'She', 'n' : 'It' }

for x in l:
    if x['dungeon'] == 0:
        where = 'town'
    else:
        where = x['location']

    print '%s the %s %s.' % (x['name'], x['race'], x['class'])

    print 'Killed by %s in the %s.' % (x['death'], where)

    if x['level'] != x['max-level']:
        print ('%s attained level %d (max %d), earned %d GP '
               'and spent %d turns.') % \
               (__map[x['sex']], x['level'], x['max-level'], x['gold'],
                x['turns'])
    else:
        print '%s attained level %d, earned %d GP and spent %d turns.' % \
               (__map[x['sex']], x['level'], x['gold'], x['turns'])

    if where == 'dungeon':
        if x['max-dungeon'] != x['dungeon']:
            print ('Death occured on dungeon level %d, '
                   'with lowest dungeon level %d.') % \
                   (x['dungeon'], x['max-dungeon'])
        else:
            print 'Death occured on dungeon level %d.' % x['dungeon']
            
    elif x['max-dungeon'] != 0:
        print 'Lowest reached dungeon level was %d.' % x['max-dungeon']

    print 'Total score: %d' % x['score']

    print

