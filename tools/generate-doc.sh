#!/bin/sh

LISPFILES="
package.lisp
base.lisp
constants.lisp
variables.lisp
core.lisp
lib/foreign/cmu.lisp
parameters.lisp
global.lisp
settings.lisp
level.lisp
floor.lisp
stat.lisp
race.lisp
class.lisp
object.lisp
equipment.lisp
player.lisp
birth.lisp
flavours.lisp
monster.lisp
dungeon.lisp
stores.lisp
allocate.lisp
rooms.lisp
generate.lisp
print.lisp
util.lisp
combat.lisp
keys.lisp
actions.lisp
lib/common/parameters.lisp
lib/common/objects.lisp
lib/common/quirks.lisp
lib/common/rooms.lisp
lib/common/keys.lisp
view.lisp
loop.lisp
init.lisp
lib/vanilla/base.lisp
"

BASEPATH=/home/stig/tmp

${BASEPATH}/bin/lisp2csf -v -p tools/sds-prefs.xml -o doc/lispy-out.xml ${LISPFILES}
${BASEPATH}/bin/csf2sdoc -v -V -o doc/lang-out.sdoc -p tools/sds-prefs.xml -r doc/lispy-out.xml
#${BASEPATH}/bin/sdoc2doc -v -f docbook -p tools/sds-prefs.xml -r doc/lang-out.sdoc
#cd doc && jade -d ${BASEPATH}/share/sds/xml/sdoc.dsl -t sgml ${BASEPATH}/share/sds/xml/xml.dcl book.xml
