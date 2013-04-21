#!/bin/sh

THEFILES="
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

BASEPATH=/home/stig/wiper
#BASEPATH=/home/stig/tmp

GENERATEPATH=doc
CSFFILE=${GENERATEPATH}/lispy-out.xml
SDOCFILE=${GENERATEPATH}/lang-out.sdoc
PREFSFILE=tools/sds-prefs.xml
DSSSLFILE=${BASEPATH}/share/sds/xml/sdoc.dsl
XMLDECL=${BASEPATH}/share/sds/xml/xml.dcl

${BASEPATH}/bin/lisp2csf -v -p ${PREFSFILE} -o ${CSFFILE} ${THEFILES}
${BASEPATH}/bin/csf2sdoc -v -V -o ${SDOCFILE} -p ${PREFSFILE} -r ${CSFFILE}
${BASEPATH}/bin/sdoc2doc -v -f docbook -p ${PREFSFILE} -r ${SDOCFILE}
cd ${GENERATEPATH} && jade -d ${DSSSLFILE} -t sgml ${XMLDECL} book.xml
