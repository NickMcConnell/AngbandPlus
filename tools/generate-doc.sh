#!/bin/sh

LISPFILES="package.lisp base.lisp birth.lisp "\
"class.lisp constants.lisp dungeon.lisp parameters.lisp flavours.lisp "\
"floor.lisp foreign.lisp game.lisp generate.lisp loop.lisp "\
"monster.lisp init.lisp object.lisp player.lisp "\
"print.lisp race.lisp rooms.lisp stat.lisp stores.lisp "\
"equipment.lisp variables.lisp settings.lisp"

BASEPATH=/home/stig/tmp

${BASEPATH}/bin/lisp2csf -v -p tools/sds-prefs.xml -o doc/lispy-out.xml ${LISPFILES}
${BASEPATH}/bin/csf2sdoc -v -V -o doc/lang-out.sdoc -p tools/sds-prefs.xml -r doc/lispy-out.xml
${BASEPATH}/bin/sdoc2doc -v -f docbook -p tools/sds-prefs.xml -r doc/lang-out.sdoc
cd doc && jade -d ${BASEPATH}/share/sds/xml/sdoc.dsl -t sgml ${BASEPATH}/share/sds/xml/xml.dcl book.xml
