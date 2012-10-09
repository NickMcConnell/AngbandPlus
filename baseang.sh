#!/bin/sh
name=$1
if [ "$name" = "" ]
then
   nr=`ls lib/save | cut -d"." -f2 | sort | uniq | wc -l`
   if [ $nr == 1 ]
   then
      name=`ls lib/save | cut -d"." -f2 | sort | uniq`
      echo using savefile $name
   else
      echo "Enter savefile name, currently known:"
      ls lib/save | cut -d"." -f2 | sort | uniq
      exit
   fi
fi
cd /home/jurriaan/games/myang
setterm -inversescreen off
test -f angband.log && mv angband.log alog`date +%m%d%H.%M`
#If you are a normal user, try this line
#/usr/bin/angband64-beta-__version__-__release__ -x/usr/local/games/ang64/angband.log -qFLOW,EXTRA -t -g -u$name 2>angband.err
#but for hardcore developers, this is it:
echo "file /usr/bin/angband64-beta-__version__-__release__" > /tmp/gdbinput._$$
echo "run -x/usr/local/games/ang64/angband.log -qFLOW,EXTRA -t -g -u$name" >> /tmp/gdbinput._$$
gdb -x /tmp/gdbinput._$$
rm /tmp/gdbinput._$$
setterm -inversescreen on
cat angband.err
