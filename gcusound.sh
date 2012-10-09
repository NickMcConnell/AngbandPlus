#!/bin/sh
date >> /home/jurriaan/games/myang/sound.log
pwd >> /home/jurriaan/games/myang/sound.log
wavplay $1 >/dev/null 2>&1 &
exit 0
