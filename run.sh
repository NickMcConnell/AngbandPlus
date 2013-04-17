#!/bin/csh

# This shell script runs CatH-Angband nicely in a 1024x768 window.
# Describe attempt
echo "Launching CatH-Angband..."

#fonts
#5x7
#5x8
#6x10
#6x12
#6x13
#6x13bold
#6x9
#7x13
#7x13bold
#7x14
#7x14bold
#8x13
#8x13bold
#8x16
#9x15
#9x15bold

# Main window
setenv ANGBAND_X11_FONT_0 7x13
setenv ANGBAND_X11_AT_X_0 0
setenv ANGBAND_X11_AT_Y_0 0

# Overhead View
setenv ANGBAND_X11_FONT_1 5x8
setenv ANGBAND_X11_AT_X_1 405
setenv ANGBAND_X11_AT_Y_1 340
setenv ANGBAND_X11_ROWS_1 24

# Inventory window
setenv ANGBAND_X11_FONT_2 5x8
setenv ANGBAND_X11_AT_X_2 0
setenv ANGBAND_X11_AT_Y_2 465
setenv ANGBAND_X11_ROWS_3 23

# Equipment window
setenv ANGBAND_X11_FONT_3 5x8
setenv ANGBAND_X11_AT_X_3 0
setenv ANGBAND_X11_AT_Y_3 340
setenv ANGBAND_X11_ROWS_3 12

# Monster Recall window
setenv ANGBAND_X11_FONT_4 5x8
setenv ANGBAND_X11_AT_X_4 570
setenv ANGBAND_X11_AT_Y_4 220
#setenv ANGBAND_X11_COLS_4 76
setenv ANGBAND_X11_ROWS_4 12

# Character (main) window
setenv ANGBAND_X11_FONT_5 5x8
setenv ANGBAND_X11_AT_X_5 570
setenv ANGBAND_X11_AT_Y_5 0
setenv ANGBAND_X11_COLS_5 76
setenv ANGBAND_X11_ROWS_5 24

# Borg Messages Window
setenv ANGBAND_X11_FONT_6 5x7
setenv ANGBAND_X11_AT_X_6 405
setenv ANGBAND_X11_AT_Y_6 550
setenv ANGBAND_X11_ROWS_6 24

# The build directory
cd ./

# Gamma correction
setenv ANGBAND_X11_GAMMA 142

# Launch Angband
  ./src/angband -mx11 -- -n7 &

