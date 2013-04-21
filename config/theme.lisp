#||
  This is a theme-file for Langband.  A Theme-file contains a (theme ...) with
  all the theme-information.  Theme-information missing will be substituted with
  defaults.  The legal keys are:
  +full-frame+, +gfxmap-frame+, +asciimap-frame+, +message-frame+, +charinfo-frame+,
  +misc-frame+, +inv-frame+ and +dialgoue-frame+

  Other constants that will be provided by Langband are:
    window.width - the width of the window (800 for SDL, may vary for GCU)
    window.height - the height of the window (600 for SDL, may vary for GCU)
    gfxtile.width - width of one graphical tile (32 typically)
    gfxtile.height - height of one graphical tile (32 typically) 

  GCU measurements are in characters, while SDL measurements are in pixels.

  Fonts are loaded from config/fonts/
  Backgrounds are loaded from graphics/textures/

  Good luck
||#

(theme "default-sdl"
       ;; which system does the theme apply to
       :system "sdl"
       :default-font "vga8x16.hex"
       
       (big      :key +full-frame+
		 :x 0 :y 0
		 :width window.width
		 :height window.height
		 :font "vga8x16.hex")
       
       (charinfo :key +charinfo-frame+
		 :x 0 :y 8
		 :width 112 :height 480
		 :background "textures/bumpi.bmp"
		 :font "vga8x16.hex")
       
       (gfxmap   :key +gfxmap-frame+
		 :x 112 :y 8
		 :width 688
		 :height 480
		 :tile-width gfxtiles.width
		 :tile-height gfxtiles.height
		 :font "vga8x16.hex"
		 ;;:font "lettergo.ttf"
		 ;;:font "augie.ttf"
		 ;;:font "nova.ttf"
		 :gfx-tiles? true)

       (asciimap :key +asciimap-frame+
		 :x 112 :y 8
		 :width 688
		 :height 480
		 :font "vga8x16.hex"
		 :gfx-tiles? false)

       
       (msg      :key +message-frame+
		 :x 0 :y 488
		 :width window.width
		 :height 32
		 :background "textures/bumpi.bmp"
		 :font "vga8x16.hex"
		 ;;:font "augie.ttf"
		 )
       
       (misc     :key +misc-frame+
		 :x 0 :y 520
		 :width window.width
		 :height 16
		 :background "textures/bumpi.bmp"
		 ;;:font #+win32 "vga8x16.hex" #-win32 "lettergo.ttf"
		 :font "vga8x16.hex"
		 )

       (inventory :key +inv-frame+
		  :x 0 :y 536
		  :width window.width
		  :height 64
		  :background "textures/invbg2.bmp"
		  :tile-width gfxtiles.width
		  :tile-height gfxtiles.height
		  :font "vga8x16.hex"
		  :gfx-tiles? true)
       
       (dialogue :key +dialogue-frame+
		 :x 0 :y 8
		 :width 800
		 :height 480
		 :font "vga8x16.hex"
		 :background "textures/woodfloor.png"
		 :gfx-tiles? false)
       )

(theme "default-gcu"
       ;; which system does the theme apply to
       :system "gcu"
       :default-font "xxx"
       
       (big      :key +full-frame+
		 :x 0 :y 0
		 :width window.width
		 :height window.height)
       
       (charinfo :key +charinfo-frame+
		 :x 0 :y 1
		 :width 13 :height 22)
       
       (map      :key +asciimap-frame+
		 :x 13 :y 1
		 :width 66
		 :height 22
		 :gfx-tiles? false)
       
       (msg      :key +message-frame+
		 :x 0 :y 0
		 :width window.width
		 :height 1)
       
       (misc     :key +misc-frame+
		 :x 0 :y 23
		 :width window.width
		 :height 1)

       (inventory :key +inv-frame+
		  :x 0 :y 0
		  :width window.width
		  :height 1)
       (dialogue :key +dialogue-frame+
		 :x 0 :y 1
		 :width window.width
		 :height 22)
       )
