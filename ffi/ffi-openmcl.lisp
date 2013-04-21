;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(ccl::def-foreign-type cptr :address)


(defun c_current_ui nil
  (ccl::external-call "_current_ui" :signed-fullword))

(defun c-listen-for-event (option)
  (ccl::external-call "_listenForEvent" :signed-fullword option :signed-fullword))

(defun init-c-side& (ui source-path config-path data-path flags)
 (ccl::with-cstr (ui1373 ui)
 (ccl::with-cstr (source-path1374 source-path)
 (ccl::with-cstr (config-path1375 config-path)
 (ccl::with-cstr (data-path1376 data-path)
  (ccl::external-call "_init_c_side" :address ui1373 :address source-path1374 :address config-path1375 :address data-path1376 :signed-fullword flags :signed-fullword))))))

(defun cleanup-c-side& nil
  (ccl::external-call "_cleanup_c_side" :signed-fullword))

(defun c-set-lisp-system! (type)
  (ccl::external-call "_set_lisp_system" :signed-fullword type :void))


#+use-callback-from-c
(defun c-set-lisp-callback! (name ptr)
 (ccl::with-cstr (name1377 name)
  (ccl::external-call "_set_lisp_callback" :address name1377 :address ptr :void)))

(defun c-init-sound-system& (size)
  (ccl::external-call "_init_sound_system" :signed-fullword size :signed-fullword))

(defun c-get-sound-status nil
  (ccl::external-call "_get_sound_status" :signed-fullword))

(defun c-load-sound-effect& (fname idx)
 (ccl::with-cstr (fname1378 fname)
  (ccl::external-call "_load_sound_effect" :address fname1378 :signed-fullword idx :signed-fullword)))

(defun c-play-sound-effect (idx)
  (ccl::external-call "_play_sound_effect" :signed-fullword idx :signed-fullword))

(defun c-load-music-file& (fname idx)
 (ccl::with-cstr (fname1379 fname)
  (ccl::external-call "_load_music_file" :address fname1379 :signed-fullword idx :signed-fullword)))

(defun c-play-music-file (idx)
  (ccl::external-call "_play_music_file" :signed-fullword idx :signed-fullword))


#+image-support
(defun load-gfx-image& (fname idx transcolour)
 (ccl::with-cstr (fname1380 fname)
  (ccl::external-call "_load_gfx_image" :address fname1380 :signed-fullword idx :unsigned-fullword transcolour :signed-fullword)))


#+image-support
(defun c-load-texture& (idx fname twid thgt alpha)
 (ccl::with-cstr (fname1381 fname)
  (ccl::external-call "_load_texture" :signed-fullword idx :address fname1381 :signed-fullword twid :signed-fullword thgt :unsigned-fullword alpha :signed-fullword)))


#+image-support
(defun c-get-image-width (idx)
  (ccl::external-call "_get_image_width" :signed-fullword idx :signed-fullword))


#+image-support
(defun c-get-image-height (idx)
  (ccl::external-call "_get_image_height" :signed-fullword idx :signed-fullword))

(defun c-init-frame-system& (act-size pre-size)
  (ccl::external-call "_init_frame_system" :signed-fullword act-size :signed-fullword pre-size :signed-fullword))

(defun c-add-frame! (key name)
 (ccl::with-cstr (name1382 name)
  (ccl::external-call "_add_frame" :signed-fullword key :address name1382 :signed-fullword)))

(defun c-add-frame-coords! (key x y w h)
  (ccl::external-call "_add_frame_coords" :signed-fullword key :signed-fullword x :signed-fullword y :signed-fullword w :signed-fullword h :signed-fullword))

(defun c-add-frame-tileinfo! (key tw th font)
 (ccl::with-cstr (font1383 font)
  (ccl::external-call "_add_frame_tileinfo" :signed-fullword key :signed-fullword tw :signed-fullword th :address font1383 :signed-fullword)))

(defun c-add-frame-gfxinfo! (key use-tiles)
  (ccl::external-call "_add_frame_gfxinfo" :signed-fullword key :signed-fullword use-tiles :signed-fullword))

(defun c-add-frame-bg! (key img-idx)
  (ccl::external-call "_add_frame_bg" :signed-fullword key :signed-fullword img-idx :signed-fullword))

(defun c-has_frame (key type)
  (ccl::external-call "_has_frame" :signed-fullword key :signed-fullword type :signed-fullword))

(defun c-get-frame-columns (key type)
  (ccl::external-call "_get_frame_columns" :signed-fullword key :signed-fullword type :signed-fullword))

(defun c-get-frame-rows (key type)
  (ccl::external-call "_get_frame_rows" :signed-fullword key :signed-fullword type :signed-fullword))

(defun c-get-frame-tile-width (key type)
  (ccl::external-call "_get_frame_tile_width" :signed-fullword key :signed-fullword type :signed-fullword))

(defun c-get-frame-tile-height (key type)
  (ccl::external-call "_get_frame_tile_height" :signed-fullword key :signed-fullword type :signed-fullword))

(defun c-get_frame-gfx-tiles (key type)
  (ccl::external-call "_get_frame_gfx_tiles" :signed-fullword key :signed-fullword type :signed-fullword))

(defun c-full-blit (num x y img flag)
  (ccl::external-call "_exp_full_blit" :signed-halfword num :signed-halfword x :signed-halfword y :unsigned-fullword img :signed-halfword flag :signed-fullword))

(defun c-transparent-blit (num x y img flag)
  (ccl::external-call "_exp_transparent_blit" :signed-halfword num :signed-halfword x :signed-halfword y :unsigned-fullword img :signed-halfword flag :signed-fullword))

(defun c-clear-coords! (num x y w h)
  (ccl::external-call "_exp_clear_coords" :signed-halfword num :signed-halfword x :signed-halfword y :signed-halfword w :signed-halfword h :signed-fullword))

(defun c-flush-coords! (num x y w h)
  (ccl::external-call "_exp_flush_coords" :signed-halfword num :signed-halfword x :signed-halfword y :signed-halfword w :signed-halfword h :signed-fullword))


(eval-when (:execute :load-toplevel :compile-toplevel)
  (export
   '(c_current_ui c-listen-for-event init-c-side& cleanup-c-side&
     c-set-lisp-system! c-set-lisp-callback! c-init-sound-system&
     c-get-sound-status c-load-sound-effect& c-play-sound-effect
     c-load-music-file& c-play-music-file load-gfx-image& c-load-texture&
     c-get-image-width c-get-image-height c-init-frame-system& c-add-frame!
     c-add-frame-coords! c-add-frame-tileinfo! c-add-frame-gfxinfo!
     c-add-frame-bg! c-has_frame c-get-frame-columns c-get-frame-rows
     c-get-frame-tile-width c-get-frame-tile-height c-get_frame-gfx-tiles
     c-full-blit c-transparent-blit c-clear-coords! c-flush-coords!)))

;;; End of generated file.
