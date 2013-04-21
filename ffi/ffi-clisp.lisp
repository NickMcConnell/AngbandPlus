;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(ffi:def-c-type cptr c-string)


(ffi:def-call-out c_current_ui (:name "current_ui") (:language :stdc) (:arguments ) (:return-type int))

(ffi:def-call-out c-listen-for-event (:name "listenForEvent") (:language :stdc) (:arguments (option
                                                                                             int)) (:return-type int))

(ffi:def-call-out init-c-side& (:name "init_c_side") (:language :stdc) (:arguments (ui
                                                                                    c-string)(source-path
                                                                                              c-string)(config-path
                                                                                                        c-string)(data-path
                                                                                                                  c-string)(flags
                                                                                                                            int)) (:return-type int))

(ffi:def-call-out cleanup-c-side& (:name "cleanup_c_side") (:language :stdc) (:arguments ) (:return-type int))

(ffi:def-call-out c-set-lisp-system! (:name "set_lisp_system") (:language :stdc) (:arguments (type
                                                                                              int)) (:return-type nil))


#+use-callback-from-c
(ffi:def-call-out c-set-lisp-callback! (:name "set_lisp_callback") (:language :stdc) (:arguments (name
                                                                                                  c-string)(ptr
                                                                                                            c-pointer)) (:return-type nil))

(ffi:def-call-out c-init-sound-system& (:name "init_sound_system") (:language :stdc) (:arguments (size
                                                                                                  int)) (:return-type int))

(ffi:def-call-out c-get-sound-status (:name "get_sound_status") (:language :stdc) (:arguments ) (:return-type int))

(ffi:def-call-out c-load-sound-effect& (:name "load_sound_effect") (:language :stdc) (:arguments (fname
                                                                                                  c-string)(idx
                                                                                                            int)) (:return-type int))

(ffi:def-call-out c-play-sound-effect (:name "play_sound_effect") (:language :stdc) (:arguments (idx
                                                                                                 int)) (:return-type int))

(ffi:def-call-out c-load-music-file& (:name "load_music_file") (:language :stdc) (:arguments (fname
                                                                                              c-string)(idx
                                                                                                        int)) (:return-type int))

(ffi:def-call-out c-play-music-file (:name "play_music_file") (:language :stdc) (:arguments (idx
                                                                                             int)) (:return-type int))


#+image-support
(ffi:def-call-out load-gfx-image& (:name "load_gfx_image") (:language :stdc) (:arguments (fname
                                                                                          c-string)(idx
                                                                                                    int)(transcolour
                                                                                                         uint)) (:return-type int))


#+image-support
(ffi:def-call-out c-load-texture& (:name "load_texture") (:language :stdc) (:arguments (idx
                                                                                        int)(fname
                                                                                             c-string)(twid
                                                                                                       int)(thgt
                                                                                                            int)(alpha
                                                                                                                 uint)) (:return-type int))


#+image-support
(ffi:def-call-out c-get-image-width (:name "get_image_width") (:language :stdc) (:arguments (idx
                                                                                             int)) (:return-type int))


#+image-support
(ffi:def-call-out c-get-image-height (:name "get_image_height") (:language :stdc) (:arguments (idx
                                                                                               int)) (:return-type int))

(ffi:def-call-out c-init-frame-system& (:name "init_frame_system") (:language :stdc) (:arguments (act-size
                                                                                                  int)(pre-size
                                                                                                       int)) (:return-type int))

(ffi:def-call-out c-add-frame! (:name "add_frame") (:language :stdc) (:arguments (key
                                                                                  int)(name
                                                                                       c-string)) (:return-type int))

(ffi:def-call-out c-add-frame-coords! (:name "add_frame_coords") (:language :stdc) (:arguments (key
                                                                                                int)(x
                                                                                                     int)(y
                                                                                                          int)(w
                                                                                                               int)(h
                                                                                                                    int)) (:return-type int))

(ffi:def-call-out c-add-frame-tileinfo! (:name "add_frame_tileinfo") (:language :stdc) (:arguments (key
                                                                                                    int)(tw
                                                                                                         int)(th
                                                                                                              int)(font
                                                                                                                   c-string)) (:return-type int))

(ffi:def-call-out c-add-frame-gfxinfo! (:name "add_frame_gfxinfo") (:language :stdc) (:arguments (key
                                                                                                  int)(use-tiles
                                                                                                       int)) (:return-type int))

(ffi:def-call-out c-add-frame-bg! (:name "add_frame_bg") (:language :stdc) (:arguments (key
                                                                                        int)(img-idx
                                                                                             int)) (:return-type int))

(ffi:def-call-out c-has_frame (:name "has_frame") (:language :stdc) (:arguments (key
                                                                                 int)(type
                                                                                      int)) (:return-type int))

(ffi:def-call-out c-get-frame-columns (:name "get_frame_columns") (:language :stdc) (:arguments (key
                                                                                                 int)(type
                                                                                                      int)) (:return-type int))

(ffi:def-call-out c-get-frame-rows (:name "get_frame_rows") (:language :stdc) (:arguments (key
                                                                                           int)(type
                                                                                                int)) (:return-type int))

(ffi:def-call-out c-get-frame-tile-width (:name "get_frame_tile_width") (:language :stdc) (:arguments (key
                                                                                                       int)(type
                                                                                                            int)) (:return-type int))

(ffi:def-call-out c-get-frame-tile-height (:name "get_frame_tile_height") (:language :stdc) (:arguments (key
                                                                                                         int)(type
                                                                                                              int)) (:return-type int))

(ffi:def-call-out c-get_frame-gfx-tiles (:name "get_frame_gfx_tiles") (:language :stdc) (:arguments (key
                                                                                                     int)(type
                                                                                                          int)) (:return-type int))

(ffi:def-call-out c-full-blit (:name "exp_full_blit") (:language :stdc) (:arguments (num
                                                                                     short)(x
                                                                                            short)(y
                                                                                                   short)(img
                                                                                                          uint)(flag
                                                                                                                short)) (:return-type int))

(ffi:def-call-out c-transparent-blit (:name "exp_transparent_blit") (:language :stdc) (:arguments (num
                                                                                                   short)(x
                                                                                                          short)(y
                                                                                                                 short)(img
                                                                                                                        uint)(flag
                                                                                                                              short)) (:return-type int))

(ffi:def-call-out c-clear-coords! (:name "exp_clear_coords") (:language :stdc) (:arguments (num
                                                                                            short)(x
                                                                                                   short)(y
                                                                                                          short)(w
                                                                                                                 short)(h
                                                                                                                        short)) (:return-type int))

(ffi:def-call-out c-flush-coords! (:name "exp_flush_coords") (:language :stdc) (:arguments (num
                                                                                            short)(x
                                                                                                   short)(y
                                                                                                          short)(w
                                                                                                                 short)(h
                                                                                                                        short)) (:return-type int))


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
