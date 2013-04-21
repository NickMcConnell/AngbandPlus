;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(ff:def-foreign-type cptr (* :char))


(ff:def-foreign-call (c_current_ui "current_ui") nil :returning :int)

(ff:def-foreign-call (c-listen-for-event "listenForEvent") ((option :int)) :returning :int)

(ff:def-foreign-call (init-c-side& "init_c_side") ((ui cptr) (source-path cptr)
                                                   (config-path cptr)
                                                   (gfx-path cptr) (flags :int)) :returning :int)

(ff:def-foreign-call (cleanup-c-side& "cleanup_c_side") nil :returning :int)

(ff:def-foreign-call (c-set-lisp-system! "set_lisp_system") ((type :int)) :returning :void)


#+use-callback-from-c
(ff:def-foreign-call (c-set-lisp-callback! "set_lisp_callback") ((name cptr)
                                                                 (ptr)) :returning :void)

(ff:def-foreign-call (c-init-sound-system& "init_sound_system") ((size :int)) :returning :int)

(ff:def-foreign-call (c-load-sound-effect& "load_sound_effect") ((fname cptr)
                                                                 (idx :int)) :returning :int)

(ff:def-foreign-call (c-get-sound-status "get_sound_status") nil :returning :int)

(ff:def-foreign-call (c-play-sound-effect "play_sound_effect") ((idx :int)) :returning :int)


#+image-support
(ff:def-foreign-call (load-gfx-image& "load_gfx_image") ((fname cptr)
                                                         (idx :int)
                                                         (transcolour
                                                          :unsigned-long)) :returning :int)


#+image-support
(ff:def-foreign-call (c-load-texture& "load_texture") ((idx :int) (fname cptr)
                                                       (twid :int) (thgt :int)
                                                       (alpha :unsigned-long)) :returning :int)


#+image-support
(ff:def-foreign-call (c-get-image-width "get_image_width") ((idx :int)) :returning :int)


#+image-support
(ff:def-foreign-call (c-get-image-height "get_image_height") ((idx :int)) :returning :int)

(ff:def-foreign-call (c-init-frame-system& "init_frame_system") ((act-size
                                                                  :int)
                                                                 (pre-size
                                                                  :int)) :returning :int)

(ff:def-foreign-call (c-add-frame! "add_frame") ((key :int) (name cptr)) :returning :int)

(ff:def-foreign-call (c-add-frame-coords! "add_frame_coords") ((key :int)
                                                               (x :int)
                                                               (y :int)
                                                               (w :int)
                                                               (h :int)) :returning :int)

(ff:def-foreign-call (c-add-frame-tileinfo! "add_frame_tileinfo") ((key :int)
                                                                   (tw :int)
                                                                   (th :int)
                                                                   (font cptr)) :returning :int)

(ff:def-foreign-call (c-add-frame-gfxinfo! "add_frame_gfxinfo") ((key :int)
                                                                 (use-tiles
                                                                  :int)) :returning :int)

(ff:def-foreign-call (c-add-frame-bg! "add_frame_bg") ((key :int)
                                                       (img-idx :int)) :returning :int)

(ff:def-foreign-call (c-has_frame "has_frame") ((key :int) (type :int)) :returning :int)

(ff:def-foreign-call (c-get-frame-columns "get_frame_columns") ((key :int)
                                                                (type :int)) :returning :int)

(ff:def-foreign-call (c-get-frame-rows "get_frame_rows") ((key :int)
                                                          (type :int)) :returning :int)

(ff:def-foreign-call (c-get-frame-tile-width "get_frame_tile_width") ((key
                                                                       :int)
                                                                      (type
                                                                       :int)) :returning :int)

(ff:def-foreign-call (c-get-frame-tile-height "get_frame_tile_height") ((key
                                                                         :int)
                                                                        (type
                                                                         :int)) :returning :int)

(ff:def-foreign-call (c-get_frame-gfx-tiles "get_frame_gfx_tiles") ((key :int)
                                                                    (type :int)) :returning :int)

(ff:def-foreign-call (c-full-blit "exp_full_blit") ((num :short) (x :short)
                                                    (y :short)
                                                    (img :unsigned-long)
                                                    (flag :short)) :returning :int)

(ff:def-foreign-call (c-transparent-blit "exp_transparent_blit") ((num :short)
                                                                  (x :short)
                                                                  (y :short)
                                                                  (img
                                                                   :unsigned-long)
                                                                  (flag :short)) :returning :int)

(ff:def-foreign-call (c-clear-coords! "exp_clear_coords") ((num :short)
                                                           (x :short)
                                                           (y :short)
                                                           (w :short)
                                                           (h :short)) :returning :int)

(ff:def-foreign-call (c-flush-coords! "exp_flush_coords") ((num :short)
                                                           (x :short)
                                                           (y :short)
                                                           (w :short)
                                                           (h :short)) :returning :int)


(eval-when (:execute :load-toplevel :compile-toplevel)
  (export
   '(c_current_ui c-listen-for-event init-c-side& cleanup-c-side&
     c-set-lisp-system! c-set-lisp-callback! c-init-sound-system&
     c-load-sound-effect& c-get-sound-status c-play-sound-effect
     load-gfx-image& c-load-texture& c-get-image-width c-get-image-height
     c-init-frame-system& c-add-frame! c-add-frame-coords!
     c-add-frame-tileinfo! c-add-frame-gfxinfo! c-add-frame-bg! c-has_frame
     c-get-frame-columns c-get-frame-rows c-get-frame-tile-width
     c-get-frame-tile-height c-get_frame-gfx-tiles c-full-blit
     c-transparent-blit c-clear-coords! c-flush-coords!)))

;;; End of generated file.
