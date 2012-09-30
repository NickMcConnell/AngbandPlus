## Makefile.am -- Process this file with automake to process Makefile.in

angband_files = \
  a_info.txt e_info.txt f_info.txt k_info.txt \
  misc.txt q_info.txt q_oberon.txt q_random.txt \
  q_serpen.txt r_info.txt v_info.txt t_info.txt \
  w_info.txt

EXTRA_DIST = \
  $(angband_files)

if SET_GID
angbanddir = @DEFAULT_PATH@edit

angband_DATA = \
  $(angband_files)
endif

