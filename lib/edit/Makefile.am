## Makefile.am -- Process this file with automake to process Makefile.in

angband_files = \
	a_info.txt e_info.txt f_info.txt k_info.txt m_info.txt \
	misc.txt r_info.txt v_info.txt w_info.txt \
	t_info.txt t_pref.txt t_basic.txt \
	q_info.txt q_morgoth.txt q_sauron.txt q_random.txt \
	q_avenge.txt

EXTRA_DIST = \
  $(angband_files)

if SET_GID
angbanddir = @DEFAULT_PATH@edit

angband_DATA = \
  $(angband_files)
endif

