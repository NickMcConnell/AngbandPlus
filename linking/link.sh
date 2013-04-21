file_list=''
wfile_list=''
mod_list=''
if test -r gen-clisp.c; then
  file_list="$file_list"' gen-clisp.o'
  wfile_list="$wfile_list"' wgen-clisp.o'
  mod_list="$mod_list"' gen_clisp'
fi
make clisp-module CC="${CC}" CFLAGS="${CFLAGS}" INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list liblang_ui.so"
NEW_LIBS="$file_list liblang_ui.so"
NEW_MODULES="$mod_list"
TO_LOAD=''
