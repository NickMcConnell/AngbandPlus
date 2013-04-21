file_list=''
wfile_list=''
mod_list=''
if test -r ffi-clisp.c; then
  file_list="$file_list"' ffi-clisp.o'
  wfile_list="$wfile_list"' wffi-clisp.o'
  mod_list="$mod_list"' ffi_clisp'
fi
make clisp-module CC="${CC}" CFLAGS="${CFLAGS}" INCLUDES="$absolute_linkkitdir"
NEW_FILES="$file_list liblangband_ui.so liblangband_dc.so"
NEW_LIBS="$file_list liblangband_ui.so liblangband_dc.so"
NEW_MODULES="$mod_list"
TO_LOAD=''
