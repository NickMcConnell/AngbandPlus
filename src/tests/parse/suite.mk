TESTPROGS += parse/a-info \
             parse/c-info \
             parse/e-info \
	     parse/f-info \
	     parse/flavor \
	     parse/h-info \
             parse/names \
             parse/parser \
             parse/k-info \
	     parse/owner \
	     parse/p-info \
	     parse/r-info \
	     parse/s-info \
	     parse/store \
	     parse/v-info \
	     parse/z-info

parse/a-info: parse/a-info.c ../reposband.o
parse/c-info: parse/c-info.c ../reposband.o
parse/e-info: parse/e-info.c ../reposband.o
parse/f-info: parse/f-info.c ../reposband.o
parse/flavor: parse/flavor.c ../reposband.o
parse/h-info: parse/h-info.c ../reposband.o
parse/names: parse/names.c ../reposband.o
parse/parser: parse/parser.c ../reposband.o
parse/k-info: parse/k-info.c ../reposband.o
parse/owner: parse/owner.c ../reposband.o
parse/p-info: parse/p-info.c ../reposband.o
parse/r-info: parse/r-info.c ../reposband.o
parse/s-info: parse/s-info.c ../reposband.o
parse/store: parse/store.c ../reposband.o
parse/v-info: parse/v-info.c ../reposband.o
parse/z-info: parse/z-info.c ../reposband.o
