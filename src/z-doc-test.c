#include "z-doc.h"

#include <assert.h>

void test(int width)
{
    doc_ptr doc = doc_alloc(width);

    doc_read_file(doc, stdin);
    doc_write_file(doc, stdout);

    doc_free(doc);
}

int main(int argc, char *argv[])
{
    int width = 72, i;

    for (i = 1; i < argc; i++)
    {
        if ( strcmp("-w", argv[i]) == 0
          || strcmp("--width", argv[i]) == 0 )
        {
            i++;
            if (i < argc)
                width = atoi(argv[i]);
        }
    }
    test(width);
    return 0;
}

/*
> gcc -O2 c-string.c c-vec.c str-map.c int-map.c z-doc.c z-doc-test.c
> time cat ../lib/help/* | ./a.out --width 80 > foo.txt; ls -lah foo.txt
real    0m0.057s (35ms reading/parsing, 22ms output)
user    0m0.066s
sys     0m0.000s
-rw-r--r-- 1 chris chris 1.5M Sep 21 15:55 foo.txt
*/

