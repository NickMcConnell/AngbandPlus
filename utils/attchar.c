#include <stdio.h>

int main(int argc, char ** argv)
{
    if(argc == 2) {
        int d;
        sscanf(argv[1], "%d", &d);
        
        printf("0x%x:0x%x\n", 
               (((d >> 7) & 0x3) << 5) | 0x80, 
               ((d & 0x7F) | 0x80));
    } else {
        printf("Usage: %s <n>\n", argv[0]);
    }
}