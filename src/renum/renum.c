#include <stdio.h>

int main(void)
{
int c, i = 0, last = getchar();

/* Yeah, yeah, I should use "for(...)", but I'm a lazy Bum ;*/
while((c = getchar()) != EOF)
  {
  if(last == '\n' && c == 'N')
    {
    printf("\nN:%d", i);
    ++i;
    getchar();
    last = getchar();
    }
  else
    {
    putchar(last);
    last = c;
    }
  }
}