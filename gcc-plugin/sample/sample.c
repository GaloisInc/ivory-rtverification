#include <stdio.h>

int a __attribute__((instrument(0)));
int b __attribute__((instrument(2)));
int c __attribute__((instrument(1))); /* Unused */

void f(void)
{
    a += 5;
}

int g(int x)
{
    return x+a;
}

/* we should probably catch cases like this at some point... */
void h(void)
{
    int *x = &a;
    *x = 99;
}

int main(int argc, const char *argv[])
{
    b = 10;
    a = 3;
    f();
    a = g(8);
    b = g(0);
    //h();
    printf("a = %d, b = %d\n", a, b);
    return 0;
}

/*
Order of updates:

b = 10
a = 3
a = 8
a = 16
b = 16

*/
