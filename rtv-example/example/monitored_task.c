#include <stdio.h>

int a __attribute__((instrument(0))) = 1;
int b __attribute__((instrument(1))) = 100;

void update(void) {
  printf("Values: %d, %d\n", a, b);
  a = a*10;
  b++;
}


