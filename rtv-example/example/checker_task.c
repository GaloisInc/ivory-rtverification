#include <stdio.h>
#include <stdlib.h>
#include "runtime-checker.h"

void verify_updates(void) {
  if (!check_properties()) {
    printf("Property failed!  Aborting...\n");
    exit(1);
  } else printf("Property passed.\n");
}
