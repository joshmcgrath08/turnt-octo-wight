#include <stdio.h>
#include <stdlib.h>

int global_var = 0;

int test_main (void) {
  global_var += 1;
  printf("global_var %d\n", global_var);
  return 0;
}
