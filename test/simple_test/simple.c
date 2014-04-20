#include <stdio.h>
#include <stdlib.h>
#include "../interleave.h"

int global_var = 0;

int test_main (void) {
  int __sm_thread_id = 0;
  global_var += 1;
  printf("global_var %d\n", global_var);
  return 0;
}
