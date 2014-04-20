#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <assert.h>

int *global_var = NULL;

void * child_code (void *a) {
  int __sm_thread_id = 1;
  *global_var = *global_var + 1;
}

int test_main (void) {
  int __sm_thread_id = 0;

  pthread_t child;
  pthread_create(&child, NULL, child_code, NULL);

  global_var = (int *) malloc(sizeof(int));
  *global_var = 1;

  void **ret = NULL;
  pthread_join(child, ret);

  assert(*global_var == 2);
}
