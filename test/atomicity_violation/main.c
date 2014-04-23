#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <assert.h>

#define NUM_THREADS 5

int num_threads = 0;

pthread_mutex_t bad_mutex = PTHREAD_MUTEX_INITIALIZER;

void * child_code (void *a) {
  if (random() % NUM_THREADS == (NUM_THREADS - 1))
    num_threads +=1;
  else {
    pthread_mutex_lock(&bad_mutex);
    num_threads += 1;
    pthread_mutex_unlock(&bad_mutex);
  }
}

int test_main (void) {
  num_threads = 0;

  int i;
  pthread_t *ts = malloc (NUM_THREADS * sizeof(pthread_t));
  for (i = 0; i < NUM_THREADS; i++)
    pthread_create((ts + i), NULL, child_code, NULL);

  void **ret = NULL;
  for (i = 0; i < NUM_THREADS; i++)
    pthread_join(*(ts + i), ret);

  assert(num_threads == NUM_THREADS);

  free(ts);
}
