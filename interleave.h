#ifndef __INTERLEAVE__
#define __INTERLEAVE__

#include <pthread.h>

typedef enum { READ_USE, WRITE_USE } var_use;

void interleave_schedule_point(var_use vu, char *_f,
                               int _l, char *_x);

int interleave_pthread_create(pthread_t *thread, const pthread_attr_t *attr,
                              void *(*start_routine) (void *), void *arg);

int interleave_pthread_join(pthread_t thread, void **retval);

int interleave_pthread_mutex_lock(pthread_mutex_t *mutex);

int interleave_pthread_mutex_unlock(pthread_mutex_t *mutex);

void reset_thread_map();

typedef struct {
  void *(*start_routine) (void *);
  void *arg;
} insert_thread_args;

#endif // __INTERLEAVE__
