#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include "interleave.h"
#include "exploration_graph.h"

// determines whether we loop until we get a CONTINUE
// or exit the loop immediately. this is meant to allow
// us to compare just inserting pauses and actually
// trying to force different schedules
#ifndef ONLY_PAUSE
#define ONLY_PAUSE 0
#endif

pthread_mutex_t scheduler_mutex = PTHREAD_MUTEX_INITIALIZER;

void interleave_schedule_point(var_use vu, char *_f, int _l, char *_x, int _t) {
  direction res = CONTINUE;
  do {
    if (res != CONTINUE)
      usleep(10);

    pthread_mutex_lock(&scheduler_mutex);

    schedule_info *si = (schedule_info *) malloc(sizeof(schedule_info));
    si->vu = vu;
    si->file_name = _f;
    si->line_num = _l;
    si->var_name = _x;
    si->tid = _t;

    res = insert_node (si);

    printf("[interleave] %s:%d, var: %s (%s), tid: %d, %s\n",
           _f, _l, _x, vu == READ_USE ? "R" : "W" , _t,
           res == CONTINUE ? "CONTINUE" : "WAIT");

    pthread_mutex_unlock(&scheduler_mutex);

#if ONLY_PAUSE
  } while (0);
#else
  } while (res != CONTINUE);
#endif
  return;
}

int interleave_pthread_create(pthread_t *thread, const pthread_attr_t *attr,
                              void *(*start_routine) (void *), void *arg) {
  return pthread_create(thread, attr, start_routine, arg);
}

int interleave_pthread_join(pthread_t thread, void **retval) {
  return pthread_join(thread, retval);
}

int interleave_pthread_mutex_lock(pthread_mutex_t *mutex) {
  return pthread_mutex_lock(mutex);
}

int interleave_pthread_mutex_unlock(pthread_mutex_t *mutex) {
  return pthread_mutex_unlock(mutex);
}
