#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include <assert.h>
#include <sys/syscall.h>
#include "interleave.h"
#include "exploration_graph.h"

// determines whether we loop until we get a CONTINUE
// or exit the loop immediately. this is meant to allow
// us to compare just inserting pauses and actually
// trying to force different schedules
#ifndef ONLY_PAUSE
#define ONLY_PAUSE 0
#endif

pthread_mutex_t thread_map_mutex = PTHREAD_MUTEX_INITIALIZER;

// maintain an array of the tids of threads created by pthread_create
// initial thread (not created with pthread_create) is implicitly
// given id -1
int thread_map_count = 0;
#define MAX_THREAD_MAP_ENTRIES 1024
long thread_map[MAX_THREAD_MAP_ENTRIES];

// code to call before calling user program
void * insert_thread_map(void *_arg) {
  insert_thread_args *arg = (insert_thread_args *) _arg;
  pthread_mutex_lock(&thread_map_mutex);
  assert(thread_map_count < MAX_THREAD_MAP_ENTRIES - 1);
  thread_map[thread_map_count++] = syscall(SYS_gettid);
  pthread_mutex_unlock(&thread_map_mutex);
  arg->start_routine (arg->arg);
  return NULL;
}

// return index of current thread in thread_map or -1
// if not found
int get_thread_id() {
  int i;
  long t = syscall(SYS_gettid);
  for (i = 0; i < thread_map_count; i++) {
    if (thread_map[i] == t)
      return i;
  }
  return -1;
}

pthread_mutex_t scheduler_mutex = PTHREAD_MUTEX_INITIALIZER;

void interleave_schedule_point(var_use vu, char *_f, int _l, char *_x) {
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

    int _t = get_thread_id();
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
  insert_thread_args *it_arg = malloc(sizeof(insert_thread_args));
  it_arg->start_routine = start_routine;
  it_arg->arg = arg;
  return pthread_create(thread, attr, insert_thread_map, it_arg);
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

void reset_thread_map() {
  pthread_mutex_lock(&thread_map_mutex);
  thread_map_count = 0;
  pthread_mutex_unlock(&thread_map_mutex);
}
