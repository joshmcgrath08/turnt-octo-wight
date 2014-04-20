#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <assert.h>
#include "merge_sort.h"

#define MAX_NUM_THREADS 10
#define MIN_SPLIT_SIZE 16

int num_threads = 1;
pthread_mutex_t num_threads_mutex = PTHREAD_MUTEX_INITIALIZER;

int tid = 0;
pthread_mutex_t tid_mutex = PTHREAD_MUTEX_INITIALIZER;

void merge_sort(int *buf, int size) {
  int __sm_thread_id = tid;
  merge_sort_args *msa = (merge_sort_args *) malloc(sizeof(merge_sort_args));
  msa->buf = buf;
  msa->size = size;
  msa->tid = tid;
  tid += 1;

  merge_sort_aux(msa);
  free(msa);
}

void * merge_sort_aux(void *args) {
  merge_sort_args *msa = (merge_sort_args *) args;
  int __sm_thread_id = msa->tid;
  pthread_mutex_lock(&num_threads_mutex);

  if (msa->size >= MIN_SPLIT_SIZE && num_threads < MAX_NUM_THREADS - 1) {
    num_threads += 2;
    pthread_mutex_unlock(&num_threads_mutex);

    merge_sort_args *msa_1 =
      (merge_sort_args *) malloc(sizeof(merge_sort_args));
    merge_sort_args *msa_2 =
      (merge_sort_args *) malloc(sizeof(merge_sort_args));

    msa_1->buf = msa->buf;
    msa_1->size = (msa->size / 2) + (msa->size % 2);
    pthread_mutex_lock(&tid_mutex);
    msa_1->tid = tid;
    tid += 1;
    pthread_mutex_unlock(&tid_mutex);
    msa_2->buf = msa->buf + msa_1->size;
    msa_2->size = msa->size - msa_1->size;
    pthread_mutex_lock(&tid_mutex);
    msa_2->tid = tid;
    tid += 1;
    pthread_mutex_unlock(&tid_mutex);

    pthread_t *t_1 = malloc(sizeof(pthread_t));
    pthread_t *t_2 = malloc(sizeof(pthread_t));

    pthread_create(t_1, NULL, merge_sort_aux, (void *) msa_1);
    pthread_create(t_2, NULL, merge_sort_aux, (void *) msa_2);

    void **ret = NULL;
    pthread_join(*t_1, ret);
    pthread_join(*t_2, ret);

    merge(msa_1, msa_2);

    pthread_mutex_lock(&num_threads_mutex);
    num_threads -= 2;
    pthread_mutex_unlock(&num_threads_mutex);

    free(msa_1);
    free(msa_2);
    free(t_1);
    free(t_2);
  }
  else if (msa->size > 1) {
    pthread_mutex_unlock(&num_threads_mutex);

    merge_sort_args *msa_1 =
      (merge_sort_args *) malloc(sizeof(merge_sort_args));
    merge_sort_args *msa_2 =
      (merge_sort_args *) malloc(sizeof(merge_sort_args));

    msa_1->buf = msa->buf;
    msa_1->size = (msa->size / 2) + (msa->size % 2);
    msa_2->buf = msa->buf + msa_1->size;
    msa_2->size = msa->size - msa_1->size;

    merge_sort_aux(msa_1);
    merge_sort_aux(msa_2);

    merge(msa_1, msa_2);

    free(msa_1);
    free(msa_2);
  }
  else {
    pthread_mutex_unlock(&num_threads_mutex);
  }
  return NULL;
}

void merge(merge_sort_args *msa_1, merge_sort_args *msa_2) {
  assert(msa_1->buf + msa_1->size == msa_2->buf);
  int *scratch =
    (int *) malloc(sizeof(int) * (msa_1->size + msa_2->size));

  int itr_1 = 0;
  int itr_2 = 0;
  int itr = 0;
  int val;

  while (itr_1 < msa_1->size || itr_2 < msa_2->size) {
    if (itr_1 >= msa_1->size) {
      val = *(msa_2->buf + itr_2);
      itr_2++;
    }
    else if (itr_2 >= msa_2->size) {
      val = *(msa_1->buf + itr_1);
      itr_1++;
    }
    else if (*(msa_1->buf + itr_1) < *(msa_2->buf + itr_2)) {
      val = *(msa_1->buf + itr_1);
      itr_1++;
    }
    else {
      val = *(msa_2->buf + itr_2);
      itr_2++;
    }
    scratch[itr] = val;
    itr++;
  }

  itr = 0;
  while (itr < msa_1->size) {
    *(msa_1->buf + itr) = scratch[itr];
    itr++;
  }

  while (itr < msa_1->size + msa_2->size) {
    *(msa_2->buf + itr - msa_1->size) = scratch[itr];
    itr++;
  }

  free(scratch);
}
