#ifndef __MERGE_SORT__
#define __MERGE_SORT__

typedef struct {
  int *buf;
  int size;
  int tid;
} merge_sort_args;

void merge_sort(int *buf, int size);

void * merge_sort_aux(void *args);

void merge(merge_sort_args *msa_1, merge_sort_args *msa_2);

#endif // __MERGE_SORT__
