#ifndef __EXPLORATION_GRAPH__
#define __EXPLORATION_GRAPH__

#include "interleave.h"

typedef struct {
  var_use vu;
  char *file_name;
  int line_num;
  char *var_name;
  int tid;
} schedule_info;

int schedule_info_eq (schedule_info *si1, schedule_info *si2);

void print_schedule_info (schedule_info *si);

typedef struct node {
  schedule_info *si;
  int num_continues;
  struct node **continues;
  int num_waits;
  struct node **waits;
} node;

typedef enum { CONTINUE, WAIT } direction;

direction insert_node (schedule_info *si);

void maybe_init ();

node * new_node (schedule_info *si);

void reset_curr_node ();

#endif // __EXPLORATION_GRAPH__
