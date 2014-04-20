#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include <assert.h>
#include "interleave.h"
#include "exploration_graph.h"

#define MAX_BRANCH 64

pthread_mutex_t graph_mutex = PTHREAD_MUTEX_INITIALIZER;

node *root = NULL;
node *curr_node = NULL;

int schedule_info_eq (schedule_info *si1, schedule_info *si2) {
  return ((si1->vu == si2->vu) &&
          (strcmp(si1->file_name, si2->file_name) == 0) &&
          (si1->line_num == si2->line_num) &&
          (strcmp(si1->var_name, si2->var_name) == 0) &&
          (si1->tid == si2->tid));
}

void print_schedule_info (schedule_info *si) {
  if (si == NULL)
    printf("schedule info NULL\n");
  else
    printf("schedule info vu = %d, file = %s, line = %d, var = %s, tid = %d\n",
           si->vu, si->file_name, si->line_num, si->var_name, si->tid);
}

// insert a node into the currently-explored graph at curr,
// returning the direction (CONTINUE or WAIT)
direction insert_node (schedule_info *si) {
  maybe_init();
  pthread_mutex_lock(&graph_mutex);

  int continued_index = -1;
  int waited_index = -1;
  int i;

  // find index (if any) where we've continued from the current
  // schedule info
  for (i = 0; i < curr_node->num_continues; i++) {
    if (schedule_info_eq((*(curr_node->continues + i))->si, si)) {
      continued_index = i;
      break;
    }
  }

  // find index (if any) where we've waited from the current
  // schedule info
   for (i = 0; i < curr_node->num_waits; i++) {
    if (schedule_info_eq((*(curr_node->waits + i))->si, si)) {
      waited_index = i;
      break;
    }
  }

  // if we've yet to continue from this point, continue
  if (continued_index < 0) {
    if (curr_node->num_continues < MAX_BRANCH) {
      node *n = new_node(si);
      *(curr_node->continues + curr_node->num_continues) = n;
      curr_node->num_continues += 1;
      curr_node = *(curr_node->continues + curr_node->num_continues - 1);
      pthread_mutex_unlock(&graph_mutex);
      return CONTINUE;
    }
    else {
      printf("couldn't insert another continue node\n");
      assert(0);
    }
  }
  // if we've yet to wait (but have continued) from this point,
  // try waiting
  else if (waited_index < 0) {
    if (curr_node->num_waits < MAX_BRANCH) {
      node *n = new_node(si);
      *(curr_node->waits + curr_node->num_waits) = n;
      curr_node->num_waits += 1;
      curr_node = *(curr_node->waits + curr_node->num_waits - 1);
      pthread_mutex_unlock(&graph_mutex);
      return WAIT;
    }
    else {
      printf("couldn't insert another wait node\n");
      assert(0);
    }
  }
  // if we've both waited and continued from this point,
  // make a random choice of which to do, advancing curre_node
  else {
    int r = random () %2;
    if (r == 0) {
      curr_node = *((curr_node->continues) + continued_index);
      pthread_mutex_unlock(&graph_mutex);
      return CONTINUE;
    }
    else {
      curr_node = *((curr_node->waits) + waited_index);
      pthread_mutex_unlock(&graph_mutex);
      return WAIT;
    }
  }
}

// initialize root and curr_node if not already non-NULL
void maybe_init () {
  if (root == NULL) {
    pthread_mutex_lock(&graph_mutex);
    if (root == NULL) {
      root = new_node(NULL);
      curr_node = root;
    }
    pthread_mutex_unlock(&graph_mutex);
  }
}

// create a new node with specified schedule info
node * new_node (schedule_info *si) {
  node *n = (node *) malloc(sizeof(node));
  n->si = si;
  n->num_continues = 0;
  n->continues = (node **) malloc(MAX_BRANCH * sizeof(node *));
  n->num_waits = 0;
  n->waits = (node **) malloc(MAX_BRANCH * sizeof(node *));
  return n;
}

// reset curr_node to root
void reset_curr_node () {
  maybe_init ();
  pthread_mutex_lock(&graph_mutex);
  curr_node = root;
  pthread_mutex_unlock(&graph_mutex);
}
