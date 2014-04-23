#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include "exploration_graph.h"
#include "interleave.h"

#define MAX_NUM_TRIALS 1000
#define MAX_NUM_STRESS_TRIALS 10000

#ifndef ONLY_STRESS
#define ONLY_STRESS 0
#endif

void termination_handler (int signum) {
  printf("signum %d\n", signum);
  exit(1);
}

// prototype of test to run
int test_main (void);

int main (void) {
  struct sigaction new_action, old_action;

  new_action.sa_handler = termination_handler;
  sigemptyset (&new_action.sa_mask);
  new_action.sa_flags = 0;

  sigaction (SIGTERM, NULL, &old_action);
  if (old_action.sa_handler != SIG_IGN)
    sigaction (SIGTERM, &new_action, NULL);
  sigaction (SIGSEGV, NULL, &old_action);
  if (old_action.sa_handler != SIG_IGN)
    sigaction (SIGSEGV, &new_action, NULL);
  sigaction (SIGABRT, NULL, &old_action);
  if (old_action.sa_handler != SIG_IGN)
    sigaction (SIGABRT, &new_action, NULL);

  int i;
#if !ONLY_STRESS
  for (i = 0; i < MAX_NUM_TRIALS; i++) {
#else
  for (i = 0; i < MAX_NUM_STRESS_TRIALS; i++) {
#endif
    printf("BEGIN_PATH %d\n", i);
#if !ONLY_STRESS
    reset_curr_node();
    reset_thread_map();
#endif
    test_main ();
    printf("END PATH %d\n", i);
  }
  return 0;
}
