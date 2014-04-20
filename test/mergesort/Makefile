SRCS=main.c merge_sort.c
VARS=num_threads tid
PATH_TO_MAKEFILE_INC=../..

include $(PATH_TO_MAKEFILE_INC)/Makefile.inc

.PHONY: main.c
main.c:
	runghc Test.hs > main.c
