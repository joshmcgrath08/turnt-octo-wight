## To use, set the following:
## SRCS - source files to instrument
## VARS - variables to instrument
## PATH_TO_MAKEFILE_INC - path to this file (no trailing '/')

HERE=$(PATH_TO_MAKEFILE_INC)
EXE=main
TOOL_SRCS=$(HERE)/interleave.c $(HERE)/interleave_main.c \
	$(HERE)/exploration_graph.c

.PHONY: stress
stress: CPP_FLAGS=-DONLY_STRESS=1
stress: stress_$(EXE)
	./$(EXE)

.PHONY: pause
pause: CPP_FLAGS=-DONLY_PAUSE=1
pause: instrumented_$(EXE)
	./$(EXE)

.PHONY: instrumented
instrumented: CPP_FLAGS=
instrumented: instrumented_$(EXE)
	./$(EXE)

.PHONY: instrumented_$(EXE)
instrumented_$(EXE): $(foreach s,$(SRCS),instrumented_$(s)) $(TOOL_SRCS)
	gcc -o $(EXE) -pthread $(CPP_FLAGS) $^

.PHONY: stress_$(EXE)
stress_$(EXE): $(SRCS) $(TOOL_SRCS)
	gcc -o $(EXE) -pthread $(CPP_FLAGS) $^

instrumented_%: %
	runghc $(HERE)/Main.hs $* instrumented_$* $(VARS)

clean:
	rm -f $(EXE) *.o
	rm -f $(foreach s,$(SRCS),instrumented_$(s))
