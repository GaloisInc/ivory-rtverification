CC      = gcc
OUT_DIR = out
SRCS		= \
  example/main.c \
  example/checker_task.c \
  example/monitored_task.c \
  example/record_assignment.c \
  $(OUT_DIR)/instrumented.c \
  $(OUT_DIR)/runtime-checker.c
INCS		= -I$(OUT_DIR) -Iexample
EXEC		= exec
PLUGIN  = ../gcc-plugin/instrument_plugin.so

EXEC: $(SRCS)
	$(CC) -Wall -std=c99 -fplugin=$(PLUGIN) $(INCS) $(SRCS) -o $(EXEC)

.PHONY: clean
clean:
	rm -rf exec
