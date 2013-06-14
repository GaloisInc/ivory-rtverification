# make EXPLICIT32=1
# To be compatible with arm-none-eabi-gcc

# Our cross-compiler isn't a C++-built compiler, so for EXPLICIT32, we'll also
# use gcc rather than g++.
ifneq ($(EXPLICIT32),)
	CC = gcc
else
	CC = gcc
endif
EXPLICIT32 ?=
TARGET_NAME = instrument_plugin.so
PLUGIN_SOURCE_FILES = src/instrument_plugin.cpp src/utils.cpp
PLUGIN_OBJECT_FILES = $(patsubst %.cpp,%.o,$(PLUGIN_SOURCE_FILES))

GCCPLUGINS_DIR := $(shell $(CC) -print-file-name=plugin)
CFLAGS += -MMD -std=c99 -I$(GCCPLUGINS_DIR)/include -fPIC -Wall -g
ifneq ($(EXPLICIT32),)
	CFLAGS += -m32
endif

DEBUGGER := $(shell command -v cgdb || echo gdb)
UNAME := $(shell uname)

.PHONY: test clean debug asm showpasses dumptrees

# main target -- sets the correct flags for building an so based on platform
$(TARGET_NAME): $(PLUGIN_OBJECT_FILES)
ifeq ($(UNAME), Darwin)
	$(CC) -bundle -undefined dynamic_lookup $^ -o $@
else
ifneq ($(EXPLICIT32),)
	$(CC) -Wall -m32 -shared $^ -o $@
else
	$(CC) -Wall -shared $^ -o $@
endif
endif

SAMPLE_PROG := sample/sample_program

# compile a sample program, with the plugin loaded
$(SAMPLE_PROG): $(TARGET_NAME) sample/*.c
	$(CC) -Wall -std=c99 -DDEBUG -g -fplugin=./$(TARGET_NAME) \
    -c sample/record_assignment.c -o sample/record_assignment.o
	$(CC) -Wall -std=c99 -DDEBUG -g -fplugin=./$(TARGET_NAME) \
    sample/record_assignment.o sample/sample.c -o $(SAMPLE_PROG)

test: $(TARGET_NAME) $(SAMPLE_PROG)
	./$(SAMPLE_PROG)

# run gcc -- with the plugin loaded -- in gdb
debug: $(TARGET_NAME)
	$(CC) -Wall -fplugin=./$(TARGET_NAME) sample/sample.c -o /dev/null \
    -wrapper $(DEBUGGER),--args

clean:
	rm -f $(TARGET_NAME) $(PLUGIN_OBJECT_FILES) src/*.d
	rm -f $(SAMPLE_PROG)
	rm -f sample/*.o
	rm -f sample/*.d

version:
	$(CC) --version
