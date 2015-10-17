CC = clang
#CFLAGS = -Wall -O3 -DNDEBUG
CFLAGS = -Wall -O0 -g
SRCS = main.c

all: solver solver_simple

solver: $(SRCS)
	$(CC) $(CFLAGS) -DCONFIG_OPT -o $@ $<

solver_simple: $(SRCS)
	$(CC) $(CFLAGS) -o $@ $<

.PHONY: clean
clean: $(TARGET)
	rm -vf $(TARGET)
