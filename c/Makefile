CC = clang
CFLAGS = -Wall -O3 -DNDEBUG
#CFLAGS = -Wall -O0 -g
SRCS = main.c

all: solver

solver: $(SRCS)
	$(CC) $(CFLAGS) -o $@ $<

.PHONY: clean
clean: $(TARGET)
	rm -vf $(TARGET)
