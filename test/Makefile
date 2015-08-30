
CC=clang
CFLAGS = -Wall -Wno-address -Wextra -std=c11
#CFLAGS += -DUNITY_INCLUDE_DOUBLE	\
	  -DUNITY_SUPPORT_TEST_CASES	\
	  -DUNITY_SUPPORT_64		\
	  -DUNITY_INT_WIDTH=32		\
	  -DUNITY_LONG_WIDTH=64

SRCS += unity.c unity_fixture.c all_tests.c test_init.c test_game.c

all: test

test: $(SRCS) ../solver.c
	$(CC) $(CFLAGS) $(SRCS) -o $@

.PHONY: clean
clean:
	rm -vf *.o *.gch
