CC = clang

all: filter solver

solver: solver.c
	$(CC) -o $@ $^ -Wall -O3

filter: filter.c
	$(CC) -o $@ $^ -Wall -O3

.PHONY: clean
clean:
	rm -vf solver filter
