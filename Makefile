CC = clang

all: solver 

solver: solver.c
	$(CC) -o $@ $^ -Wall -O3

.PHONY: clean
clean:
	rm -vf solver
