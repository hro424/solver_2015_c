CC = clang
CFLAGS = -Wall -O3 -DNDEBUG
#CFLAGS = -Wall -g
TARGET = solver
SRCS = main.c

all: $(TARGET)

$(TARGET): $(SRCS)
	$(CC) $(CFLAGS) -o $(TARGET) $<

.PHONY: clean
clean: $(TARGET)
	rm -vf $(TARGET)
