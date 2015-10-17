CC = clang
CFLAGS = -Wall -O3
TARGET = solver
SRCS = main.c

all: $(TARGET)

$(TARGET): $(SRCS)
	$(CC) $(CFLAGS) -o $(TARGET) $<

.PHONY: clean
clean: $(TARGET)
	rm -vf $(TARGET)
