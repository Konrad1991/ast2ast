# Define the compiler and the flags
CC = gcc
CFLAGS = -Wall -g

# Define the source files and object files
SRCS = $(wildcard *.c)
OBJS = $(SRCS:.c=.o)

# Define the target executable
TARGET = main

# Rule to build the target executable
$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS)

# Rule to build object files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Clean up
clean:
	rm -f $(OBJS) $(TARGET)

