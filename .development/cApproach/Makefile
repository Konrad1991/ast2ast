CC = gcc
CFLAGS = -Wall -g

SRCS = main.c SubsetManager.c VectorManager.c
OBJS = $(SRCS:.c=.o)

TARGET = main

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS)

# Rule to build object files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

SubsetManager.o: VectorManager.h
VectorManager.o: VectorManager.h

# Clean up
clean:
	rm -f $(OBJS) $(TARGET)

