CC = gcc
CFLAGS = -Wall -g

SRCS = main.c Print.c VectorManager.c GetAndSet.c Colon.c DetermineSize.c Subsets.c
OBJS = $(SRCS:.c=.o)

TARGET = main

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS)

# Rule to build object files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

Print.o: VectorManager.h
VectorManager.o: VectorManager.h
GetAndSet.o: VectorManager.h
Colon.o: VectorManager.h
DetermineSize.o: VectorManager.h
Subsets.o: VectorManager.h


# Clean up
clean:
	rm -f $(OBJS) $(TARGET)

