# Define the compiler and flags
CC = gcc
CFLAGS = -Wall
TARGET = hash_table_test

# The default target that runs when you just type 'make'
all: $(TARGET)

# Rule to build the executable
# It depends on BOTH main.c and hash_table.c so it rebuilds if either changes
$(TARGET): main.c hash_table.c
	$(CC) $(CFLAGS) -o $(TARGET) main.c hash_table.c

# Rule to clean up generated files
clean:
	rm -f $(TARGET)
