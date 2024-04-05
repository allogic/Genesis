PG = bison
CC = g++
LG = re2c

CFLAGS = -O0 -Wall -pedantic -ansi -std=c++17

TARGET = Genesis

all: $(TARGET)

clean:
	rm -rf *.cpp $(TARGET) $(TARGET).output

$(TARGET): $(TARGET).cpp
	$(CC) $(CFLAGS) $< -o $(TARGET)

$(TARGET).cpp: $(TARGET).y
	$(PG) $< --report=all -o $@
	$(LG) -F $@ -o $@