PG = bison
CC = g++
LG = re2c

CFLAGS = -O0 -Wall -pedantic -ansi -std=c++17

TARGET = genesis

%.cpp: %.y
	$(PG) $< --report=all -o $@
	$(LG) -F $@ -o $@

$(TARGET): $(TARGET).cpp
	$(CC) $(CFLAGS) $< -o $(TARGET)

all: $(TARGET)

clean:
	rm -rf *.cpp $(TARGET) $(TARGET).output