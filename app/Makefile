CXX = g++
CXXFLAGS = -std=c++17 -Wall -Wextra -O2
TARGET = fizzbuzz
SOURCE = main.cpp

.PHONY: all clean test run

all: $(TARGET)

$(TARGET): $(SOURCE)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(SOURCE)

test: $(TARGET)
	./$(TARGET)

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TARGET)