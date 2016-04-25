CC = g++
LD = g++
CPPFLAGS = -std=c++11 -g -Wall -pedantic
LDFLAGS =

SRCS = example.cpp jsoncpp.cpp

OBJS = $(SRCS:.cpp=.o)

all: example

example: $(OBJS)
	g++ -o $@ $^ $(LDFLAGS)

%.o : %.cpp
	$(CC) -c $(CPPFLAGS) $< -o $@

clean :
	rm -f example *.o
