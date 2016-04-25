CC = g++
LD = g++
CPPFLAGS = -std=c++11 -g -Wall -pedantic
LDFLAGS =

SRCS = example_program.cpp jsoncpp.cpp

OBJS = $(SRCS:.cpp=.o)

all: example_program

example_program: $(OBJS)
	g++ -o $@ $^ $(LDFLAGS)

%.o : %.cpp
	$(CC) -c $(CPPFLAGS) $< -o $@

clean :
	rm -f example_program *.o
