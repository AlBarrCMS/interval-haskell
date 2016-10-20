CC      = g++
CPPFLAGS = -std=c++11 -g -Wall -pedantic
HEADERS = box.h json/json.h tree.h viewer.h
GL      = -lGL -lGLU -lglut
OBJECTS = box.o jsoncpp.o tree.o viewer.o

all: viewer

%.o: %.cpp $(HEADERS)
	$(CC) -c $(CPPFLAGS) $< -o $@

viewer: $(OBJECTS)
	$(CC) $(OBJECTS) -o viewer $(GL)

clean:
	rm *.o viewer
