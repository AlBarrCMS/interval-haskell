CC      = g++
HEADERS = tree.h viewer.h
GL      = -lGL -lGLU -lglut
OBJECTS = tree.o viewer.o

all: viewer

%.o: %.cpp $(HEADERS)
	$(CC) -c $< -o $@

viewer: $(OBJECTS)
	$(CC) $(OBJECTS) -o viewer $(GL)

clean:
	rm *.o viewer
