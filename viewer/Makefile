CC=g++
GL=-lGL -lGLU -lglut

all: viewer

viewer: viewer.o
	$(CC) -o viewer viewer.o $(GL)

viewer.o: main.cpp main.h
	$(CC) -c -o viewer.o main.cpp

clean:
	rm *.o viewer
