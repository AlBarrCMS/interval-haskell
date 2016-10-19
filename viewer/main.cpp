#include "main.h"

#include <GL/glut.h>

#include <iostream>
#include <string>

void DisplayFunc(void) {
  glClear(GL_COLOR_BUFFER_BIT);
  glBegin(GL_POLYGON);
    glVertex3f(0.0, 0.0, 0.0);
    glVertex3f(0.5, 0.0, 0.0);
    glVertex3f(0.5, 0.5, 0.0);
    glVertex3f(0.0, 0.5, 0.0);
  glEnd();
  glFlush();
}

void Init(int argc, char **argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_SINGLE);
  glutInitWindowSize(300, 300);
  glutInitWindowPosition(100, 100);
  glutCreateWindow("Hello world :D");
  glutDisplayFunc(DisplayFunc);
  glutMainLoop();
}

int main(int argc, char **argv) {
  std::string line;

  while (std::getline(std::cin, line)) {
    std::cout << line << std::endl;
  }

  return 0;
}
