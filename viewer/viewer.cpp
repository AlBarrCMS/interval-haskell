#include "viewer.h"

#include <GL/glut.h>

#include <iostream>
#include <string>

#include "json/json.h"
#include "tree.h"

#define TREE_ORDER 2

void displayFunc(void) {
  glClear(GL_COLOR_BUFFER_BIT);
  glBegin(GL_POLYGON);
    glVertex3f(0.0, 0.0, 0.0);
    glVertex3f(0.5, 0.0, 0.0);
    glVertex3f(0.5, 0.5, 0.0);
    glVertex3f(0.0, 0.5, 0.0);
  glEnd();
  glFlush();
}

void init(int argc, char **argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_SINGLE);
  glutInitWindowSize(300, 300);
  glutInitWindowPosition(100, 100);
  glutCreateWindow("Hello world :D");
  glutDisplayFunc(displayFunc);
  glutMainLoop();
}

int main(int argc, char **argv) {
  IntervalNode *tree = new IntervalNode(TREE_ORDER);
  Box *box = new Box(TREE_ORDER);
  std::string line;

  box->pos[0] = -3;
  box->pos[1] = -3;
  box->dim[0] = 6;
  box->dim[1] = 6;
  tree->setBoundary(box);

  while (std::getline(std::cin, line)) {
    Json::Value val;
    Json::Reader reader;
    float low;
    float high;

    if (reader.parse(line.c_str(), val)) {
      if (!val.isObject()) {
        std::cout << val << std::endl;
        continue;
      }

      for (int i = 0; i < TREE_ORDER; i++) {
        box->pos[i] = val["input"][i]["low"].asFloat();
        box->dim[i] = val["input"][i]["high"].asFloat() - box->pos[i];
      }

      low = val["low"].asDouble();
      high = val["high"].asDouble();
      tree->setInterval(box, low, high);
    }
  }

  return 0;
}
