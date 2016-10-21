#include "viewer.h"

#include <GL/glut.h>

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <string>

#include "json/json.h"

#define TREE_ORDER 2
#define XMIN -3
#define XMAX 3
#define XSCALE ((XMAX - XMIN) / 2)
#define YMIN -3
#define YMAX 3
#define YSCALE ((YMAX - YMIN) / 2)

IntervalNode tree = IntervalNode(TREE_ORDER);

void displayFunc() {
  std::string line;

  if (std::getline(std::cin, line)) {
    Json::Value val;
    Json::Reader reader;
    double low;
    double high;
    Box *box = new Box(TREE_ORDER);

    if (reader.parse(line.c_str(), val)) {
      if (val.isObject()) {
        for (int i = 0; i < TREE_ORDER; i++) {
          box->pos[i] = val["input"][i]["low"].asDouble();
          box->dim[i] = val["input"][i]["high"].asDouble() - box->pos[i];
        }

        low = val["output"]["low"].asDouble();
        high = val["output"]["high"].asDouble();
        tree.setInterval(box, low, high);
      }
    }

    delete box;
  }

  glClear(GL_COLOR_BUFFER_BIT);
  drawIntervalBox(&tree);
  glFlush();
}

void drawIntervalBox(IntervalNode *node) {
  Box *box = node->getBoundary();
  //~ std::cout << node->low_ << ' ' << node->high_ << ' '
            //~ << box->pos[0] << ' ' << box->pos[1] << ' '
            //~ << box->dim[0] << ' ' << box->dim[1] << std::endl;
  double t = (node->high_ + node->low_) / 2;
  double l = std::min(log(log(abs(t))), 1.0);
  double r = t < 0 ? l : 0;
  double g = (float) rand() / RAND_MAX;
  double b = t > 0 ? l : 0;

  if (node->high_ > 0 && node->low_ < 0) {
    r = 1;
    g = 1;
    b = 1;
  }

  glBegin(GL_POLYGON);
    glColor3d(r, g, b);
    glVertex2d(box->pos[0] / XSCALE, box->pos[1] / YSCALE);
    glVertex2d((box->pos[0] + box->dim[0]) / XSCALE, box->pos[1] / YSCALE);
    glVertex2d((box->pos[0] + box->dim[0]) / XSCALE,
               (box->pos[1] + box->dim[1]) / YSCALE);
    glVertex2d(box->pos[0] / XSCALE, (box->pos[1] + box->dim[1]) / YSCALE);
  glEnd();

  for (unsigned int i = 0; i < node->divisions_; i++) {
    drawIntervalBox(node->children_[i]);
  }
}

void init(int argc, char **argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_SINGLE);
  glutInitWindowSize(1366, 768);
  glutCreateWindow("Hello world :D");
  glutDisplayFunc(displayFunc);
  glutIdleFunc(displayFunc);
  glutMainLoop();
}

int main(int argc, char **argv) {
  Box *box = new Box(TREE_ORDER);

  box->pos[0] = XMIN;
  box->pos[1] = YMIN;
  box->dim[0] = XMAX - XMIN;
  box->dim[1] = YMAX - YMIN;
  tree.setBoundary(box);
  delete box;
  init(argc, argv);
  return 0;
}
