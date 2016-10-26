#include "viewer.h"

#include <GL/glut.h>

#include <cmath>
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
      }
    }

    double t = (high + low) / 2;
    double l = std::min(log(abs(t) + 1), 1.0) * 0.6 + 0.4;
    double r = t < 0 ? l : 0.2;
    double g = 0.2;
    double b = t > 0 ? l : 0.2;

    glBegin(GL_POLYGON);
      glColor3d(r, g, b);
      glVertex2d(box->pos[0] / XSCALE, box->pos[1] / YSCALE);
      glVertex2d((box->pos[0] + box->dim[0]) / XSCALE, box->pos[1] / YSCALE);
      glVertex2d((box->pos[0] + box->dim[0]) / XSCALE,
                 (box->pos[1] + box->dim[1]) / YSCALE);
      glVertex2d(box->pos[0] / XSCALE, (box->pos[1] + box->dim[1]) / YSCALE);
    glEnd();

    glBegin(GL_LINE_LOOP);
      if (high > 0 && low < 0) {
        glColor3d(1, 1, 1);
      } else {
        glColor3d(0, 0, 0);
      }

      glVertex2d(box->pos[0] / XSCALE, box->pos[1] / YSCALE);
      glVertex2d((box->pos[0] + box->dim[0]) / XSCALE, box->pos[1] / YSCALE);
      glVertex2d((box->pos[0] + box->dim[0]) / XSCALE,
                 (box->pos[1] + box->dim[1]) / YSCALE);
      glVertex2d(box->pos[0] / XSCALE, (box->pos[1] + box->dim[1]) / YSCALE);
    glEnd();

    delete box;
  }

  glFlush();
}

void init(int argc, char **argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_SINGLE);
  glutInitWindowSize(600, 600);
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
  delete box;
  init(argc, argv);
  return 0;
}
