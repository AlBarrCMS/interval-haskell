#include "viewer.h"

#include <GL/glut.h>

#include <cmath>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#include "json/json.h"

#define XSCALE ((xmax - xmin) / 2)
#define YSCALE ((ymax - ymin) / 2)

double xmin;
double xmax;
double ymin;
double ymax;
int xc;
int yc;
bool dirty;
FILE *rin;
std::string cmd;
Box *head;
Box *tail;

void displayFunc() {
  char line[1024];
  Box *box;
  Box *next = NULL;

  if (dirty) {
    for (box = head; box; box = next) {
      next = box->next;
      free(box);
    }

    head = NULL;
    tail = NULL;
    dirty = false;
  }

  if (fgets(line, 1024, rin)) {
    Json::Value val;
    Json::Reader reader;

    if (reader.parse(line, val)) {
      if (val.isObject()) {
        box = (Box *) malloc(sizeof(Box));
        double low = val["output"]["low"].asDouble();
        double high = val["output"]["high"].asDouble();

        box->x0 = val["input"][0]["low"].asDouble() / XSCALE;
        box->x1 = val["input"][0]["high"].asDouble() / XSCALE;
        box->y0 = val["input"][1]["low"].asDouble() / YSCALE;
        box->y1 = val["input"][1]["high"].asDouble() / YSCALE;
        box->valid = high > 0 && low < 0;
        low = (high + low) / 2;
        high = std::min(log(abs(low) + 1), 1.0) * 0.6 + 0.4;
        box->r = low < 0 ? high : 0.2;
        box->g = 0.2;
        box->b = low > 0 ? high : 0.2;
        box->next = NULL;

        if (box->valid) {
          if (!head) {
            head = box;
          } else {
            tail->next = box;
          }

          tail = box;
        } else {
          if (!head) {
            tail = box;
          } else {
            box->next = head;
          }

          head = box;
        }
      }
    }

    glClear(GL_COLOR_BUFFER_BIT);

    for (box = head; box; box = box->next) {
      glBegin(GL_POLYGON);
        glColor3d(box->r, box->g, box->b);
        glVertex2d(box->x0, box->y0);
        glVertex2d(box->x1, box->y0);
        glVertex2d(box->x1, box->y1);
        glVertex2d(box->x0, box->y1);
      glEnd();

      glBegin(GL_LINE_LOOP);
        if (box->valid) {
          glColor3d(1, 1, 1);
        } else {
          glColor3d(0, 0, 0);
        }

        glVertex2d(box->x0, box->y0);
        glVertex2d(box->x1, box->y0);
        glVertex2d(box->x1, box->y1);
        glVertex2d(box->x0, box->y1);
      glEnd();
    }
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
  glutMotionFunc(motionFunc);
  glutMouseFunc(mouseFunc);
  glutMainLoop();
}

void load() {
  std::stringstream s;
  s << cmd << ' ' << xmin << ' ' << xmax << ' ' << ymin << ' ' << ymax;
  rin = popen(s.str().c_str(), "r");
}

int main(int argc, char **argv) {
  std::ifstream f;

  xmin = atof(argv[2]);
  xmax = atof(argv[3]);
  ymin = atof(argv[4]);
  ymax = atof(argv[5]);
  f.open(argv[1]);
  std::getline(f, cmd);
  cmd = "dist/build/rin/rin \"" + cmd + "\"";
  head = NULL;
  tail = NULL;
  dirty = false;
  xc = -1;
  yc = -1;
  load();
  init(argc, argv);
  return 0;
}

void motionFunc(int x, int y) {

}

void mouseFunc(int button, int state, int x, int y) {
  if (button == GLUT_LEFT_BUTTON) {
    if (state == GLUT_DOWN) {
      xc = x;
      yc = y;
    } else {
      if (xc != x && yc != y) {
        double width = XSCALE * 2 / glutGet(GLUT_WINDOW_WIDTH);
        double height = YSCALE * 2 / glutGet(GLUT_WINDOW_HEIGHT);

        dirty = true;
        xmin += std::min(xc, x) * width;
        xmax = xmin + std::max(xc, x) * width;
        ymin += std::min(yc, y) * height;
        ymax = ymin + std::max(yc, y) * height;
        pclose(rin);
        load();
      }

      xc = -1;
      yc = -1;
    }
  }
}
