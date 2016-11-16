#include "viewer.h"

#include <GL/glut.h>

#include <cmath>
#include <fstream>
#include <limits>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "json/json.h"

#define XSCALE ((xmax - xmin) / 2)
#define YSCALE ((ymax - ymin) / 2)

double xmin;
double xmax;
double ymin;
double ymax;
double current_min;
Mode display_mode;
int xc;
int yc;
bool dirty;
FILE *rin;
std::string cmd;
std::vector<Box> potential_solutions;
std::vector<Box> discarded_boxes;


void displayFunc() {
  glClear(GL_COLOR_BUFFER_BIT);

  for (auto box = discarded_boxes.begin(); box != discarded_boxes.end(); ++box) {
    draw_box(*box);
    draw_outline(*box, false);
  }

  for (auto box = potential_solutions.begin(); box != potential_solutions.end(); ++box) {
    draw_box(*box);
    draw_outline(*box, true);
  }

  glutSwapBuffers();
}

void idleFunc(){
  char line[1024];
  Box box;

  if (dirty) {
    potential_solutions.clear();
    discarded_boxes.clear();
    dirty = false;
  }

  if (fgets(line, 1024, rin)) {
    Json::Value val;
    Json::Reader reader;

    if (reader.parse(line, val)) {
      if (val.isObject()) {
        double low = val["output"]["low"].asDouble();
        double high = val["output"]["high"].asDouble();
        if (high < current_min) {
          current_min = high;
        }

        box.x0 = val["input"][0]["low"].asDouble() / XSCALE;
        box.x1 = val["input"][0]["high"].asDouble() / XSCALE;
        box.y0 = val["input"][1]["low"].asDouble() / YSCALE;
        box.y1 = val["input"][1]["high"].asDouble() / YSCALE;
        box.high = high;
        box.low = low;
        switch (display_mode) {
          case roots:
            box.valid = high > 0 && low < 0;
            break;
          case ms:
          case fd:
            box.valid = low < current_min;
            prune_min_solution_boxes(current_min);
            break;
        }

        low = (high + low) / 2;
        high = std::min(log(abs(low) + 1), 1.0) * 0.6 + 0.4;
        box.r = low < 0 ? high : 0.2;
        box.g = 0.2;
        box.b = low > 0 ? high : 0.2;
        box.next = NULL;

        if (box.valid) {
          potential_solutions.push_back(box);
        } else {
          discarded_boxes.push_back(box);
        }
      }
    }
    glutPostRedisplay();
  }
}

void init(int argc, char **argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE);
  glutInitWindowSize(600, 600);
  glutCreateWindow("Hello world :D");
  glutDisplayFunc(displayFunc);
  glutIdleFunc(idleFunc);
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
  if (argc < 6) {
    std::cerr<<"usage: " <<argv[0]<<" poly_file xmin xmax ymin ymax [draw_mode]"
      <<std::endl<<"\t where draw_mode = 1 means draw minimization data"
      <<std::endl<<"\t and anything else means draw root-finder"
      <<std::endl;
    exit(1);
  }

  std::ifstream f;

  xmin = atof(argv[2]);
  xmax = atof(argv[3]);
  ymin = atof(argv[4]);
  ymax = atof(argv[5]);
  display_mode = roots;

  if (argc >= 7) {
    int mode = atoi(argv[6]);

    if (mode == 1 || mode == 2) {
      display_mode = mode == 1 ? ms : fd;
      current_min = std::numeric_limits<double>::max();
    }
  }

  f.open(argv[1]);
  std::getline(f, cmd);

  switch (display_mode) {
    case ms:
      cmd = "dist/build/moore_skelboe/moore_skelboe \"" + cmd + "\"";
      break;
    case fd:
      cmd = "dist/build/fd/fd \"" + cmd + "\"";
      break;
    case roots:
      cmd = "dist/build/rin/rin \"" + cmd + "\"";
      break;
  }
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

// Marks boxes that can't contain the new min as not valid and moves them to
// the beginning of the list
void prune_min_solution_boxes(double new_min) {
  auto box = potential_solutions.begin();
  while(box != potential_solutions.end()) {
      if(box->low > new_min) {
          discarded_boxes.push_back(*box);
          box = potential_solutions.erase(box);
      }
      else ++box;
  }
}

void draw_box(Box box) {
  glBegin(GL_POLYGON);
    glColor3d(box.r, box.g, box.b);
    glVertex2d(box.x0, box.y0);
    glVertex2d(box.x1, box.y0);
    glVertex2d(box.x1, box.y1);
    glVertex2d(box.x0, box.y1);
  glEnd();
}

void draw_outline(Box box, bool emphasize) {
  glBegin(GL_LINE_LOOP);
    if (emphasize) {
      glColor3d(1, 1, 1);
    } else {
      glColor3d(0, 0, 0);
    }

    glVertex2d(box.x0, box.y0);
    glVertex2d(box.x1, box.y0);
    glVertex2d(box.x1, box.y1);
    glVertex2d(box.x0, box.y1);
  glEnd();
}
