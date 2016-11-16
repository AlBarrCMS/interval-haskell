#include <GL/glut.h>

#include <cmath>
#include <fstream>
#include <limits>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "json/json.h"

#define XSCALE (xmax - xmin)
#define YSCALE (ymax - ymin)

enum Mode { roots, minimization };

class Box {
  public:
    Box();
    Box(double x0, double x1, double y0, double y1) : x0(x0), x1(x1), y0(y0), y1(y1) {};
    Box(Json::Value data, Mode display_mode);
    double x0;
    double x1;
    double y0;
    double y1;
    double r;
    double g;
    double b;
    double high;
    double low;
    bool valid;
    struct Box *next;
};

void draw_box(Box box, bool emphasize);
void draw_outline(Box box, bool emphasize);

void prune_min_solution_boxes(double new_min);

void displayFunc();
void reshapeFunc();
void idleFunc();
void init(int argc, char **argv);
void reshapeFunc(GLint width, GLint height);
FILE *load(std::string cmd, std::string polynomial, double xmin, double xmax,
                                                    double ymin, double ymax);
void motionFunc(int x, int y);
void mouseFunc(int button, int state, int x, int y);

double to_x_world_coord(double math_coord);
double to_y_world_coord(double math_coord);
