#include "viewer.h"

double xmin;
double xmax;
double ymin;
double ymax;
double current_min;
Mode display_mode;
int width  = 800;
int height = 800;
int xc;
int yc;
FILE *rin;
std::string cmd;
std::string polynomial;
std::vector<Box> potential_solutions;
std::vector<Box> discarded_boxes;
Box *mouse_rect;


void displayFunc() {
  glClear(GL_COLOR_BUFFER_BIT);

  for (auto box = discarded_boxes.begin();
            box != discarded_boxes.end(); ++box) {
    draw_box(*box, false);
    draw_outline(*box, false);
  }

  for (auto box = potential_solutions.begin();
            box != potential_solutions.end(); ++box) {
    draw_box(*box, true);
    draw_outline(*box, true);
  }

  if (mouse_rect) {
    draw_outline(*mouse_rect, true);
  }

  glutSwapBuffers();
}

void idleFunc(){
  char line[1024];
  Box box;

  // Read in up to 10 boxes before the next frame
  int num_boxes = 50;

  // Try to read in input
  for (int i = 0; i < num_boxes; i++) {
    if (fgets(line, 1024, rin)) {
      Json::Value val;
      Json::Reader reader;

      if (reader.parse(line, val)) {
        if (val.isObject()) {

          // If we are computing a minimum, we need to keep track of the
          // minimum
          if (display_mode != roots) {
            if (val.isMember("min")) {
              current_min = val["min"].asDouble();
              prune_min_solution_boxes(current_min);
            }

            double high = val["output"]["high"].asDouble();
            if (high < current_min) {
              current_min = high;
            }
          }

          box = Box(val, display_mode);

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
}

void init(int argc, char **argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE);
  glutInitWindowSize(800, 800);
  glutCreateWindow("Hello world :D");
  glutDisplayFunc(displayFunc);
  glutReshapeFunc(reshapeFunc);
  glutIdleFunc(idleFunc);
  glutMotionFunc(motionFunc);
  glutMouseFunc(mouseFunc);
  glutMainLoop();
}

FILE *load(std::string cmd, std::string polynomial, double xmin, double xmax,
                                                    double ymin, double ymax) {
  std::stringstream s;
  switch(display_mode) {
    case roots:
      s << cmd << " \"" << polynomial << "\" " << xmin << ' ' << xmax << ' '
        << ymin << ' ' << ymax << " "<< (double) pow(XSCALE/1000, 2);
      break;
    case ms:
    case fd:
      s << cmd << " \"" << polynomial << "\" " << xmin << ' ' << xmax << ' '
        << ymin << ' ' << ymax << " "<< (double) pow(XSCALE/200, 1);
      break;
  }
  return popen(s.str().c_str(), "r");
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
  std::getline(f, polynomial);

  switch (display_mode) {
    case ms:
      cmd = "dist/build/moore_skelboe/moore_skelboe";
      break;
    case fd:
      cmd = "dist/build/fd/fd";
      break;
    case roots:
      cmd = "dist/build/rin/rin";
      break;
  }
  xc = -1;
  yc = -1;
  rin = load(cmd, polynomial, xmin, xmax, ymin, ymax);
  init(argc, argv);
  return 0;
}

void motionFunc(int x, int y) {
  mouse_rect->x1 = 2 * (x - (double) width / 2) / width;
  mouse_rect->y1 = 2 * (-y + (double) height / 2) / height;

  glutPostRedisplay();
}

void mouseFunc(int button, int state, int x, int y) {
  if (button == GLUT_LEFT_BUTTON) {
    if (state == GLUT_DOWN) {
      xc = x;
      yc = y;
      double world_x = 2 * (x - (double)width/2)/width;
      double world_y = 2 * (-y + (double)height/2)/height;
      mouse_rect = new Box(world_x, world_x, world_y, world_y);
    } else {
      free(mouse_rect);
      mouse_rect = NULL;
      if (xc != x && yc != y) {
        //double width = XSCALE / glutGet(GLUT_WINDOW_WIDTH);
        //double height = YSCALE / glutGet(GLUT_WINDOW_HEIGHT);

        double screen_xmin = std::min(xc, x) / (double) width;
        double screen_ymin = std::min(height - yc, height - y) / (double) height;

        double side_len = std::max(std::abs(xc - x), std::abs(yc - y));

        double screen_xmax = screen_xmin + side_len / (double) width;
        double screen_ymax = screen_ymin + side_len / (double) height;

        xmin = xmin + screen_xmin * XSCALE;
        xmax = xmin + screen_xmax * XSCALE;
        ymin = ymin + screen_ymin * YSCALE;
        ymax = ymin + screen_ymax * YSCALE;

        pclose(rin);
        rin = load(cmd, polynomial, xmin, xmax, ymin, ymax);

        potential_solutions.clear();
        discarded_boxes.clear();

        if (display_mode != roots)
          current_min = std::numeric_limits<double>::max();
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

// Draws a box. If emphasize is true, it is colored white. Otherwise, it is
// colored its own color
void draw_box(Box box, bool emphasize) {
  glBegin(GL_POLYGON);
    if (emphasize) {
      glColor3d(1, 1, 1);
    } else {
      glColor3d(box.r, box.g, box.b);
    }
    glVertex2d(box.x0, box.y0);
    glVertex2d(box.x1, box.y0);
    glVertex2d(box.x1, box.y1);
    glVertex2d(box.x0, box.y1);
  glEnd();
}

// Draws the outline of a box. If emphasize is true, it is colored white.
// Otherwise, it is colored its own color
void draw_outline(Box box, bool emphasize) {
  if (emphasize) {
    glColor3d(1, 1, 1);
  } else {
    glColor3d(0.5 * box.r, 0.5 * box.g, 0.5 * box.b);
  }

  glBegin(GL_LINE_LOOP);
    glVertex2d(box.x0, box.y0);
    glVertex2d(box.x1, box.y0);
    glVertex2d(box.x1, box.y1);
    glVertex2d(box.x0, box.y1);
  glEnd();
}

Box::Box() {
  next = NULL;
}
Box::Box(Json::Value val, Mode display_mode) {
  x0 = to_x_world_coord(val["input"][0]["low"].asDouble());
  x1 = to_x_world_coord(val["input"][0]["high"].asDouble());
  y0 = to_y_world_coord(val["input"][1]["low"].asDouble());
  y1 = to_y_world_coord(val["input"][1]["high"].asDouble());

  low = val["output"]["low"].asDouble();
  high = val["output"]["high"].asDouble();
  next = NULL;
  switch (display_mode) {
    case roots:
      valid = high > 0 && low < 0;
      break;
    case ms:
    case fd:
      valid = low < current_min;
      prune_min_solution_boxes(current_min);
      break;
  }

  double max_noise = 0.1;
  double r_noise = max_noise * ((double)rand()/(double)RAND_MAX) - max_noise/2;
  double g_noise = max_noise * ((double)rand()/(double)RAND_MAX) - max_noise/2;
  double b_noise = max_noise * ((double)rand()/(double)RAND_MAX) - max_noise/2;

  // Set box color based on function value
  switch (display_mode) {
    case roots:
      low = (high + low) / 2;
      high = std::min(log(abs(low) + 1), 1.0) * 0.6 + 0.4;
      r = low < 0 ? high : 0.2 + r_noise;
      g = 0.2 + g_noise;
      b = low > 0 ? high : 0.2 + b_noise;
      break;
    case ms:
    case fd:
      r = 0.4 + r_noise;
      g = 0.2 + g_noise;
      b = 0.8 + b_noise;
      break;
  }
}

void reshapeFunc(GLint gl_width, GLint gl_height) {
  width  = gl_width;
  height = gl_height;
  glutPostRedisplay();
}

// Take an x coordinate from the domain we are working on and convert
// it into world space to be drawn
double to_x_world_coord(double math_coord) {
  return 2 * (math_coord - xmin) / (double) XSCALE - 1;
}

// Take a y coordinate from the domain we are working on and convert
// it into world space to be drawn
double to_y_world_coord(double math_coord) {
  return 2 * (math_coord - ymin) / (double) YSCALE - 1;
}

