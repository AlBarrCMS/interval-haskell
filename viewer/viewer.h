struct Box {
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

enum Mode { roots, minimization };

void draw_box(Box box);
void draw_outline(Box box, bool emphasize);

void prune_min_solution_boxes(double new_min);

void displayFunc();
void idleFunc();
void init(int argc, char **argv);
void load();
void motionFunc(int x, int y);
void mouseFunc(int button, int state, int x, int y);
