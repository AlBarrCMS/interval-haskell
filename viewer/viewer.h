struct Box {
  double x0;
  double x1;
  double y0;
  double y1;
  double r;
  double g;
  double b;
  bool valid;
  struct Box *next;
};

enum Mode { roots, minimization };

void displayFunc();
void init(int argc, char **argv);
void load();
void motionFunc(int x, int y);
void mouseFunc(int button, int state, int x, int y);
