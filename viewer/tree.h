#include "box.h"

class IntervalNode {
 private:
  Box *boundary_;

 public:
  unsigned int divisions_;
  IntervalNode **children_;
  double low_;
  double high_;
  IntervalNode(unsigned int order);
  Box *getBoundary();
  void setBoundary(Box *boundary);
  bool setInterval(Box *boundary, double low, double high);
  void subdivide(Box *boundary);
};
