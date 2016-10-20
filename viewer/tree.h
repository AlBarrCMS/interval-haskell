#include "box.h"

class IntervalNode {
 private:
  unsigned int divisions_;
  Box *boundary_;
  IntervalNode **children_;
  double low_;
  double high_;

 public:
  IntervalNode(unsigned int order);
  void setBoundary(Box *boundary);
  bool setInterval(Box *boundary, double low, double high);
  void subdivide(Box *boundary);
};
