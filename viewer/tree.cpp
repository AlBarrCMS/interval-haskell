#include "tree.h"

#include <cstdlib>

Box::Box(unsigned int order) {
  size = order;
  pos = (float *) std::malloc(sizeof(float) * order);
  dim = (float *) std::malloc(sizeof(float) * order);
}

Box::~Box() {
  free(pos);
  free(dim);
}

Box &Box::operator=(const Box &other) {
  for (int i = 0; i < size; ++i) {
    pos[i] = other.pos[i];
    dim[i] = other.dim[i];
  }
}

bool operator==(const Box &left, const Box &right) {
  bool equal = left.size == right.size;

  if (equal) {
    for (int i = 0; i < left.size; ++i) {
      if (left.pos[i] != right.pos[i] || left.dim[i] != right.dim[i]) {
        equal = false;
        break;
      }
    }
  }

  return equal;
}

IntervalNode::IntervalNode(unsigned int order) {
  divisions_ = 0;
  boundary_ = new Box(order);
  children_ = (IntervalNode **) std::malloc(sizeof(IntervalNode *) * order);

  for (int i = 0; i < order; ++i) {
    children_[i] = NULL;
  }
}

void IntervalNode::setInterval(Box *boundary, float low, float high) {
  if (*boundary == *boundary_) {
    *boundary_ = *boundary;
  } else {

  }
}
