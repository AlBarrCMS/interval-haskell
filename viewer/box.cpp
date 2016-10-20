#include "box.h"

#include <cstdlib>
#include <iostream>

Box::Box(unsigned int order) {
  size = order;
  pos = (double *) std::malloc(sizeof(float) * order);
  dim = (double *) std::malloc(sizeof(float) * order);
}

Box::~Box() {
  free(pos);
  free(dim);
}

bool Box::contains(Box *other) {
  bool valid = size == other->size;

  if (valid) {
    for (unsigned int i = 0; i < size; ++i) {
      if (other->pos[i] < pos[i] ||
          pos[i] + dim[i] < other->pos[i] + other->dim[i]) {
        valid = false;
        break;
      }
    }
  }

  return valid;
}

bool operator==(const Box &left, const Box &right) {
  bool equal = left.size == right.size;

  if (equal) {
    for (unsigned int i = 0; i < left.size; ++i) {
      if (left.pos[i] != right.pos[i] || left.dim[i] != right.dim[i]) {
        equal = false;
        break;
      }
    }
  }

  return equal;
}

