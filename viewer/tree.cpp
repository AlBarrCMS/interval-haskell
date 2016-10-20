#include "tree.h"

#include <cstdlib>
#include <iostream>

IntervalNode::IntervalNode(unsigned int order) {
  divisions_ = 0;
  boundary_ = new Box(order);
}

void IntervalNode::setBoundary(Box *boundary) {
  for (unsigned int i = 0; i < boundary_->size; ++i) {
    boundary_->pos[i] = boundary->pos[i];
    boundary_->dim[i] = boundary->dim[i];
  }
}

bool IntervalNode::setInterval(Box *boundary, double low, double high) {
  bool valid = false;

  if (*boundary == *boundary_) {
    low_ = low;
    high_ = high;
    valid = true;
  } else if (boundary_->contains(boundary)) {
    if (!divisions_) {
      subdivide(boundary);
    }

    for (unsigned int i = 0; i < divisions_; ++i) {
      if (children_[i]->setInterval(boundary, low, high)) {
        valid = true;
        break;
      }
    }
  }

  return valid;
}

void IntervalNode::subdivide(Box *boundary) {
  unsigned int divisions = 1;
  unsigned int *sides =
      (unsigned int *) malloc(sizeof(unsigned int) * boundary_->size);
  unsigned int *is =
      (unsigned int *) malloc(sizeof(unsigned int) * boundary_->size);

  for (unsigned int i = 0; i < boundary_->size; ++i) {
    sides[i] = 1 +
        (boundary_->pos[i] != boundary->pos[i]) +
        (boundary_->pos[i] + boundary_->dim[i] !=
        boundary->pos[i] + boundary->dim[i]);
    is[i] = 0;
    divisions *= sides[i];
  }

  children_ =
      (IntervalNode **) std::malloc(sizeof(IntervalNode *) * divisions);

  for (unsigned int j = 0; j < divisions; ++j) {
    unsigned int i;

    children_[j] = new IntervalNode(boundary_->size);

    for (i = 0; i < boundary_->size; ++i) {
      double low = boundary_->pos[i];
      double high = low + boundary_->dim[i];

      switch (is[i]) {
        case 0:
          if (low == boundary->pos[i]) {
            high = low + boundary->dim[i];
          } else {
            high = boundary->pos[i];
          }

          break;
        case 1:
          if (low == boundary->pos[i]) {
            low += boundary->dim[i];
          } else {
            low = boundary->pos[i];
            high = low + boundary->dim[i];
          }

          break;
        case 2:
          low = boundary->pos[i] + boundary->dim[i];
          break;
      }

      children_[j]->boundary_->pos[i] = low;
      children_[j]->boundary_->dim[i] = high - low;
    }

    i = boundary_->size;

    while (i --> 0) {
      if (++is[i] == sides[i]) {
        is[i] = 0;
      } else {
        break;
      }
    }
  }

  free(sides);
  free(is);
  divisions_ = divisions;
}
