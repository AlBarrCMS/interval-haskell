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

bool Box::contains(Box *other) {
  bool valid = size == other->size;

  if (valid) {
    for (int i = 0; i < size; ++i) {
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
}

bool IntervalNode::setInterval(Box *boundary, float low, float high) {
  bool valid = false;

  if (*boundary == *boundary_) {
    *boundary_ = *boundary;
    valid = true;
  } else if (boundary_->contains(boundary)) {
    if (!divisions_) {
      subdivide(boundary);
    }

    for (int i = 0; i < divisions_; ++i) {
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

  for (int i = 0; i < boundary_->size; ++i) {
    sides[i] = 1 +
        (boundary_->pos[i] != boundary->pos[i]) +
        (boundary_->pos[i] + boundary_->dim[i] !=
        boundary->pos[i] + boundary->dim[i]);
    is[i] = 0;
    divisions *= sides[i];
  }

  children_ =
      (IntervalNode **) std::malloc(sizeof(IntervalNode *) * divisions);

  for (int j = 0; j < divisions; ++j) {
    int i = boundary_->size;

    while (i --> 0) {
      if (++is[i] == sides[i]) {
        is[i] = 0;
      } else {
        break;
      }
    }

    children_[j] = new IntervalNode(boundary_->size);
    children_[j]->low_ = low_;
    children_[j]->high_ = high_;

    for (i = 0; i < boundary_->size; ++i) {
      float low = boundary_->pos[i];
      float high = low + boundary_->dim[i];

      switch (is[i]) {
        case 0:
          if (sides[i] != 1) {
            high = boundary->pos[i];
          }

          break;
        case 1:
          low = boundary->pos[i];
          high = low + boundary->dim[i];
          break;
        case 2:
          low = boundary->pos[i] + boundary->dim[i];
          break;
      }

      children_[j]->boundary_->pos[i] = low;
      children_[j]->boundary_->dim[i] = high - low;
    }
  }

  free(sides);
  free(is);
  divisions_ = divisions;
}
