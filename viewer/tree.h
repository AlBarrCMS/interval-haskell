struct Box {
  unsigned int size;
  float *pos;
  float *dim;
  Box(unsigned int order);
  ~Box();
  Box &operator=(const Box &other);
  bool contains(Box *other);
};

class IntervalNode {
  unsigned int divisions_;
  Box *boundary_;
  IntervalNode **children_;
  float low_;
  float high_;
  IntervalNode(unsigned int order);
  bool setInterval(Box *boundary, float low, float high);
  void subdivide(Box *boundary);
};

bool operator==(const Box &left, const Box &right);
