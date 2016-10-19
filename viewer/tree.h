struct Box {
  unsigned int size;
  float *pos;
  float *dim;
  Box(unsigned int order);
  ~Box();
  Box &operator=(const Box &other);
};

class IntervalNode {
  unsigned int divisions_;
  unsigned int order_;
  Box *boundary_;
  IntervalNode **children_;
  float low_;
  float high_;
  IntervalNode(unsigned int order);
  void setInterval(Box *boundary, float low, float high);
};

bool operator==(const Box &left, const Box &right);
