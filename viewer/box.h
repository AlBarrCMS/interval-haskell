struct Box {
  unsigned int size;
  double *pos;
  double *dim;
  Box(unsigned int order);
  ~Box();
  bool contains(Box *other);
};

bool operator==(const Box &left, const Box &right);
