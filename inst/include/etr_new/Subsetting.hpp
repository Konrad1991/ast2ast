inline double &at(const VEC<double> &inp, int i) {
  i--;
  return inp.d[i];
}

inline double &at(const VEC<double> &&inp, int i) {
  i--;
  return inp.d[i];
}

inline double &at(const VEC<double> &inp, double i_) {
  int i = d2i(i_);
  i--;
  return inp.d[i];
}

inline double &at(const VEC<double> &&inp, double i_) {
  int i = d2i(i_);
  i--;
  return inp.d[i];
}

inline double &at(const VEC<double> &inp, int r, int c) {

  ass(inp.im() == true, "Input is not a matrix!");
  r--;
  c--;
  return inp.d[c * inp.nr() + r];
}

inline double &at(const VEC<double> &inp, double r_, double c_) {

  ass(inp.im() == true, "Input is not a matrix!");
  int r = d2i(r_);
  int c = d2i(c_);
  r--;
  c--;
  return inp.d[c * inp.nr() + r];
}

inline double &at(const VEC<double> &&inp, int r, int c) {

  ass(inp.im() == true, "Input is not a matrix!");
  r--;
  c--;
  return inp.d[c * inp.nr() + r];
}

inline double &at(const VEC<double> &&inp, double r_, double c_) {

  ass(inp.im() == true, "Input is not a matrix!");
  int r = d2i(r_);
  int c = d2i(c_);
  r--;
  c--;
  return inp.d[c * inp.nr() + r];
}