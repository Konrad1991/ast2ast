#include <iostream>
#include <memory>


class VEC {
  public:
    double* data;
    int size;
  VEC(double* p_, int size_) : size(size_) {
    data = new double[size_];
    for(int i = 0; i < size_; i++) {
      data[i] = p_[i];
    }
  }

  ~VEC() {
    delete[] data;
  }

  double operator[](int i) {
    return data[i];
  }
};

class SPAN {
  public:
  double** sp;
  int size;
  SPAN(double* p, int size_) : size(size_) {
    sp = &p;
  }

  double operator[](int i) {
    double temp = **sp+i;
    return *sp[i];
  }

};


// works
class SPAN2 {
  public:
  double*& sp;
  int size;
  SPAN2(double*& p, int size_) : size(size_), sp(p) {}

  double operator[](int i) {
    return *(sp + i);
  }

  SPAN2 operator=(double inp) {
    *sp = inp;
    return *this;
  }

};


int main() {
  int n = 10;
  double* p = new double[n];

  for(int i = 0; i < n; i++) {
    p[i] = static_cast<double>(i);
  }

  VEC v(p, n);
  SPAN s(p, n);
  SPAN2 s2(p,n);

  for(int i = 0; i < v.size; i++) {
    std::cout << v[i] << std::endl;
  }

  p[0] = 30;
  double*& ref = p;
  std::cout << *(s2.sp + 0) << std::endl;

  s2 = 70;
  s2.sp[1] =34567;
  std::cout << ref[0]<< " " << p[0] << " " << s2[0] + s2[1] << std::endl;

  v.data[3] = 345;
  std::cout << v[3] << std::endl;

  delete[] p;
}
