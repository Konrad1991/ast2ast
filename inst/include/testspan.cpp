#include <iostream>
#include <memory>


class SPAN {

  double* sp;
  int size;

  public:
  SPAN(double* p, int size_) : size(size_), sp(p) {}

  double& operator[](int i) {
    return sp[i];
  }

};


int main() {
  int n = 10;
  double* p = new double[n];

  for(int i = 0; i < n; i++) {
    p[i] = static_cast<double>(i);
  }

  SPAN sp(p, n);

  sp[3] = 80;
  std::cout << sp[3] << " " << p[3] << std::endl;

  delete[] p;
}
