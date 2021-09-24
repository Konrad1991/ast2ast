#include <iostream>
#include <memory>
#include <vector>


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



  std::vector<double> v = {1., 2., 3.};

  SPAN s(v.data(), 2);

  s[0] = 10;
  std::cout << v[0] << std::endl;
}
