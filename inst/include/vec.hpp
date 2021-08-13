#ifndef VEC_H
#define VEC_H

#include "util.hpp"

template< typename T, typename R = std::vector<T> >
class VEC {

private:
  R d; // data
public:
  bool subsetted;
  std::vector<int> indices;
  VEC(const int n) : d(n), subsetted(0) {}
  VEC(const int n, const double value) : d(n, value), subsetted(0) {}
  VEC(const R& other_vec) : d(other_vec), subsetted(0) {}

  T& operator=(const T &other_vec) {
    if(subsetted == false) {
      for(int i = 0; i < d.size(); i++) {
        d[i] = other_vec[i];
      }
    } else {
      for(int i = 0; i < indices.size(); i++) {
        d[indices[i]] = other_vec[indices[i]];
      }
    }

    subsetted = false;
    return *this;
  }

  template<typename T2, typename R2>
  VEC& operator=(const VEC<T2, R2> &other_vec) {
    if(subsetted == false) {
      for(int i = 0; i < d.size(); i++) {
        d[i] = other_vec[i];
      }
    } else {
      for(int i = 0; i < indices.size(); i++) {
        d[indices[i]] = other_vec[i];
      }
    }

    subsetted = false;

    return *this;
  }


int size() const {
   return d.size();
}

 T operator[](int i) const {
   return d[i];
 }

 T& operator[](int i) {
   return d[i];
 }

 T operator()(int i) const {
   return d[i];
 }

 T& operator()(int i) {
   return d[i];
 }

 const R& data() const {
   return d;
 }

 R& data() {
   return d;
 }


 void data_sub(std::vector<double>& temp) const{
    temp.resize(indices.size());
    for(int i = 0; i < temp.size(); i++) {
      temp[indices[i]] = d[indices[i]];
    }
}


VEC& subset(std::vector<int> inp) {
   subsetted = true;
   indices.resize(inp.size());
   for(int i = 0; i < inp.size(); i++) {
     indices[i] = inp[i];
   }
   return *this;
}

VEC& subset(int start, int end) {
  subsetted = true;
  indices.resize(end - start);
  for(int i = 0; i < indices.size(); i++) {
    indices[i] = start + i;
  }
  return *this;
}


friend std::ostream& operator<<(std::ostream& os, const VEC& v) {
  if(v.subsetted == true) {
    for(int i = 0; i < v.indices.size(); i++) {
      os << v[v.indices[i]] << std::endl;
    }
  } else {
    for(int i = 0; i < v.size(); i++) {
      os << v[i] << std::endl;
    }
  }
      return os;
};

bool is_subsetted() const {
  return subsetted;
}

};

VEC<double> subset(VEC<double>& inp, int start, int end) {
  std::vector<double> temp(end - start);
  for(int i = 0; i < temp.size(); i++) {
    temp[i] = inp(i + start);
  }
  VEC<double> t(temp);
  return t;
}

void print(VEC<double>& inp) { // const
  if(inp.subsetted == true) {
    for(int i = 0; i < inp.indices.size(); i++) {
      std::cout << inp[inp.indices[i]] << std::endl;
    }
  } else {
    for(int i = 0; i < inp.size(); i++) {
      std::cout << inp[i] << std::endl;
    }
  }
}


void print(const VEC<double>& inp, std::string&& message) {

  std::cout << message << std::endl;

  if(inp.subsetted == true) {
    for(int i = 0; i < inp.indices.size(); i++) {
      std::cout << inp[inp.indices[i]] << std::endl;
    }
  } else {
    for(int i = 0; i < inp.size(); i++) {
      std::cout << inp[i] << std::endl;
    }
  }
}

#endif
