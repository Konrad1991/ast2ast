#ifndef MUL
#define MUL

#include "vec.hpp"


template<typename T, typename L, typename R>
class VVTIMES {

private:
  const L& l;
  const R& r;
  std::vector<int> indices1;
  std::vector<int> indices2;

public:
  VVTIMES(const L &a, const R &b ) :
     l(a), r(b) {
       if(l.size() > r.size()) {
         assert( (l.size() % r.size()) == 0);
         indices1.resize(l.size());
         indices2.resize(l.size());
         for(int i = 0; i < indices2.size(); i++) {
           indices1[i] = i;

           int times = floor(i/r.size());
           indices2[i] =  i - times*r.size();
         }
       } else if(r.size() > l.size()) {
         assert( (r.size() % l.size()) == 0);
         indices1.resize(r.size());
         indices2.resize(r.size());
         for(int i = 0; i < indices2.size(); i++) {
           indices2[i] = i;

           int times = floor(i/l.size());
           indices1[i] =  i - times*l.size();
         }
     } else if(r.size() == l.size()) {
       indices1.resize(l.size());
       indices2.resize(r.size());
       for(int i = 0; i < indices2.size(); i++) {
         indices1[i] = i;
         indices2[i] = i;
       }
     }
   }

   T operator[](const int i) const {
     return l[indices1[i]] * r[indices2[i]];
   }

   int size() const {
     return l.size();
   }

};


template<typename T, typename L, typename R>
VEC< T, VVTIMES< T, L, R > > operator*(const VEC<T, L>& a, const VEC<T, R>& b) {
    return VEC<T, VVTIMES<T, L, R> > (VVTIMES<T, L, R>(a.data(), b.data() ) );
}

template<typename T, typename L, typename R>
class VSTIMES {

private:
  const L& l;
  const R& r;

public:
  VSTIMES(const L &a, const R &b ) :
     l(a), r(b) {}

   T operator[](const int i) const {
     return l[i] * r;
   }

   int size() const {
     return l.size();
   }

};

template<typename T, typename L, typename R>
VEC< T, VSTIMES< T, L, R > > operator*(const VEC<T, L>& a, const R& b) {
    return VEC<T, VSTIMES<T, L, R> > (VSTIMES<T, L, R>(a.data(), b ) );
}



template<typename T, typename L, typename R>
class SVTIMES {

private:
  const L& l;
  const R& r;

public:
  SVTIMES(const L &a, const R &b ) :
     l(a), r(b) {}

   T operator[](const int i) const {
     return l * r[i];
   }

   int size() const {
     return r.size();
   }

};


template<typename T, typename L, typename R>
VEC< T, SVTIMES< T, L, R > > operator*(const L& a, const VEC<T, R>& b) {
    return VEC<T, SVTIMES<T, L, R> > (SVTIMES<T, L, R>(a, b.data() ) );
}


#endif
