#include <iostream>
#include <vector>
#include "util.hpp"

using namespace std;

template< typename T, typename R = vector<T> >
class Vector {
private:
   R r;   // data representation
public:

   Vector(const size_t n) : r(n) {}

   Vector(const size_t n, const double initialValue) : r(n, initialValue) {}

   // copy constructor
   Vector(const R &other) : r(other) {}

   // assignment operator: same type
   T& operator=(const T &other)
   {
      for (size_t i = 0; i < r.size(); ++i)
         r[i] = other[i];

      return *this;
   }

   // assignment operator: other type
   template< typename T2, typename R2 >
   Vector& operator=(const Vector<T2, R2> &other)
   {
      for (size_t i = 0; i < r.size(); ++i)
         r[i] = other[i];

      return *this;
   }

   size_t size() const
   { return r.size(); }


   T operator[](size_t i) const
   {
     return r[i]; }

   T& operator[](size_t i)
   {
     return r[i]; }

   const R& data() const
   { return r; }

   R& data()
   { return r; }
};

// Addition
template< typename T, typename Op1 , typename Op2 >
class VecAdd {
private:
   const Op1 &op1;
   const Op2 &op2;

public:
   VecAdd(const Op1 &a, const Op2 &b ) :
      op1(a), op2(b) {}

   T operator[](const size_t i) const
   {

     int i1 = compare(op1.size(), i);// evtl. muss das besser in die Vektor Klasse verlegt werden. 
     int i2 = compare(op2.size(), i);
     return op1[i1] + op2[i2]; }

   size_t size() const
   { return op1.size(); }
};

// Multiplikation (elementweise)
template< typename T, typename Op1 , typename Op2 >
class VecMul {
private:
   const Op1 &op1;
   const Op2 &op2;

public:
   VecMul(const Op1 &a, const Op2 &b ) :
      op1(a), op2(b) {}

   T operator[](const size_t i) const
   { return op1[i] * op2[i]; }

   size_t size() const
   { return op1.size(); }
};

template< typename T, typename R1, typename R2 >
Vector< T, VecAdd< T, R1, R2 > >
operator+(const Vector<T, R1> &a, const Vector<T, R2> &b)
{
   return Vector<T, VecAdd< T, R1, R2 > >(VecAdd< T, R1, R2 >(a.data(), b.data()));
}

template< typename T, typename R1, typename R2 >
Vector< T, VecMul< T, R1, R2 > >
operator*(const Vector<T, R1> &a, const Vector<T, R2> &b)
{
   return Vector<T, VecMul< T, R1, R2 > >(VecMul< T, R1, R2 >(a.data(), b.data()));
}








// Multiplikation (elementweise)
template< typename T, typename Op1 , typename Op2 >
class VecScalMul {
private:
   const Op1 &op1;
   const Op2 &op2;

public:
   VecScalMul(const Op1 &a, const Op2 &b ) :
      op1(a), op2(b) {}

   T operator[](const size_t i) const
   { return op1[i] * op2; }

   size_t size() const
   { return op1.size(); }
};

template< typename T, typename R1, typename R2 >
Vector< T, VecScalMul< T, R1, R2 > >
operator*(const Vector<T, R1> &a, const R2 &b)
{
   return Vector<T, VecScalMul< T, R1, R2 > >(VecScalMul< T, R1, R2 >(a.data(), b) );
}






// Multiplikation (elementweise)
template< typename T, typename Op1 , typename Op2 >
class ScalVecMul {
private:
   const Op1 &op1;
   const Op2 &op2;

public:
   ScalVecMul(const Op1 &a, const Op2 &b ) :
      op1(a), op2(b) {}

   T operator[](const size_t i) const
   { return op1 * op2[i]; }

   size_t size() const
   { return op2.size(); }
};

template< typename T, typename R1, typename R2 >
Vector< T, ScalVecMul< T, R1, R2 > >
operator*(const R1 &a, const Vector<T, R2> &b)
{
   return Vector<T, ScalVecMul< T, R1, R2 > >(ScalVecMul< T, R1, R2 >(a, b.data()) );
}




























// Multiplikation (elementweise)
template< typename T, typename Op1 , typename Op2 >
class VecScalAdd {
private:
   const Op1 &op1;
   const Op2 &op2;

public:
   VecScalAdd(const Op1 &a, const Op2 &b ) :
      op1(a), op2(b) {}

   T operator[](const size_t i) const
   { return op1[i] + op2; }

   size_t size() const
   { return op1.size(); }
};

template< typename T, typename R1, typename R2 >
Vector< T, VecScalAdd< T, R1, R2 > >
operator+(const Vector<T, R1> &a, const R2 &b)
{
   return Vector<T, VecScalAdd< T, R1, R2 > >(VecScalAdd< T, R1, R2 >(a.data(), b) );
}


// Multiplikation (elementweise)
template< typename T, typename Op1 , typename Op2 >
class ScalVecAdd {
private:
   const Op1 &op1;
   const Op2 &op2;

public:
   ScalVecAdd(const Op1 &a, const Op2 &b ) :
      op1(a), op2(b) {}

   T operator[](const size_t i) const
   { return op1 + op2[i]; }

   size_t size() const
   { return op2.size(); }
};

template< typename T, typename R1, typename R2 >
Vector< T, ScalVecAdd< T, R1, R2 > >
operator+(const R1 &a, const Vector<T, R2> &b)
{
   return Vector<T, ScalVecAdd< T, R1, R2 > >(ScalVecAdd< T, R1, R2 >(a, b.data()) );
}

int main() {
Vector<double> x(2);
Vector<double> y(6);
const double c = 2.;

for(int i = 0; i < y.size(); i++) {
  y[i] = 1.;
}
x[0] = 10.;
x[1] = 20.;

/*
y = x*c + 50. +x * y*3.;
*/

for(int i = 0; i < y.size(); i++) {
  std::cout << y[i] << std::endl;
} std::cout << std::endl;

y = y*2. + x;


for(int i = 0; i < y.size(); i++) {
  std::cout << y[i] << std::endl;
} std::cout << std::endl;

int siz1 = 10;
int siz2 = 2;
int i = 6;

//std::cout << (i /siz2) << std::endl;

}
