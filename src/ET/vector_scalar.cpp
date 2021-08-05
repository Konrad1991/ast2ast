// https://stackoverflow.com/questions/35613620/how-should-i-use-expression-templates-in-order-to-implement-scalar-multiplicatio
// http://www.cplusplus.com/forum/general/72582/
// http://log.fundamental-code.com/2014/09/19/expression-templates-and-vector-math-in-cpp.html
// https://sodocumentation.net/cplusplus/topic/3404/expression-templates
// https://gist.github.com/pranavladkat/8353eacdde55fa6c590b
// http://www.it.uu.se/research/conf/SCSE07/material/Pflaum.pdf

#include <iostream>


#include <cassert>
#include <iostream>

namespace expr
{
	typedef unsigned int uint;

	// globals to count constructor and destructor calls
	// of a vec
	uint ctor_calls = 0;
	uint dtor_calls = 0;
	// print the numbers
	void printctor()
	{
		std::cout << "ctor/dtor = " << ctor_calls
                << "/" << dtor_calls << "\n";
	}

	// an example vec container of three successive floats
	class vec
	{
		public:

		float v[3];

		// note: the {...} is c++0x initializer syntax
		vec () : v({0,0,0}) { ++ctor_calls; }
		vec (const vec& o) : v({o.v[0], o.v[1], o.v[2]}) { ++ctor_calls; }
		vec (float x,float y,float z) : v({x,y,z}) { ++ctor_calls; }

		~vec() { ++dtor_calls; }

		// return indexed value
		float operator[](uint index) const { return v[index]; }
		// return indexed reference
		float& operator[](uint index) { return v[index]; }

		void print() { std::cout << "<"<<v[0]<<","<<v[1]<<","<<v[2]<<">\n"; }

		// assignment to an expression
                // E must have operator[](uint)
		template <class E>
		vec& operator= (const E& x)
		{
			for (uint i=0; i!=3; ++i) (*this)[i] = x[i];
			return *this;
		}
	};


	// basic catch-all expression node
	// L and R must provide operator[](uint)
	// O must provide static function float eval(float,float)
	template <class L, class O, class R>
	struct expression
	{
		expression(const L& l, const R& r)
			:	l(l), r(r) { }

		float operator[](const uint index) const
		{
			return O::eval(l[index], r[index]);
		}

		const L& l;
		const R& r;
	};

	// wraps a reference to float into an operator[](uint) entity
	class scalar
	{
		public:

		scalar(const float& t) : t(t) { }

		// act like an endless vector of ts
		float operator[](uint) const { return t; }

		const float& t;
	};

	// an operation function object
	struct plus
	{
		static float eval(const float a, const float b) { return a + b; }
	};

	// anything + anything
	template <class L, class R>
	expression<L,plus,R> operator+(const L& l, const R& r)
	{
		return expression<L,plus,R>(l, r);
	}

	// anything + scalar
	template <class L>
	expression<L,plus,scalar> operator+(const L& l, const float& r)
	{
		return expression<L,plus,scalar>(l, r);
	}


}

void do_some()
{
	using namespace expr;

	vec a(1,2,3), b(2,3,4), c(3,4,5);
	a.print(); b.print(); c.print();

	// works
	a = b + c;
	a.print();
	assert( a.v[0] == 5 && a.v[1] == 7 && a.v[2] == 9 );

	// does not work -> segfault
	a = b + 1.f;
	a.print();
	assert( a.v[0] == 3 && a.v[1] == 4 && a.v[2] == 5 );
}

int main()
{
	do_some();

	// check ctor calls
	expr::printctor();

    return 0;
}
