## Holder and ConstHolder

- adapt preserving subsetting in the same way as unary and binary operations

## Core Tests

- [ ] Borrow
- [ ] Buffer
- [ ] Concepts
- [ ] Holder
- [ ] Traits
- [ ] Utils

## DataStructures

- [ ] Vec
- [ ] Mat

## Allocation

- [ ] concatenate
- [ ] colon
- [ ] rep
- [ ] logical/integer/numeric
- [ ] matrix

## Calculations

- [ ] unary Calculations
- [ ] binary Calculations

## Interpolation

- [ ] catmull rom spline
- [ ] check whether linear will be added to ast2ast

## Subsetting

- [ ] Preserving subsetting
- [ ] Simplifying subsetting

## Utilities

- [ ] Helper: is.na, is.inf, is.finite, length, and dim
- [ ] printing
- [ ] SEXP conversions: Evaluate, SEXP2Scalar, Cast

## What is stored in what

### The Buffer:

Buffer is the most common storage.
Constructors:
  * empty constructor
  * const Buffer&
  * Buffer&&
  * std::size_t
  * std::size_t, std::size_t
Assignments:
  * const Buffer&
  * Buffer&&

Buffer is used as standard container.
* If the user passes a SEXP object to a function it is copied by default into Vec<Buffer>
* Each Vec or Mat variable defined within the function uses Buffer to manage memory.

### The Borrow:

Borrow does not own the memory. Two applications are in mind:
* The user wants not to copy a SEXP element.
  Either because it is too expensive
  or because it should be modified directly.
  This is done by Vec<double, Borrow<double>> vec = vecSEXP;
* If an external pointer is created the function can be used in other C++ programs.
  It is handy to wrap a pointer and its size in a Borrow and passed it to the
  user function. Vec<double, Borrow<double>> vec(ptr, size); fct_ptr(vec);
The assumption is made that the class already gets a Borrow as argument or wraps the SEXP
into a Borrow instance. Thus, only the pointer constructor for vector and matrix, and the
SEXP constructor are handled.
It is pivotal to make sure that Borrow and Buffer behave mostly in the same way.
* the std::size_t constructor is deleted, TODO: why is the std::size_t, std::size_t neither defined or even better deleted?
* Has additional constructors:
  - T*, std::size_t; T*, int
  - T*, std::size_t, std::size_t
  - T*, int, int
* resize only if possible. It can shrink and regrow until the size/capacity is reached

## What combinations have to be tested

The possible types are:

1. bool
2. int
3. double

4. Vec<bool, Buffer<bool, LBufferTrait>>
5. Vec<int, Buffer<int, LBufferTrait>>
6. Vec<double, Buffer<double, LBufferTrait>>

7. Vec<bool, Buffer<bool, RBufferTrait>>
8. Vec<int, Buffer<int, RBufferTrait>>
9. Vec<double, Buffer<double, RBufferTrait>>

10. Vec<bool, Borrow<bool>>
11. Vec<int, Borrow<int>>
12. Vec<double, Borrow<double>>

13. Vec<double, UnaryOperation<L/R SomeInnerType, SomeTrait>>

14. Vec<bool, BinaryOperation<L/R SomeInnerType, L/R SomeInnerType, SomeTrait>>
15. Vec<int, BinaryOperation<L/R SomeInnerType, L/R SomeInnerType, SomeTrait>>
16. Vec<double, BinaryOperation<L/R SomeInnerType, L/R SomeInnerType, SomeTrait>>

17. Vec<bool, SubsetClass<L/R SomeInnerType, L/R SomeInnerType, SomeTrait>>
18. Vec<int, SubsetClass<L/R SomeInnerType, L/R SomeInnerType, SomeTrait>>
19. Vec<double, SubsetClass<L/R SomeInnerType, L/R SomeInnerType, SomeTrait>>

20. Mat<bool, Buffer<bool, LBufferTrait>>
21. Mat<int, Buffer<int, LBufferTrait>>
22. Mat<double, Buffer<double, LBufferTrait>>

23. Mat<bool, Buffer<bool, RBufferTrait>>
24. Mat<int, Buffer<int, RBufferTrait>>
25. Mat<double, Buffer<double, RBufferTrait>>

26. Mat<bool, Borrow<bool>>
27. Mat<int, Borrow<int>>
28. Mat<double, Borrow<double>>

29. Mat<bool, SubsetClass<L/R SomeInnerType, L/R SomeInnerType, SomeTrait>>
30. Mat<int, SubsetClass<L/R SomeInnerType, L/R SomeInnerType, SomeTrait>>
31. Mat<double, SubsetClass<L/R SomeInnerType, L/R SomeInnerType, SomeTrait>>

### Which types can be at LHS:

Arithmetics: 1, 2, 3
L Buffers: 4, 5, 6, 23, 24, 25
Borrows: 10, 11, 12, 26, 27, 28
Subsets: 17, 18, 19, 29, 30, 31

### Expressions

All types can be used within expressions. An exception is that the value type has to be a double for unary operations.
