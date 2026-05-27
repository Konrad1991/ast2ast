
- check that it is really not possible to Borrow in case realtype is Dual or ReverseDouble
- Borrow only uses p_na if a dedicated bool* is passed to it.
   * Currently R objects only give the raw pointer (p) but it is never checked whether they are NA
   Maybe it is possible to extract also the NA state from SEXP objects, which could be used to
   initialize bool* p_na.
   * In case no bool* p_na is passed. I could allocate it on the heap and manage lifetime.
