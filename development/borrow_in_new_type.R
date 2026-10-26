
types <- function() {
  new_type(
    Foo,
    slots(
      x |> type(borrow_vec(double)),
      y |> type(vec(double))
    )
  )
}

f <- function(foo, bv) {
  argtypes(
    # Here the type is constructed from fSEXP
    # Thus, this works, as the raw double* from x is borrowed
    # The underlying type of x is Array<Double, Borrow<Double>>
    foo |> type(Foo),
    # Normal borrow vec
    bv |> type(borrow_vec(double))
  )
  # The difference between foo and bv
  owning |> type(vec(double)) <- bv # This works
  # Here the instantation of foo would only work if we allow
  # the empty constructor for Array<Double, Borrow<Double>>
  new_foo |> type(foo)
  new_foo$x <- bv # This would be ok (aka not dangerous)
  # This is dangerous. Because the pointer of owning
  # could become invalid (e.g. swap with the pointer from temp)
  new_foo$x <- owning
  # Also using Array<Double, ArrayRef<Double>> would not help
  # Because this is also a different type than Array<Double, Borrow<Double>>
  #
  # Thus, we require two different type defintions for foo.
  # But the R user should see them as one type.
  # Internally we would have to create
  # new_type(
  #   Foo_OWNING,
  #   slots(
  #     x |> type(vec(double)),
  #     y |> type(vec(double))
  #   )
  # )
  # And then new_foo would be of type Foo_OWNING
  # But this is completly confusing because the R user
  # thinks that if something is a borrow_vec it will change
  # also when copied (i.e. not by reference) to other functions
  #
  # Thus, the second type of new_type has to look like this
  # new_type(
  #   Foo_REF,
  #   slots(
  #     x |> type(borrow_vec_ref(double)),
  #     y |> type(vec(double))
  #   )
  # )
  # Then, x in new_foo would be of type
  # Array<Double, ArrayRef<Double>>
  # Thus, x would behave like Array<Double, Borrow<Double>>
}
