# Type system

- each variables type is a pre_type_node
- pre_type_node stores:
   * name
   * data_struct
   * const or mut
   * copy or ref
   * r_fct
   * real_type
- data_struct is an instance of one of these classes:
   * scalar
   * vec
   * mat
   * arr
   * borrow_vector
   * borrow_matrix
   * borrow_array
- Each data_struct class has an attribute base_type
- getter functions:
   * get_copy_or_ref
   * get_const_or_mut
   * get_base_type
   * get_data_struct (removes borrow)
   * get_data_struct_verbose
- pre_type_node has a signature method
which creates the respective signature
- pre_type_node has a declare method which declares the
respective variables.
Missing is so far that if the output is an R function
the SEXP variables have to be assigned to the C++ variables.
Moreover, don't forget, to add the cast functions for scalars.
Furthermore, currently scalar SEXPs are not handled so far.
In addition if the output is XPtr consider that declare is not needed.
Maybe it would be a good idea that in this case an empty line is returned.
Thereby, declare can be called without checking whether it is required or not.

# Collections

 multiple class instances of new_type can be stored
 in a collection. collection(cool_type)
 The user has to pass a list of cool_types.
 collections are in C++ template<typename T> struct Collection {std::vector<T> data;}
 At the beginning the user can only:
* push new elements
   * extract/get one single element
   * set/overwrite a single element
