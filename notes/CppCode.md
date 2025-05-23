# 🟦 Kanban Board: ToDo in ast2ast

## 🎯Goal: Matrix System Extension
- For the data structure Mat the Buffer and Borrow
  have to contain the attributes is_matrix, nrow and ncol

## TO DO
- [x] add borrow SEXP object to struct Borrow
- [x] re-add pointer constructor to borrow
- [x] Update assignment in Matrix data structure
  - [x] retrieve new ncol and nrow if a matrix is received
  - [x] if a vector is received check that the size matches ncol*nrow

## 🟩 DONE
- [x] Defined `Mat<T>` struct (with Buffer and Borrow currently)
- [x] Agreed on unified API matching `Vec`.


## 🎯Goal: Efficient handling of temporary objects

## TO DO
- [x] Update and test the Concepts to check whether an L- or R-Vec/Mat is handled
- [ ] instead of copying move r objects (swap pointers)
- [ ] Replace all the IsRVec with IsRArrayLike

## 🟩 DONE
- [x] defined concepts IsMat, IsRMat, IsArrayLike and IsRArrayLike


## 🎯Goal: lazy Subsetting

## TO DO
- [ ] check what is missing for subsetting vectors
- [ ] write subsetting for matrices


## 🎯Goal: Check the remaining stuff
- adapt it to the new data structures and consider Mat

## TO DO
- [ ] Allocation
- [ ] helpers
- [ ] printing
