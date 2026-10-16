# Generic `borrow(T)`

Status: idea (2026-10-14), postponed. Named vectors use `named_borrow_vec`
first (see `named_vector_layout.md`).

## Why
- today 6 spellings: borrow_vec/borrow_vector, borrow_mat/borrow_matrix,
  borrow_arr/borrow_array; named vectors add named_borrow_vec
- names belong to the type, memory location belongs to the variable
  -> one wrapper for all

## Syntax
```r
borrow(vec(double)), borrow(mat(double)), borrow(arr(double)), borrow(State)
y |> type(borrow(State)) |> ref()
```

## Rules
- borrow(T): T in vec/mat/arr(...) or a layout (new_layout)
- borrow_vec(double), named_borrow_vec(State) etc. stay valid as aliases
  (borrow_* are on CRAN and documented)
- deprecation of the old names: decide later
- parser only: borrow(vec(double)) -> same internal type as borrow_vec(double)

## Where
- R/TypeParser.R (type constructors)
- docs: MemoryModel.Rmd, DetailedDocumentation.Rmd, translate.Rd
