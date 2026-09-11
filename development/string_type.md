# Character / string type

**Status:** design only, not started.

Additive. Strings are inert: no arithmetic, no `real_type`, no tape. Sits next to
the numeric core, does not touch it.

## Design direction

- Scalar `character` = wrapper around `std::string` + an `NA` flag.
- `Char` inside the existing `Array` = large work. Instead: a dedicated `ArrayChar`.
- No mixing of `Char` with the numeric scalars.
- No `ArrayChar` + `Array`. Those cases are only `etr::c`, `paste` and similar.
- Important: If a variable is of type character or array character it cannot change its type.
  --> character variables should be handled in the same way as new_types.
      So they are in the R transpiler of type pre_type_node but the base_type is character

## What can be done

- character(...)
- vector(mode = "character", length = ...)
- c() --> only allow to concatenate Char, ArrayChars --> to a ArrayChar
  --> no mixing of Char, other Scalars possible
- == and != for Char --> but only compare Char with Char not Char with other Scalar
- paste0
  --> again paste0 will only handle Char!
- length
- print
- as.character()
- as.numeric()
- subsetting ArrayChar
- subsetting with Char or ArrayChar
  --> only for hashmaps possible I guess
