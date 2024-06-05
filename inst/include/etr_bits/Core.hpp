#ifndef UTILSTRAITS_H
#define UTILSTRAITS_H

// this is sorted by clang format be careful
#include "./Core/Header.hpp"
#include "./Core/Utils.hpp"
#include "./Core/Reflection.hpp"
#include "./Core/Traits.hpp"
#include "./Core/Types.hpp"
#include "./Core/Concepts.hpp"
#include "./Core/MatrixParameter.hpp"
#include "./Core/BaseStore.hpp"
#include "./Core/Borrow.hpp"
#include "./Core/BorrowSEXP.hpp"
#include "./Core/SubsetClass.hpp"

namespace etr {
template <typename T, typename Trait, typename CTrait>
struct Buffer : public BaseStore<T> {
  using RetType = T; // BaseType;
  using TypeTrait = Trait;
  using CaseTrait = CTrait;
};

} // namespace etr

#endif
