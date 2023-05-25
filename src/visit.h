#pragma once

#include "get.h"
#include "visit_table.h"

///==================================================================================================================///
/// visit

namespace visit_helper {

///------------------------------------------------------------------------------------///
/// Apply_Indexes
template <typename Visitor, typename... Variants>
struct Apply {
  template <size_t... Indexes>
  constexpr auto operator()(Visitor&& visitor, Variants&&... variants) {
    return std::forward<Visitor>(visitor)(get<Indexes>(std::forward<Variants>(variants))...);
  }
};

/// END: Apply_Indexes
///------------------------------------------------------------------------------------///

template <typename Visitor, typename... Variants>
using NormalReturnType = decltype(std::forward<Visitor>(std::declval<Visitor>())(
    get<0>(std::forward<Variants>(std::declval<Variants>()))...)); /// Visitor Return Type

template <typename ApplyVisitor, typename Visitor, typename... Variants>
using Normal_Handler_Function_Pointer =
    NormalReturnType<Visitor, Variants...> (*)(ApplyVisitor&&, Visitor&&,
                                               Variants&&...); /// get_func function pointer

///------------------------------------------------------------------------------------///
/// visit_table

template <typename Visitor, typename... Variants>
constexpr auto visit_table(Visitor&& visitor, Variants&&... variants) {
  using A = Apply<Visitor, Variants...>;
  using FP = Normal_Handler_Function_Pointer<A, Visitor, Variants...>;

  auto handler = Storage<FP, A, Visitor, Variants...>::table.get_func(std::forward<Variants>(variants).index()...);

  A apply;
  return handler(std::forward<A>(apply), std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
}

/// END: visit_table
///------------------------------------------------------------------------------------///
/// do_visit (some optimizations)

template <typename Visitor, typename... Variants>
constexpr auto do_visit(Visitor&& visitor, Variants&&... variants) { /// Visit optimizations
  if constexpr (sizeof...(Variants) == 0) {                          /// Optimization, the number of variants is zero
    return std::forward<Visitor>(visitor)();
  }

  if ((std::forward<Variants>(variants).valueless_by_exception() || ...)) {
    throw bad_variant_access();
  }

  /// start at <0, 0> (<index first variant , index current variant>)
  return visit_table<Visitor, Variants...>(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
}

/// END: do_visit (some optimizations)
///------------------------------------------------------------------------------------///
} // namespace visit_helper

template <typename Visitor, typename... Variants>
constexpr auto visit(Visitor&& visitor, Variants&&... variants) {
  return visit_helper::do_visit(std::forward<Visitor>(visitor), std::forward<Variants>(variants)...);
}

/// END: visit
///==================================================================================================================///
