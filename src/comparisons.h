#pragma once

#include "visit.h"

struct Visiter
{
  constexpr bool operator()(auto& a, auto& b)
  {
    if constexpr (std::is_same_v<decltype(a), decltype(b)>)
      return a == b;
    return false;
  }
} vis;

template <typename... Types>
constexpr bool operator==(variant<Types...> const &lhs, variant<Types...> const &rhs)
{
  if (lhs.index() != rhs.index()) {
    return false;
  }
  return visit_helper::do_visit(vis, lhs, rhs);
}

template <typename... Types>
constexpr bool operator !=(const variant<Types...> &first, const variant<Types...> &second)
{
  return !(first == second);
}