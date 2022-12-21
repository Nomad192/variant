#pragma once

#include "visit.h"

template <typename... Types>
constexpr bool operator==(variant<Types...> const &lhs, variant<Types...> const &rhs)
{
  if (lhs.index() != rhs.index())
    return false;
  if (lhs.valueless_by_exception())
    return true;
  return visit_helper::do_visit([](auto& a, auto& b){
    if constexpr (std::is_same_v<decltype(a), decltype(b)>)
      return a == b;
    return false;
  }, lhs, rhs);
}

template <typename... Types>
constexpr bool operator!=(variant<Types...> const &lhs, variant<Types...> const &rhs)
{
  if (lhs.index() != rhs.index())
    return true;
  if (lhs.valueless_by_exception())
    return false;
  return visit_helper::do_visit([](auto& a, auto& b){
    if constexpr (std::is_same_v<decltype(a), decltype(b)>)
      return a != b;
    return false;
  }, lhs, rhs);
} /// the same thing as "return !(lhs == rhs);"

template <typename... Types>
constexpr bool operator<(variant<Types...> const &lhs, variant<Types...> const &rhs)
{
  if (rhs.valueless_by_exception())
    return false;
  if (lhs.valueless_by_exception())
    return true;
  if (lhs.index() < rhs.index())
    return true;
  if (lhs.index() > rhs.index())
    return false;

  return visit_helper::do_visit([](auto& a, auto& b){
    if constexpr (std::is_same_v<decltype(a), decltype(b)>)
      return a < b;
    return false;
  }, lhs, rhs);
}

template <typename... Types>
constexpr bool operator>(variant<Types...> const &lhs, variant<Types...> const &rhs)
{
  if (lhs.valueless_by_exception())
    return false;
  if (rhs.valueless_by_exception())
    return true;
  if (lhs.index() > rhs.index())
    return true;
  if (lhs.index() < rhs.index())
    return false;

  return visit_helper::do_visit([](auto& a, auto& b){
    if constexpr (std::is_same_v<decltype(a), decltype(b)>)
      return a > b;
    return false;
  }, lhs, rhs);
}

template <typename... Types>
constexpr bool operator<=(variant<Types...> const &lhs, variant<Types...> const &rhs)
{
  if (lhs.valueless_by_exception())
    return true;
  if (rhs.valueless_by_exception())
    return false;
  if (lhs.index() < rhs.index())
    return true;
  if (lhs.index() > rhs.index())
    return false;

  return visit_helper::do_visit([](auto& a, auto& b){
    if constexpr (std::is_same_v<decltype(a), decltype(b)>)
      return a <= b;
    return false;
  }, lhs, rhs);
}

template <typename... Types>
constexpr bool operator>=(variant<Types...> const &lhs, variant<Types...> const &rhs)
{
  if (rhs.valueless_by_exception())
    return true;
  if (lhs.valueless_by_exception())
    return false;
  if (lhs.index() > rhs.index())
    return true;
  if (lhs.index() < rhs.index())
    return false;

  return visit_helper::do_visit([](auto& a, auto& b){
    if constexpr (std::is_same_v<decltype(a), decltype(b)>)
      return a >= b;
    return false;
  }, lhs, rhs);
}
