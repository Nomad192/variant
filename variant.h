#pragma once

#include "enable.h"
#include "trivial.h"

template <typename First, typename... Rest_Types>
struct variant : trivial_move_assign_base<First, Rest_Types...>,
                 default_constructable_first<First>,
                 nothrow_default_constructable_first<First>,
                 nothrow_move_construct_base<First, Rest_Types...>,
                 /// nothrow_move_assign_base<First, Rest_Types...>,
                 copy_assign_base<First, Rest_Types...>,
                 move_assign_base<First, Rest_Types...>,
                 copy_construct_base<First, Rest_Types...>,
                 move_construct_base<First, Rest_Types...> {

  using trivial_move_assign_base<First, Rest_Types...>::trivial_move_assign_base;

  constexpr variant() noexcept(std::is_nothrow_constructible_v<First>) = default;
  constexpr variant(const variant&) = default;
  constexpr variant(variant&&) noexcept(std::conjunction_v<std::is_nothrow_move_constructible<Rest_Types>...,
                                                           std::is_nothrow_move_constructible<First>>) = default;
  variant& operator=(const variant&) = default;
  variant& operator=(variant&&) noexcept(
      std::conjunction_v<std::is_nothrow_move_assignable<Rest_Types>..., std::is_nothrow_move_assignable<First>>&&
          std::conjunction_v<std::is_nothrow_move_constructible<Rest_Types>...,
                             std::is_nothrow_move_constructible<First>>) = default;

public:
  template <typename T, typename... Types>
  friend constexpr bool holds_alternative(variant<Types...> const& v);

  constexpr size_t index() const noexcept {
    return this->current_index;
  }
};

template <typename T, typename... Types>
constexpr bool holds_alternative(variant<Types...> const& v) {
  return get_index_by_type<0, T, Types...>::value == v.current_index;
}

template <size_t Index, typename... Types>
constexpr size_t get(variant<Types...> const& v) {
  return v.index();
}

// template <typename... Types>
// constexpr bool operator==(variant<Types...> const& a, variant<Types...> const& b) {
//   if (bool(a) != bool(b))
//     return false;
//
//   if (!bool(a) || !bool(b))
//     return true;
//
//   return *a == *b;
// }
//
// template <typename... Types>
// constexpr bool operator!=(variant<Types...> const& a, variant<Types...> const& b) {
//   if (bool(a) != bool(b))
//     return true;
//
//   if (!bool(a) || !bool(b))
//     return false;
//
//   return *a != *b;
// }
//
// template <typename... Types>
// constexpr bool operator<(variant<Types...> const& a, variant<Types...> const& b) {
//   if (!bool(b))
//     return false;
//
//   if (!bool(a))
//     return true;
//
//   return *a < *b;
// }
//
// template <typename... Types>
// constexpr bool operator<=(variant<Types...> const& a, variant<Types...> const& b) {
//   if (!bool(a))
//     return true;
//
//   if (!bool(b))
//     return false;
//
//   return *a <= *b;
// }
//
// template <typename... Types>
// constexpr bool operator>(variant<Types...> const& a, variant<Types...> const& b) {
//   if (!bool(a))
//     return false;
//
//   if (!bool(b))
//     return true;
//
//   return *a > *b;
// }
//
// template <typename... Types>
// constexpr bool operator>=(variant<Types...> const& a, variant<Types...> const& b) {
//   if (!bool(b))
//     return true;
//
//   if (!bool(a))
//     return false;
//
//   return *a >= *b;
// }
