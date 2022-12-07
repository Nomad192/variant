#pragma once

#include "enable.h"
#include "trivial.h"

template <typename T>
concept Default_Constructable = std::is_default_constructible_v<T>;


template <typename First, typename... Rest_Types>
struct variant : trivial_move_assign_base<First, Rest_Types...>,
                 //default_constructable_first<First>,
                 //nothrow_default_constructable_first<First>,
                 ///nothrow_move_construct_base<First, Rest_Types...>,
                 /// nothrow_move_assign_base<First, Rest_Types...>,
                 copy_assign_base<First, Rest_Types...>,
                 move_assign_base<First, Rest_Types...>,
                 copy_construct_base<First, Rest_Types...>,
                 move_construct_base<First, Rest_Types...> {

  using trivial_move_assign_base<First, Rest_Types...>::trivial_move_assign_base;

  constexpr variant() = default;
  //constexpr variant() noexcept(std::is_nothrow_default_constructible_v<First>)  = default;
  //constexpr variant() requires (!Default_Constructable<First>) = delete;
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

  template <size_t Index>
  constexpr typename get_type_by_index<Index, First, Rest_Types...>::type& get_from_index() const noexcept {
    return storage_t<First, Rest_Types...>::template get<Index>(this->storage);
  }

  template <typename T>
  constexpr T& get_from_type() const noexcept {
    return storage_t<First, Rest_Types...>::template get<get_index_by_type<T, First, Rest_Types...>::index>(this->storage);
  }
};

template <typename T, typename... Types>
constexpr bool holds_alternative(variant<Types...> const& v) {
  return get_index_by_type<T, Types...>::index == v.index();
}

template <size_t Index, typename... Types>
constexpr typename get_type_by_index<Index, Types...>::type& get(variant<Types...>& v) {
  return v.template get_from_index<Index>();
}

template <typename T, typename... Types>
constexpr T& get(variant<Types...>& v) {
  return v.template get_from_type<T>();
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
