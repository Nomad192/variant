#pragma once

#include <utility>

template <bool enabled = false, class... Types> // bool enabled = std::is_copy_constructible_v<Types>>
struct copy_construct_base_ {
  copy_construct_base_() = default;
  constexpr copy_construct_base_(const copy_construct_base_&) = delete;
  constexpr copy_construct_base_(copy_construct_base_&&) = default;
  copy_construct_base_& operator=(const copy_construct_base_&) = default;
  copy_construct_base_& operator=(copy_construct_base_&&) = default;
};

template <bool enabled = false, class... Types> // bool enabled = std::is_move_constructible_v<T>>
struct move_construct_base_ {
  move_construct_base_() = default;
  constexpr move_construct_base_(const move_construct_base_&) = default;
  constexpr move_construct_base_(move_construct_base_&&) = delete;
  move_construct_base_& operator=(const move_construct_base_&) = default;
  move_construct_base_& operator=(move_construct_base_&&) = default;
};

template <bool enabled = false,
          class... Types> // bool enabled = std::is_copy_assignable_v<T> && std::is_copy_constructible_v<T>>
struct copy_assign_base_ {
  copy_assign_base_() = default;
  constexpr copy_assign_base_(const copy_assign_base_&) = delete;
  constexpr copy_assign_base_(copy_assign_base_&&) = default;
  copy_assign_base_& operator=(const copy_assign_base_&) = delete;
  copy_assign_base_& operator=(copy_assign_base_&&) = default;
};

template <bool enabled = false,
          class... Types> // bool enabled = std::is_move_assignable_v<T> && std::is_move_constructible_v<T>>
struct move_assign_base_ {
  move_assign_base_() = default;
  constexpr move_assign_base_(const move_assign_base_&) = default;
  constexpr move_assign_base_(move_assign_base_&&) = delete;
  move_assign_base_& operator=(const move_assign_base_&) = default;
  move_assign_base_& operator=(move_assign_base_&&) = delete;
};

template <typename First, bool enabled = false>
struct default_constructable_first_ {
  default_constructable_first_() = delete;
  constexpr default_constructable_first_(const default_constructable_first_&) = default;
  constexpr default_constructable_first_(default_constructable_first_&&) = default;
  default_constructable_first_& operator=(const default_constructable_first_&) = default;
  default_constructable_first_& operator=(default_constructable_first_&&) = default;
};

template <typename First, bool enabled = false>
struct nothrow_default_constructable_first_ {
  nothrow_default_constructable_first_() noexcept(!std::is_nothrow_constructible_v<First>) = delete;
  constexpr nothrow_default_constructable_first_(const nothrow_default_constructable_first_&) = default;
  constexpr nothrow_default_constructable_first_(nothrow_default_constructable_first_&&) = default;
  nothrow_default_constructable_first_& operator=(const nothrow_default_constructable_first_&) = default;
  nothrow_default_constructable_first_& operator=(nothrow_default_constructable_first_&&) = default;
};

template <bool enabled = false, class... Types>
struct nothrow_move_construct_base_ {
  nothrow_move_construct_base_() noexcept(
      !std::conjunction_v<std::is_nothrow_move_constructible<Types>...>) = delete;
  constexpr nothrow_move_construct_base_(const nothrow_move_construct_base_&) = default;
  constexpr nothrow_move_construct_base_(nothrow_move_construct_base_&&) = default;
  nothrow_move_construct_base_& operator=(const nothrow_move_construct_base_&) = default;
  nothrow_move_construct_base_& operator=(nothrow_move_construct_base_&&) = default;
};

//template <bool enabled = false, class... Types>
//struct nothrow_move_assign_base_ {
//  nothrow_move_assign_base_() = default;
//  constexpr nothrow_move_assign_base_(const nothrow_move_assign_base_&) = default;
//  constexpr nothrow_move_assign_base_(nothrow_move_assign_base_&&) noexcept(
//      !(std::conjunction_v<std::is_nothrow_move_assignable<Types>...> &&
//        std::conjunction_v<std::is_nothrow_move_constructible<Types>...>)) = delete;
//  nothrow_move_assign_base_& operator=(const nothrow_move_assign_base_&) = default;
//  nothrow_move_assign_base_& operator=(nothrow_move_assign_base_&&) noexcept(
//      !(std::conjunction_v<std::is_nothrow_move_assignable<Types>...> &&
//        std::conjunction_v<std::is_nothrow_move_constructible<Types>...>)) = delete;
//};

///==================================================================================================================///

template <typename... Types>
struct copy_construct_base_<true, Types...> {};

template <typename... Types>
struct move_construct_base_<true, Types...> {};

template <typename... Types>
struct copy_assign_base_<true, Types...> {};

template <typename... Types>
struct move_assign_base_<true, Types...> {};

template <typename First>
struct default_constructable_first_<First, true> {};

template <typename First>
struct nothrow_default_constructable_first_<First, true> {
  nothrow_default_constructable_first_() noexcept(std::is_nothrow_constructible_v<First>) = default;
  constexpr nothrow_default_constructable_first_(const nothrow_default_constructable_first_&) = default;
  constexpr nothrow_default_constructable_first_(nothrow_default_constructable_first_&&) = default;
  nothrow_default_constructable_first_& operator=(const nothrow_default_constructable_first_&) = default;
  nothrow_default_constructable_first_& operator=(nothrow_default_constructable_first_&&) = default;
};

template <class... Types>
struct nothrow_move_construct_base_<true, Types...> {
  nothrow_move_construct_base_() noexcept(
      std::conjunction_v<std::is_nothrow_move_constructible<Types>...>) = default;
  constexpr nothrow_move_construct_base_(const nothrow_move_construct_base_&) = default;
  constexpr nothrow_move_construct_base_(nothrow_move_construct_base_&&) = default;
  nothrow_move_construct_base_& operator=(const nothrow_move_construct_base_&) = default;
  nothrow_move_construct_base_& operator=(nothrow_move_construct_base_&&) = default;
};

//template <class... Types>
//struct nothrow_move_assign_base_<true, Types...> {
//  nothrow_move_assign_base_() = default;
//  constexpr nothrow_move_assign_base_(const nothrow_move_assign_base_&) = default;
//  constexpr nothrow_move_assign_base_(nothrow_move_assign_base_&&) noexcept(
//      std::conjunction_v<std::is_nothrow_move_assignable<Types>...>&&
//          std::conjunction_v<std::is_nothrow_move_constructible<Types>...>) = default;
//  nothrow_move_assign_base_& operator=(const nothrow_move_assign_base_&) = default;
//  nothrow_move_assign_base_& operator=(nothrow_move_assign_base_&&) noexcept(
//      std::conjunction_v<std::is_nothrow_move_assignable<Types>...>&&
//          std::conjunction_v<std::is_nothrow_move_constructible<Types>...>) = default;
//};

///==================================================================================================================///

template <typename... Types>
using copy_construct_base = copy_construct_base_<std::conjunction_v<std::is_copy_constructible<Types>...>, Types...>;

template <typename... Types>
using move_construct_base = move_construct_base_<std::conjunction_v<std::is_move_constructible<Types>...>, Types...>;

template <typename... Types>
using copy_assign_base = copy_assign_base_<std::conjunction_v<std::is_copy_assignable<Types>...> &&
                                               std::conjunction_v<std::is_copy_constructible<Types>...>,
                                           Types...>;

template <typename... Types>
using move_assign_base = move_assign_base_<std::conjunction_v<std::is_move_assignable<Types>...> &&
                                               std::conjunction_v<std::is_move_constructible<Types>...>,
                                           Types...>;

template <typename First>
using default_constructable_first = default_constructable_first_<First, std::is_default_constructible_v<First>>;

template <typename First>
using nothrow_default_constructable_first =
    nothrow_default_constructable_first_<First, std::is_nothrow_default_constructible_v<First>>;

template <typename... Types>
using nothrow_move_construct_base =
    nothrow_move_construct_base_<std::conjunction_v<std::is_nothrow_move_constructible<Types>...>, Types...>;

//template <typename... Types>
//using nothrow_move_assign_base =
//    nothrow_move_assign_base_<std::conjunction_v<std::is_nothrow_move_assignable<Types>...> &&
//                                  std::conjunction_v<std::is_nothrow_move_constructible<Types>...>,
//                              Types...>;

///==================================================================================================================///
